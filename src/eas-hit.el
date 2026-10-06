;;; eas-hit.el --- hit-test indexes and pointer -> datum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Compile attaches an :index to every data mark:
;;
;;   x-sorted  series and rules: :xs ascending, :refs [[ITEM POINT] ...]
;;   grid      points and text: :size S, :cells [(:cell [CX CY] :items [..])]
;;   rects     bars and rects: items are tested directly
;;
;; `eas-hit' maps a pixel to the nearest datum through them.  Per the
;; fc-qx1.14 spike a bisect costs ~7 µs, so hover never needs SVG
;; hot spots for continuous series.

;;; Code:

(require 'eas-core)

(defconst eas-hit-grid-size 20 "Pixel size of a point-index grid cell.")

(defun eas-hit--anchors (item)
  "Series ITEM's per-datum vertices: :anchors when stepped, else :points."
  (or (plist-get item :anchors) (plist-get item :points)))

(defun eas-hit--item-point (item k)
  "Point K of series ITEM as (X . Y), or ITEM's anchor when K is nil."
  (cond
   ((and k (eas-hit--anchors item))
    (let ((p (aref (eas-hit--anchors item) k))) (cons (aref p 0) (aref p 1))))
   ((plist-member item :x1) (cons (/ (+ (plist-get item :x1) (plist-get item :x2)) 2.0)
                                  (/ (+ (plist-get item :y1) (plist-get item :y2)) 2.0)))
   ((plist-member item :w) (cons (+ (plist-get item :x) (/ (plist-get item :w) 2.0))
                                 (+ (plist-get item :y) (/ (plist-get item :h) 2.0))))
   (t (cons (plist-get item :x) (plist-get item :y)))))

(defun eas-hit-index (mark)
  "Return the hit-test index for MARK's items."
  (let ((items (plist-get mark :items)))
    (pcase (plist-get mark :mark)
      ((or "bar" "rect" "brush" "image") (list :kind "rects"))
      ("geoshape" (list :kind "shapes"))
      ((or "line" "area" "trail" "rule" "tick")
       (let (entries)
         (seq-do-indexed
          (lambda (item i)
            (if-let* ((pts (eas-hit--anchors item)))
                (dotimes (k (length pts))
                  (push (list (aref (aref pts k) 0) i k) entries))
              (push (list (car (eas-hit--item-point item nil)) i :null) entries)))
          items)
         (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
         (list :kind "x-sorted" :xs (vconcat (mapcar #'car entries))
               :refs (vconcat (mapcar (lambda (e) (vector (nth 1 e) (nth 2 e))) entries)))))
      (_ (let ((cells (make-hash-table :test 'equal)) order)
           (seq-do-indexed
            (lambda (item i)
              (let* ((p (eas-hit--item-point item nil))
                     (key (vector (floor (car p) eas-hit-grid-size) (floor (cdr p) eas-hit-grid-size))))
                (unless (gethash key cells) (push key order))
                (puthash key (cons i (gethash key cells)) cells)))
            items)
           (list :kind "grid" :size eas-hit-grid-size
                 :cells (vconcat (mapcar (lambda (k) (list :cell k :items (vconcat (nreverse (gethash k cells)))))
                                         (sort order (lambda (a b) (or (< (aref a 0) (aref b 0))
                                                                       (and (= (aref a 0) (aref b 0))
                                                                            (< (aref a 1) (aref b 1))))))))))))))

(defun eas-hit--bisect (xs x)
  "Index of the element of ascending vector XS nearest X."
  (let ((lo 0) (hi (1- (length xs))))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (aref xs mid) x) (setq lo (1+ mid)) (setq hi mid))))
    (if (and (> lo 0) (< (- x (aref xs (1- lo))) (- (aref xs lo) x))) (1- lo) lo)))

(defun eas-hit--datum (item k)
  "Datum index of ITEM (anchor K for series)."
  (let ((d (plist-get item :datum)))
    (if (vectorp d) (aref d k) d)))

(defun eas-hit--distance (item p px py x-only)
  "Distance from PX PY to ITEM, whose point is P; X-ONLY measures |dx|."
  (if (plist-member item :w)
      (let ((ex (max 0 (- (plist-get item :x) px) (- px (+ (plist-get item :x) (plist-get item :w)))))
            (ey (max 0 (- (plist-get item :y) py) (- py (+ (plist-get item :y) (plist-get item :h))))))
        (if x-only ex (sqrt (+ (* ex ex) (* ey ey)))))
    (let ((dx (- px (car p))) (dy (- py (cdr p))))
      (if x-only (abs dx) (sqrt (+ (* dx dx) (* dy dy)))))))

(defun eas-hit--candidate (mark item-index k px py x-only)
  "Return the hit candidate for MARK's item ITEM-INDEX (point K) at PX PY.
X-ONLY measures the horizontal distance alone."
  (let* ((item (aref (plist-get mark :items) item-index))
         (p (eas-hit--item-point item (if (eq k :null) nil k))))
    (list :mark (plist-get mark :id) :item item-index :datum (eas-hit--datum item (if (eq k :null) 0 k))
          :distance (eas-hit--distance item p px py x-only) :x (car p) :y (cdr p))))

;;; Grid search (fc-qx1.9: a linear scan was 13 ms a query at 10k points)

(defvar eas-hit--grids (make-hash-table :test 'eq :weakness 'key)
  "Grid :cells vector -> (COLUMNS . BOUNDS): column x -> ((CY . ITEMS) ...).")

(defun eas-hit--grid-table (index)
  "Column table and cell bounds [CX0 CX1 CY0 CY1] of grid INDEX."
  (let ((cells (plist-get index :cells)))
    (or (gethash cells eas-hit--grids)
        (let ((columns (make-hash-table :test 'eql)) (bounds nil))
          (seq-doseq (c cells)
            (let ((cx (aref (plist-get c :cell) 0)) (cy (aref (plist-get c :cell) 1)))
              (push (cons cy (plist-get c :items)) (gethash cx columns))
              (setq bounds (if bounds (vector (min cx (aref bounds 0)) (max cx (aref bounds 1))
                                              (min cy (aref bounds 2)) (max cy (aref bounds 3)))
                             (vector cx cx cy cy)))))
          (puthash cells (cons columns bounds) eas-hit--grids)))))

(defun eas-hit--grid (mark px py x-only)
  "Return the nearest item of grid-indexed MARK to PX PY.
The same item `eas-hit--scan' finds.  Search square rings of cells
outwards (columns only with X-ONLY) and stop once no unvisited cell can
hold anything nearer."
  (let* ((index (plist-get mark :index)) (size (plist-get index :size))
         (table (eas-hit--grid-table index)) (columns (car table)) (b (cdr table))
         (items (plist-get mark :items))
         (cx0 (floor px size)) (cy0 (floor py size))
         (reach (max (abs (- cx0 (aref b 0))) (abs (- cx0 (aref b 1)))
                     (if x-only 0 (max (abs (- cy0 (aref b 2))) (abs (- cy0 (aref b 3)))))))
         (best nil) (best-d nil) (r 0))
    (cl-flet ((visit (cell-items)
                (seq-doseq (i cell-items)
                  (let* ((item (aref items i))
                         (d (eas-hit--distance item (eas-hit--item-point item nil) px py x-only)))
                    (when (or (null best-d) (< d best-d) (and (= d best-d) (< i best)))
                      (setq best i best-d d))))))
      (while (and (<= r reach) (not (and best-d (<= best-d (* (max 0 (1- r)) size)))))
        (dolist (cx (if (= r 0) (list cx0) (list (- cx0 r) (+ cx0 r))))
          (dolist (cell (gethash cx columns))
            (when (or x-only (<= (abs (- (car cell) cy0)) r)) (visit (cdr cell)))))
        (unless x-only
          (dolist (cy (if (= r 0) nil (list (- cy0 r) (+ cy0 r))))
            (cl-loop for cx from (- cx0 (1- r)) to (+ cx0 (1- r))
                     do (when-let* ((cell (assq cy (gethash cx columns)))) (visit (cdr cell))))))
        (setq r (1+ r))))
    (when best (eas-hit--candidate mark best :null px py x-only))))

(defun eas-hit--scan (mark px py x-only)
  "Return the nearest of MARK's items to PX PY by testing each.
The first wins on ties; X-ONLY measures the horizontal distance alone."
  (let ((items (plist-get mark :items)) best best-d)
    (dotimes (i (length items))
      (let* ((item (aref items i))
             (d (eas-hit--distance item (eas-hit--item-point item nil) px py x-only)))
        (when (or (null best-d) (< d best-d)) (setq best i best-d d))))
    (eas-hit--candidate mark best :null px py x-only)))

(declare-function eas-geoshape-hit "eas-geoshape-render")

(defun eas-hit-mark (mark px py &optional x-only)
  "Nearest hit candidate in MARK for PX PY; X-ONLY measures |dx| only."
  (let ((index (plist-get mark :index)) (items (plist-get mark :items)))
    (when (> (length items) 0)
      (pcase (plist-get index :kind)
        ("x-sorted"
         (let* ((xs (plist-get index :xs)) (refs (plist-get index :refs))
                (i (eas-hit--bisect xs px)) best)
           ;; Points sharing the nearest x (several series) compete on y.
           (let ((x (aref xs i)) (j i))
             (while (and (> j 0) (= (aref xs (1- j)) x)) (setq j (1- j)))
             (while (and (< j (length xs)) (= (aref xs j) x))
               (let ((c (eas-hit--candidate mark (aref (aref refs j) 0) (aref (aref refs j) 1) px py nil)))
                 (when (or (null best) (< (plist-get c :distance) (plist-get best :distance))) (setq best c)))
               (setq j (1+ j))))
           (if x-only (plist-put best :distance (abs (- px (plist-get best :x)))) best)))
        ("shapes" (eas-geoshape-hit mark px py))
        ;; Box distance can undercut a cell's bound: only point items use the grid.
        ("grid" (if (plist-member (aref items 0) :w) (eas-hit--scan mark px py x-only)
                  (eas-hit--grid mark px py x-only)))
        (_ (eas-hit--scan mark px py x-only))))))

(defun eas-hit--contains (bounds px py)
  "Non-nil when PX PY lies inside BOUNDS [x y w h]."
  (and (<= (aref bounds 0) px (+ (aref bounds 0) (aref bounds 2)))
       (<= (aref bounds 1) py (+ (aref bounds 1) (aref bounds 3)))))

(defun eas-hit-view-at (scene px py)
  "Return the view of SCENE whose plot has PX PY inside, else the nearest."
  (let ((views (append (plist-get scene :views) nil)))
    (or (seq-find (lambda (v) (eas-hit--contains (plist-get v :bounds) px py)) views)
        (car (sort (copy-sequence views)
                   (lambda (a b)
                     (cl-flet ((d (v) (let* ((b (plist-get v :bounds))
                                             (cx (+ (aref b 0) (/ (aref b 2) 2.0)))
                                             (cy (+ (aref b 1) (/ (aref b 3) 2.0))))
                                        (+ (abs (- px cx)) (abs (- py cy))))))
                       (< (d a) (d b)))))))))

(defun eas-hit (scene view px &optional x-only)
  "Return the datum nearest pixel PX ([X Y]) in SCENE's VIEW.
VIEW is a view id, a view plist, or nil for the view under PX.  The
result is (:view ID :mark ID :item I :datum D :distance PX :x :y
:row ROW), D indexing the mark's :rows, or nil when there is no data.
With X-ONLY, distance is horizontal (crosshair semantics)."
  (let* ((px0 (aref px 0)) (py0 (aref px 1))
         (view (cond ((stringp view) (seq-find (lambda (v) (equal (plist-get v :id) view))
                                               (plist-get scene :views)))
                     (view view)
                     (t (eas-hit-view-at scene px0 py0))))
         best)
    (when view
      (seq-doseq (mark (plist-get view :marks))
        (unless (plist-get mark :interactive-off)
          (when-let* ((c (eas-hit-mark mark px0 py0 x-only)))
            (when (or (null best) (< (plist-get c :distance) (plist-get best :distance)))
              (setq best (append c (list :row (aref (plist-get mark :rows) (plist-get c :datum)))))))))
      (when best (append (list :view (plist-get view :id)) best)))))

(provide 'eas-hit)
;;; eas-hit.el ends here
