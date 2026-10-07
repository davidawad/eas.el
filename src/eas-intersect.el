;;; eas-intersect.el --- does the pointer touch a drawn mark? -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.34).  `eas-hit' answers "which datum is nearest";
;; hover answers "which mark is the pointer on".  A mark interacts
;; (tooltip, header readout, a non-nearest pointermove selection) only
;; when the pointer intersects what is drawn:
;;
;;   line, trail     within half the stroke of the path's segments
;;   area            inside the band between the path and its base
;;   bar, rect       inside the box
;;   rule, tick      within half the stroke of the segment
;;   point           within the symbol's radius; text, half its font size
;;
;; plus half a character cell's diagonal (at least `eas-intersect-slop'):
;; a terminal pointer is the centre of a cell the glyph fills, and the
;; GUI uses the same reach so a replayed log hovers alike (fc-qx1.8).  Fully
;; transparent items (hit targets such as the crosshair's rules) never
;; touch.  The values a chart shows without touching live in eas-strip.
;; Pure: scene in, hit out, shaped like `eas-hit''s.

;;; Code:

(require 'eas-core)
(require 'eas-hit)

(defconst eas-intersect-slop 2 "Pixels a pointer may miss a drawn mark by and still touch it.")

(defun eas-intersect--segment (px py x1 y1 x2 y2)
  "Distance from PX PY to the segment X1 Y1 -- X2 Y2."
  (let* ((dx (- x2 x1)) (dy (- y2 y1)) (len2 (+ (* dx dx) (* dy dy)))
         (u (if (zerop len2) 0 (max 0 (min 1 (/ (+ (* (- px x1) dx) (* (- py y1) dy)) (float len2))))))
         (ex (- px (+ x1 (* u dx)))) (ey (- py (+ y1 (* u dy)))))
    (sqrt (+ (* ex ex) (* ey ey)))))

(defun eas-intersect--first-at (points x)
  "Index of the first of POINTS ([X Y], ascending in x) at or past X."
  (let ((lo 0) (hi (length points)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (aref (aref points mid) 0) x) (setq lo (1+ mid)) (setq hi mid))))
    lo))

(defun eas-intersect-nearest-x (points x)
  "Index of the one of POINTS ([X Y], ascending in x) nearest X in x, or nil."
  (let ((n (length points)))
    (when (> n 0)
      (let ((i (min (eas-intersect--first-at points x) (1- n))))
        (if (and (> i 0) (< (- x (aref (aref points (1- i)) 0)) (- (aref (aref points i) 0) x))) (1- i) i)))))

(defun eas-intersect--path (points px py reach)
  "Distance from PX PY to the polyline POINTS, searching REACH pixels around PX."
  (let ((n (length points)) best)
    (if (= n 1)
        (let ((p (aref points 0))) (eas-intersect--segment px py (aref p 0) (aref p 1) (aref p 0) (aref p 1)))
      (let ((i (max 0 (1- (eas-intersect--first-at points (- px reach))))))
        (while (and (< (1+ i) n) (<= (aref (aref points i) 0) (+ px reach)))
          (let* ((a (aref points i)) (b (aref points (1+ i)))
                 (d (eas-intersect--segment px py (aref a 0) (aref a 1) (aref b 0) (aref b 1))))
            (when (or (null best) (< d best)) (setq best d)))
          (setq i (1+ i))))
      best)))

(defun eas-intersect--y-at (points x)
  "The y of polyline POINTS at X, or nil outside its x span."
  (let ((n (length points)))
    (when (and (> n 0) (<= (aref (aref points 0) 0) x (aref (aref points (1- n)) 0)))
      (let* ((i (min (eas-intersect--first-at points x) (1- n)))
             (b (aref points i)) (a (aref points (max 0 (1- i))))
             (w (- (aref b 0) (aref a 0))))
        (if (zerop w) (aref b 1)
          (+ (aref a 1) (* (/ (- x (aref a 0)) (float w)) (- (aref b 1) (aref a 1)))))))))

(defun eas-intersect--stroke (item default)
  "Half of ITEM's stroke width (DEFAULT when it has none)."
  (/ (or (plist-get item :strokeWidth) default) 2.0))

(defun eas-intersect--area (item px py reach)
  "Distance from PX PY to area ITEM: 0 inside its band, else to its edge."
  (let ((top (eas-intersect--y-at (plist-get item :points) px))
        (base (and (plist-get item :base) (eas-intersect--y-at (plist-get item :base) px))))
    (if (and top base (<= (min top base) py (max top base))) 0
      (eas-intersect--path (plist-get item :points) px py reach))))

(defun eas-intersect--item (kind item px py reach)
  "(DISTANCE . TOLERANCE) of PX PY from ITEM of mark KIND, or nil.
REACH bounds how far along x a series is searched."
  (pcase kind
    ((or "line" "trail")
     (when-let* ((d (eas-intersect--path (plist-get item :points) px py reach)))
       (cons d (eas-intersect--stroke item 2))))
    ("area" (when-let* ((d (eas-intersect--area item px py reach))) (cons d (eas-intersect--stroke item 0))))
    ((or "bar" "rect") (cons (eas-hit--distance item nil px py nil) 0))
    ((or "rule" "tick")
     (cons (eas-intersect--segment px py (plist-get item :x1) (plist-get item :y1)
                                   (plist-get item :x2) (plist-get item :y2))
           (eas-intersect--stroke item 1)))
    ("text" (cons (eas-hit--distance item (eas-hit--item-point item nil) px py nil)
                  (/ (or (plist-get item :fontSize) 11) 2.0)))
    (_ (cons (eas-hit--distance item (eas-hit--item-point item nil) px py nil)
             (sqrt (/ (or (plist-get item :size) 30) float-pi))))))

(defun eas-intersect--slop (scene)
  "Pixels beyond a mark's own extent that still touch it in SCENE.
Half a character cell's diagonal on both backends: a terminal pointer is
a cell's centre, and one event log must hover the same datum on both."
  (let ((cell (plist-get (plist-get scene :size) :cell)))
    (if (vectorp cell)
        (max eas-intersect-slop (/ (sqrt (+ (expt (aref cell 0) 2) (expt (aref cell 1) 2))) 2.0))
      eas-intersect-slop)))

(defun eas-intersect--hidden-p (item)
  "Non-nil when ITEM is drawn fully transparent (a hit target, not a mark)."
  (let ((o (plist-get item :opacity))) (and (numberp o) (zerop o))))

(defconst eas-intersect--big-radius 16
  "Symbols wider than this radius are tested whatever the nearest centre.")

(defvar eas-intersect--big (make-hash-table :test 'eq :weakness 'key)
  "Mark -> indexes of its symbols over `eas-intersect--big-radius'.")

(defun eas-intersect--big-items (mark)
  "Indexes of MARK's large symbols (nested circles, bubbles), cached."
  (with-memoization (gethash mark eas-intersect--big)
    (let ((items (plist-get mark :items)))
      (cl-loop for i below (length items)
               when (> (sqrt (/ (or (plist-get (aref items i) :size) 30) float-pi)) eas-intersect--big-radius)
               collect i))))

(defvar eas-intersect--anchors (make-hash-table :test 'eq :weakness 'key)
  "Shapes index -> [XS ORDER REACH] or `none' (`eas-intersect--shape-window').")

(defun eas-intersect--anchors (mark)
  "MARK's shape anchors sorted by x, as [XS ORDER REACH], or nil.
XS ascend, ORDER holds the item index of each, REACH is the symbol
radius.  The table hangs off MARK's index, which a hover patch
keeps: patched shapes are restyled, never moved.  Nil when an item is
measured otherwise than from its anchor."
  (let ((index (plist-get mark :index)))
    (when index
      (let ((v (with-memoization (gethash index eas-intersect--anchors)
                 (let ((items (plist-get mark :items)) (pairs nil))
                   (if (cl-loop for i below (length items)
                                for item = (aref items i)
                                always (and (numberp (plist-get item :x)) (numberp (plist-get item :y))
                                            (not (plist-member item :w)) (not (plist-member item :x1))
                                            (not (plist-member item :size)))
                                do (push (cons (plist-get item :x) i) pairs))
                       (let ((sorted (sort (nreverse pairs) (lambda (a b) (< (car a) (car b))))))
                         ;; Every symbol has the default size (`eas-intersect--item').
                         (vector (vconcat (mapcar #'car sorted)) (vconcat (mapcar #'cdr sorted))
                                 (sqrt (/ 30 float-pi))))
                     'none)))))
        (and (vectorp v) v)))))

(defun eas-intersect--shape-window (mark px reach)
  "Item indexes of geoshape MARK whose anchor is within REACH of PX in x.
Ascending, as a full scan visits them.  MARK must have an anchors
table (`eas-intersect--anchors')."
  (let ((a (eas-intersect--anchors mark)))
    (let* ((xs (aref a 0)) (order (aref a 1)) (n (length xs))
           ;; A touch is within the symbol radius plus REACH of the anchor,
           ;; so |dx| is too (a micropixel more for rounding).
           (r (+ (aref a 2) reach 1e-6)) (lo 0) (hi n) out)
      (while (< lo hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (< (aref xs mid) (- px r)) (setq lo (1+ mid)) (setq hi mid))))
      (while (and (< lo n) (<= (aref xs lo) (+ px r)))
        (push (aref order lo) out)
        (setq lo (1+ lo)))
      (sort out #'<))))

(defun eas-intersect--candidates (mark px py &optional slop)
  "Item indexes of MARK worth testing against PX PY.
SLOP is the reach beyond a mark's extent (`eas-intersect--slop')."
  (let ((items (plist-get mark :items)))
    (cond
     ((and (member (plist-get mark :mark) '("point" "circle" "square" "text"))
           (> (length items) 64))
      ;; The grid index finds the nearest centre without a scan (fc-qx1.9);
      ;; a large symbol can hold PX PY far from its centre (circle packing).
      (let ((near (when-let* ((c (eas-hit-mark mark px py))) (list (plist-get c :item)))))
        (if (equal (plist-get mark :mark) "text") near
          (delete-dups (append near (eas-intersect--big-items mark))))))
     ;; A map's shapes touch near their anchors (eas-b2s.8): scan only
     ;; the ones within reach in x.
     ((and slop (equal (plist-get mark :mark) "geoshape") (> (length items) 64)
           (eas-intersect--anchors mark))
      (eas-intersect--shape-window mark px slop))
     (t (number-sequence 0 (1- (length items)))))))

(defun eas-intersect-mark (scene mark px py)
  "Hit (as `eas-hit-mark') of PX PY on MARK of SCENE when it touches, else nil."
  (let* ((slop (eas-intersect--slop scene)) (kind (plist-get mark :mark))
         (items (plist-get mark :items)) (reach (+ slop 8)) best)
    (dolist (i (eas-intersect--candidates mark px py slop))
      (let ((item (aref items i)))
        (unless (eas-intersect--hidden-p item)
          (when-let* ((dt (eas-intersect--item kind item px py reach))
                      ((<= (car dt) (+ (cdr dt) slop)))
                      ((or (null best) (< (car dt) (plist-get best :distance)))))
            (let* ((anchors (eas-hit--anchors item))
                   (k (if anchors (eas-intersect-nearest-x anchors px) :null)))
              (setq best (plist-put (eas-hit--candidate mark i k px py nil) :distance (car dt))))))))
    best))

(defun eas-intersect (scene view px)
  "The datum of the mark PX ([X Y]) touches in SCENE's VIEW (a plist), or nil.
The result is shaped like `eas-hit''s; :distance is to the drawn mark.
The topmost (last drawn) of equally near marks wins."
  (let ((x (aref px 0)) (y (aref px 1)) best)
    (seq-doseq (mark (plist-get view :marks))
      (unless (or (plist-get mark :interactive-off) (= (length (plist-get mark :items)) 0))
        (when-let* ((c (eas-intersect-mark scene mark x y)))
          (when (or (null best) (<= (plist-get c :distance) (plist-get best :distance)))
            (setq best (append c (list :row (aref (plist-get mark :rows) (plist-get c :datum)))))))))
    (when best (append (list :view (plist-get view :id)) best))))

(provide 'eas-intersect)
;;; eas-intersect.el ends here
