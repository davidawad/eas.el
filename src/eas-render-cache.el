;;; eas-render-cache.el --- reuse unchanged work between renders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5 helper (eas-7r1.19).  An animated view redraws a scene that
;; differs from the last one in a few marks: a clock's hands, the
;; routes of the hovered airport.  The renderers ask this file for the
;; work an earlier render already did, and the output stays the same,
;; byte for byte and property for property, as an uncached render's.
;;
;; SVG: each mark's printed fragment (and the gradient definitions it
;; records) is kept under the mark plist itself, compared with `equal'
;; (cheap when the compiler reuses the mark's :items vector, as it does
;; for unchanged layers).  Two generations of bounded size keep the
;; working set and drop the rest.
;;
;; Text: the text renderer paints a list of steps (axes, each mark,
;; legends, titles) onto one cell grid.  `eas-render-cache-paint'
;; restarts such a paint from a grid snapshot taken before the first
;; step whose KEY changed.  The text renderer now keeps its own last
;; frame per canvas (`eas-render-cache--frames', eas-b2s.7): the grid,
;; the rows each step and item tried, and a snapshot; a frame whose
;; marks changed repaints only the rows of the changed items
;; (`eas-text--frame').  `eas-render-cache-dirty' carries the update
;; path's dirty marks to both renderers as a hint.
;;
;; Scenes are values: a render never mutates one, so a scene object
;; that is `equal' to a cached key draws the same.  Set
;; `eas-render-cache-enabled' to nil to render every frame from scratch.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'svg)

(defcustom eas-render-cache-enabled t
  "Non-nil to let renderers reuse the unchanged parts of the last render."
  :type 'boolean :group 'eas-chart)

(defvar eas-render-cache-svg-max-bytes (* 8 1024 1024)
  "Bytes of SVG fragments one cache generation holds before it ages.")

(defvar eas-render-cache-text-entries 4
  "Text canvases whose last render is remembered.")

(defvar eas-render-cache-text-snapshots 4
  "Grid snapshots kept per remembered text canvas.")

(defvar eas-render-cache-stats nil
  "Counters, a plist: :svg-hits, :svg-misses, :text-hits, :text-skipped.
:text-skipped counts paint steps not run, :row-hits composed rows
reused and :restored snapshots written over an old state in place.
`eas-render-cache-clear' resets it.")

(defvar eas-render-cache--svg-new nil "SVG fragments of this generation.")
(defvar eas-render-cache--svg-old nil "SVG fragments of the last generation.")
(defvar eas-render-cache--svg-bytes 0 "Bytes held by `eas-render-cache--svg-new'.")
(defvar eas-render-cache--text nil "Remembered text renders, the latest first.")

(defvar eas-render-cache--frames nil
  "The last text frame per canvas, an alist: ENV -> [GRID STEPS ROWS LABELS ...].
See `eas-text--frame' (eas-b2s.7).")

(defvar eas-render-cache-dirty nil
  "What the scene being rendered changed since its view's last frame.
Nil (no hint), t or ((VIEW-ID MARK-ID ...) ...) as `eas-scene-dirty'
returns it, bound by the glue around a redraw.  A renderer treats a
listed mark as changed without comparing it; any other mark is still
compared (by `eq' first), so a wrong hint costs time, never output.")

(defun eas-render-cache-dirty-p (view-id mark)
  "Non-nil when `eas-render-cache-dirty' lists MARK of VIEW-ID as changed."
  (and (consp eas-render-cache-dirty) (consp mark)
       (member (plist-get mark :id) (cdr (assoc view-id eas-render-cache-dirty)))))

(defun eas-render-cache-clear ()
  "Forget every cached render and reset `eas-render-cache-stats'."
  (interactive)
  (setq eas-render-cache--svg-new (make-hash-table :test 'equal)
        eas-render-cache--svg-old (make-hash-table :test 'equal)
        eas-render-cache--svg-bytes 0
        eas-render-cache--text nil
        eas-render-cache--frames nil
        eas-render-cache-stats (list :svg-hits 0 :svg-misses 0 :text-hits 0
                                     :text-skipped 0 :row-hits 0)))

(eas-render-cache-clear)

(defun eas-render-cache--count (key &optional n)
  "Add N (default 1) to counter KEY of `eas-render-cache-stats'."
  (setq eas-render-cache-stats
        (plist-put eas-render-cache-stats key (+ (or n 1) (or (plist-get eas-render-cache-stats key) 0)))))

;;; SVG fragments

(defvar eas-paint--svg-defs)

(defun eas-render-cache--merge-defs (defs)
  "Append each of DEFS whose id is new to `eas-paint--svg-defs'.
This is the order and dedup `eas-paint' records gradients in."
  (dolist (d defs)
    (unless (seq-some (lambda (o) (equal (dom-attr o 'id) (dom-attr d 'id))) eas-paint--svg-defs)
      (setq eas-paint--svg-defs (append eas-paint--svg-defs (list d))))))

(defun eas-render-cache-svg-fragment (key nodes-fn)
  "The SVG string NODES-FN's nodes print to, reused under KEY.
NODES-FN takes no arguments and returns a list of DOM nodes; KEY holds
every input they depend on.  The gradient definitions NODES-FN records
in `eas-paint--svg-defs' are recorded again on a hit."
  (let ((hit (or (gethash key eas-render-cache--svg-new)
                 (when-let* ((old (gethash key eas-render-cache--svg-old)))
                   (cl-incf eas-render-cache--svg-bytes (length (car old)))
                   (puthash key old eas-render-cache--svg-new)))))
    (if hit (eas-render-cache--count :svg-hits)
      (eas-render-cache--count :svg-misses)
      (let* ((defs nil)
             (string (let ((eas-paint--svg-defs nil))
                       (prog1 (with-temp-buffer
                                (mapc #'svg-print (funcall nodes-fn))
                                (buffer-string))
                         (setq defs eas-paint--svg-defs)))))
        (setq hit (cons string defs))
        (when (> (+ eas-render-cache--svg-bytes (length string)) eas-render-cache-svg-max-bytes)
          (setq eas-render-cache--svg-old eas-render-cache--svg-new
                eas-render-cache--svg-new (make-hash-table :test 'equal)
                eas-render-cache--svg-bytes 0))
        (cl-incf eas-render-cache--svg-bytes (length string))
        (puthash key hit eas-render-cache--svg-new)))
    (eas-render-cache--merge-defs (cdr hit))
    (car hit)))

;;; Text grids

(defun eas-render-cache--copy (state)
  "A copy of paint STATE that painting the original cannot change.
Conses, records and vectors are copied, the vectors and hash tables a
record or cons holds too; their elements are shared."
  (cond ((consp state) (cons (eas-render-cache--copy (car state)) (eas-render-cache--copy (cdr state))))
        ((recordp state)
         (let ((r (copy-sequence state)))
           (dotimes (i (length r))
             (when (> i 0) (aset r i (eas-render-cache--copy (aref r i)))))
           r))
        ((vectorp state) (copy-sequence state))
        ((hash-table-p state) (copy-hash-table state))
        (t state)))

(defun eas-render-cache--restore (dst src)
  "Make DST what `eas-render-cache--copy' of SRC would be; return it.
Where DST has SRC's shape (conses, records, vectors of SRC's length,
hash tables) it is overwritten in place, allocating nothing; elsewhere
a copy is made.  DST must share no structure with SRC."
  (cond ((and (consp src) (consp dst))
         (setcar dst (eas-render-cache--restore (car dst) (car src)))
         (setcdr dst (eas-render-cache--restore (cdr dst) (cdr src)))
         dst)
        ((and (recordp src) (recordp dst) (= (length src) (length dst)) (eq (aref src 0) (aref dst 0)))
         (let ((i 1) (n (length src)))
           (while (< i n)
             (aset dst i (eas-render-cache--restore (aref dst i) (aref src i)))
             (setq i (1+ i))))
         dst)
        ((and (vectorp src) (vectorp dst) (= (length src) (length dst)) (not (eq src dst)))
         (let ((i 0) (n (length src)))
           (while (< i n) (aset dst i (aref src i)) (setq i (1+ i))))
         dst)
        ((and (hash-table-p src) (hash-table-p dst) (not (eq src dst))
              (eq (hash-table-test src) (hash-table-test dst)) (eq (hash-table-weakness src) (hash-table-weakness dst)))
         (clrhash dst)
         (maphash (lambda (k v) (puthash k v dst)) src)
         dst)
        (t (eas-render-cache--copy src))))

(defun eas-render-cache--reuse (spare snapshot)
  "A state equal to a copy of SNAPSHOT, written over SPARE when non-nil.
SPARE is the state a render returned two renders ago: nobody draws
from it any more.
Restoring in place spares a copy of the whole grid per frame
\(eas-b2s.1: about 290 KB of allocation on a 100x40 canvas)."
  (if (null spare) (eas-render-cache--copy snapshot)
    (eas-render-cache--count :restored)
    (eas-render-cache--restore spare snapshot)))

(defun eas-render-cache--prefix (keys old)
  "How many of vector KEYS lead vector OLD, `equal' one for one."
  (let ((n (min (length keys) (length old))) (k 0))
    (while (and (< k n) (equal (aref keys k) (aref old k))) (cl-incf k))
    k))

(defun eas-render-cache-paint (env state steps run)
  "Paint STATE, reusing the last render with ENV; return it.
STEPS, the paint, is a list of (KEY . FN): RUN is called as (RUN FN STATE) and
paints in place, depending only on STATE and on what KEY holds.  ENV
holds what every step depends on besides (canvas size, ink).  When ENV
is nil nothing is cached.  The returned state may be a copy of STATE.
STATE may be a function of no arguments returning it, called only when
no snapshot is reused."
  (if (or (null env) (not eas-render-cache-enabled))
      (let ((state (if (functionp state) (funcall state) state)))
        (dolist (s steps) (funcall run (cdr s) state))
        state)
    (let* ((keys (vconcat (mapcar #'car steps))) (fns (vconcat (mapcar #'cdr steps)))
           (n (length keys)) (entry nil) (k 0))
      ;; The remembered render of ENV sharing the longest prefix.
      (dolist (e eas-render-cache--text)
        (when (equal (car e) env)
          (let ((p (eas-render-cache--prefix keys (nth 1 e))))
            (when (or (null entry) (> p k)) (setq entry e k p)))))
      (let* ((old-keys (nth 1 entry))
             ;; Snapshots before step I stay good while steps below I match.
             (snaps (seq-filter (lambda (s) (<= (car s) k)) (nth 2 entry)))
             (start (car (seq-reduce (lambda (best s) (if (> (car s) (car best)) s best)) snaps '(0))))
             (taken 0))
        (if (= start 0)
            (when (functionp state) (setq state (funcall state)))
          (setq state (eas-render-cache--reuse (let ((spare (nth 1 (nth 3 entry))))
                                                 (and (not (eq spare (car (nth 3 entry)))) spare))
                                               (cdr (assq start snaps))))
          (eas-render-cache--count :text-hits)
          (eas-render-cache--count :text-skipped start))
        ;; Keep the latest snapshots of the prefix.
        (setq snaps (seq-take (sort snaps (lambda (a b) (> (car a) (car b))))
                              (max 1 (- eas-render-cache-text-snapshots 2))))
        ;; Snapshot where an unchanged run of steps meets a changed step.
        (cl-loop with same = nil
                 for i from start to n
                 for now = (or (< i k)
                               (and (< i (length old-keys)) (< i n) (equal (aref keys i) (aref old-keys i))))
                 do (when (and entry (> i 0) (not (assq i snaps))
                               (< taken eas-render-cache-text-snapshots)
                               (or (= i k) (and (> i k) (< i n) same (not now))))
                      (push (cons i (eas-render-cache--copy state)) snaps)
                      (cl-incf taken))
                 do (when (< i n) (funcall run (aref fns i) state))
                 do (setq same now))
        (setq eas-render-cache--text
              (cons (list env keys snaps (list state (car (nth 3 entry))))
                    (seq-take (delq entry eas-render-cache--text)
                              (max 0 (1- eas-render-cache-text-entries)))))
        state))))

(provide 'eas-render-cache)
;;; eas-render-cache.el ends here
