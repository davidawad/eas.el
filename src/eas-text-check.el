;;; eas-text-check.el --- structural invariants of a text rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5 verification (fc-qx1.49).  The SVG gallery is judged
;; against reference images; a text rendering has no oracle, so it is
;; judged against its own scene.  `eas-text-check' renders a text-target
;; scene with `eas-text-trace' on and returns what is wrong with it:
;;
;;   empty       the rendering is blank.
;;   item        a visible mark item (inside its plot) drew no cell.
;;   mark        a mark with visible items shows in no cell at all.
;;   baseline    a bar or area whose edge sits at the scale's zero does
;;               not reach the zero's cell; bars all standing on the
;;               plot's floor of a linear scale that excludes zero.
;;   label       an axis tick label of the scene is missing from its row.
;;   legend      a legend entry's label is missing from its row.
;;   collision   two axis tick labels claim the same cell.
;;   side        a bottom axis label not below its axis line, or a
;;               left axis label not left of it.
;;   contrast    a glyph's color is under WCAG 3:1 against the
;;               background, drawn for a light and for a dark one.
;;
;; Each problem is a string starting with its kind, so callers can
;; count them.  Nothing here draws differently from `eas-text-render'.

;;; Code:

(require 'eas-core)
(require 'eas-text)
(require 'eas-arc)
(require 'eas-scale)
(require 'eas-text-ink)

(defun eas-text-check--cell (scene)
  "SCENE's text cell size as (CW . CH)."
  (let ((cell (plist-get (plist-get scene :size) :cell)))
    (cons (aref cell 0) (aref cell 1))))

(defun eas-text-check--span (scene x y text align)
  "(ROW COL0 . COL1) a one-line TEXT anchored at X Y with ALIGN occupies,
as `eas-text--string-1' places it."
  (let ((cell (eas-text-check--cell scene)) (size (plist-get scene :size)))
    (eas-text-string-span (max 1 (round (/ (float (plist-get size :w)) (car cell))))
                          (car cell) (cdr cell) x y text align
                          (max 1 (round (/ (float (plist-get size :h)) (cdr cell)))))))

(defun eas-text-check--line (lines row)
  "Line ROW of LINES (a vector of strings), or the empty string."
  (if (< -1 row (length lines)) (aref lines row) ""))

(defun eas-text-check--shown-p (scene lines x y text align)
  "Non-nil when TEXT (its first line) drawn at X Y with ALIGN shows in LINES,
at its own columns."
  (let* ((text (car (split-string text "\n")))
         (span (eas-text-check--span scene x y text align))
         (line (eas-text-check--line lines (car span))))
    (or (string-empty-p (string-trim text))
        (and (<= 0 (cadr span))
             (equal (truncate-string-to-width line (cddr span) (cadr span)) text)))))

(defun eas-text-check--box (item mark)
  "Pixel box [X0 Y0 X1 Y1] of ITEM of MARK, or nil when it has no extent."
  (pcase mark
    ((or "line" "area" "trail")
     (let ((pts (append (plist-get item :points) (plist-get item :base) nil)))
       (when pts
         (vector (apply #'min (mapcar (lambda (p) (aref p 0)) pts)) (apply #'min (mapcar (lambda (p) (aref p 1)) pts))
                 (apply #'max (mapcar (lambda (p) (aref p 0)) pts)) (apply #'max (mapcar (lambda (p) (aref p 1)) pts))))))
    ((or "bar" "rect")
     ;; A zero-length bar (a value of 0) has nothing to draw.
     (let ((x (plist-get item :x)) (y (plist-get item :y)))
       (and x y (>= (plist-get item :w) 0.01) (>= (plist-get item :h) 0.01)
            (vector x y (+ x (plist-get item :w)) (+ y (plist-get item :h))))))
    ((or "rule" "tick")
     (let ((x1 (plist-get item :x1)) (x2 (plist-get item :x2)) (y1 (plist-get item :y1)) (y2 (plist-get item :y2)))
       (and x1 y1 x2 y2 (vector (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))))
    ("arc" (and (plist-get item :cx) (> (or (plist-get item :outerRadius) 0) 0)
                (eas-arc-bounds item)))
    ("text" (and (plist-get item :x) (stringp (plist-get item :text))
                 (not (string-empty-p (string-trim (plist-get item :text))))
                 (vector (plist-get item :x) (plist-get item :y) (plist-get item :x) (plist-get item :y))))
    (_ (and (plist-get item :x) (plist-get item :y)
            (vector (plist-get item :x) (plist-get item :y) (plist-get item :x) (plist-get item :y))))))

(defun eas-text-check--visible-p (item mark bounds)
  "Non-nil when ITEM of MARK should draw: opaque, with extent inside BOUNDS."
  (let ((box (eas-text-check--box item mark)))
    (and box (not (equal (plist-get item :opacity) 0))
         (not (and (equal (plist-get item :fill) "none") (member (plist-get item :stroke) '(nil "none"))
                   (member mark '("bar" "rect" "arc" "area"))))
         ;; Inside the plot, with half a pixel of grace on every edge.
         (<= (aref bounds 0) (+ (aref box 2) 0.5)) (<= (aref box 0) (+ (aref bounds 0) (aref bounds 2) 0.5))
         (<= (aref bounds 1) (+ (aref box 3) 0.5)) (<= (aref box 1) (+ (aref bounds 1) (aref bounds 3) 0.5)))))

(defun eas-text-check--zero (scale)
  "Pixel of SCALE's zero when SCALE is linear and its domain holds 0, else nil."
  (let ((d (plist-get scale :domain)))
    (when (and (equal (plist-get scale :type) "linear") (vectorp d) (= (length d) 2)
               (numberp (aref d 0)) (numberp (aref d 1))
               (<= (min (aref d 0) (aref d 1)) 0 (max (aref d 0) (aref d 1))))
      (eas-scale-apply scale 0))))

(defun eas-text-check--bar-baseline (view mark item i cells cw ch)
  "A problem string when bar ITEM (index I of MARK in VIEW) misses its zero."
  (let* ((orient (plist-get item :orient)) (vertical (equal orient "vertical"))
         (z (eas-text-check--zero (plist-get (plist-get view :scales) (if vertical :y :x))))
         (lo (if vertical (plist-get item :y) (plist-get item :x)))
         (hi (and lo (+ lo (if vertical (plist-get item :h) (plist-get item :w)))))
         (drawn (gethash (list (plist-get view :id) (plist-get mark :id) i) cells)))
    (when (and z drawn hi (member orient '("vertical" "horizontal")) (> (- hi lo) 0.5)
               (or (< (abs (- z lo)) 0.5) (< (abs (- z hi)) 0.5)))
      ;; The cell holding the zero, entered from the bar's side, within
      ;; the eighth of a cell a glyph can resolve.
      (let* ((size (if vertical ch cw)) (tol (+ 0.01 (/ size 8.0))) (dir (if (< (abs (- z hi)) 0.5) -1 1))
             (ks (list (floor (+ z (* dir 0.01)) size) (floor (+ z (* dir tol)) size))))
        (unless (seq-some (lambda (c) (memq (if vertical (cdr c) (car c)) ks)) drawn)
          (format "baseline: %s item %d of view %s stops short of zero"
                  (plist-get mark :id) i (plist-get view :id)))))))

(defun eas-text-check--baselines (view cells cw ch)
  "Baseline problems of VIEW's bars and areas, given CELLS (key -> cells)."
  (let* ((scales (plist-get view :scales)) (out nil)
         (yz (eas-text-check--zero (plist-get scales :y))))
    (seq-doseq (mark (plist-get view :marks))
      (let ((type (plist-get mark :mark)) (bottoms nil))
        (seq-do-indexed
         (lambda (item i)
           (when (equal type "bar")
             (when (and (equal (plist-get item :orient) "vertical") (plist-get item :y))
               (push (+ (plist-get item :y) (plist-get item :h)) bottoms))
             (when-let* ((p (eas-text-check--bar-baseline view mark item i cells cw ch))) (push p out)))
           (let ((drawn (gethash (list (plist-get view :id) (plist-get mark :id) i) cells)))
             (when (and (equal type "area") yz drawn (plist-get item :base)
                        (seq-every-p (lambda (p) (< (abs (- (aref p 1) yz)) 0.5)) (plist-get item :base)))
               ;; An area's bottom row shows once it covers half the cell.
               (let ((ks (list (floor (- yz 0.01) ch) (floor (- yz 0.01 (/ ch 2.0)) ch))))
                 (unless (seq-some (lambda (c) (memq (cdr c) ks)) drawn)
                   (push (format "baseline: area %s of view %s stops short of zero"
                                 (plist-get mark :id) (plist-get view :id))
                         out))))))
         (plist-get mark :items))
        ;; Bars that all stand on the floor of a linear scale without
        ;; zero lost Vega-Lite's zero default.
        (let* ((ys (plist-get scales :y)) (d (plist-get ys :domain))
               (b (plist-get view :bounds)) (floor-px (+ (aref b 1) (aref b 3))))
          (when (and (cdr bottoms) (equal (plist-get ys :type) "linear") (vectorp d) (numberp (aref d 0))
                     (numberp (aref d 1)) (> (min (aref d 0) (aref d 1)) 0)
                     (seq-every-p (lambda (y) (< (abs (- y floor-px)) 0.5)) bottoms))
            (push (format "baseline: bars of %s in view %s stand on %s, not zero"
                          (plist-get mark :id) (plist-get view :id) (min (aref d 0) (aref d 1)))
                  out)))))
    (nreverse out)))

(defun eas-text-check--label-spans (scene)
  "Every tick label of SCENE as (VIEW AXIS TK CELLS), CELLS its (COL . ROW)s."
  (let (out)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (axis (plist-get view :axes))
        (seq-doseq (tk (plist-get axis :ticks))
          (let ((label (plist-get tk :label)))
            (when (and (stringp label) (not (string-empty-p label)) (plist-get tk :lx))
              (let ((span (eas-text-check--span scene (plist-get tk :lx) (plist-get tk :ly)
                                                (car (split-string label "\n")) (plist-get tk :align))))
                (push (list view axis tk (cl-loop for c from (cadr span) below (cddr span) collect (cons c (car span))))
                      out)))))))
    (nreverse out)))

(defun eas-text-check--labels (scene lines)
  "Axis label problems of SCENE drawn as LINES.
A label may be left out only where it would overlap a label that shows
\(the renderer drops it, as Vega's labelOverlap does); every axis with
labels shows at least one; no two shown labels share a cell."
  (let* ((all (eas-text-check--label-spans scene))
         (shown (seq-filter (lambda (l) (let ((tk (nth 2 l)))
                                          (eas-text-check--shown-p scene lines (plist-get tk :lx) (plist-get tk :ly)
                                                                   (plist-get tk :label) (plist-get tk :align))))
                            all))
         (taken (make-hash-table :test 'equal)) (out nil))
    (dolist (l shown)
      (dolist (c (nth 3 l))
        (let ((owner (gethash c taken)))
          (when (and owner (not (eq owner (nth 2 l))))
            (push (format "collision: axis labels %S and %S of view %s share a cell"
                          (plist-get owner :label) (plist-get (nth 2 l) :label) (plist-get (nth 0 l) :id))
                  out))
          (puthash c (nth 2 l) taken))))
    (dolist (l all)
      (unless (or (memq l shown) (seq-some (lambda (c) (gethash c taken)) (nth 3 l)))
        (push (format "label: %s axis label %S of view %s is not shown"
                      (plist-get (nth 1 l) :channel) (plist-get (nth 2 l) :label) (plist-get (nth 0 l) :id))
              out)))
    (dolist (axis (delete-dups (mapcar #'cadr all)))
      (unless (seq-some (lambda (l) (eq (nth 1 l) axis)) shown)
        (push (format "label: %s axis of view %s shows none of its labels"
                      (plist-get axis :channel)
                      (plist-get (nth 0 (seq-find (lambda (l) (eq (nth 1 l) axis)) all)) :id))
              out)))
    (delete-dups (nreverse out))))

(defun eas-text-check--sides (scene)
  "Axis labels of SCENE on the wrong side of their axis line."
  (let ((cell (eas-text-check--cell scene)) out)
    (pcase-dolist (`(,view ,axis ,tk ,cells) (eas-text-check--label-spans scene))
      (let ((line (plist-get axis :domain-line)) (orient (plist-get axis :orient)))
        (when (and line cells)
          (pcase orient
            ("bottom" (unless (> (cdar cells) (floor (aref line 1) (cdr cell)))
                        (push (format "side: x axis label %S of view %s is not below its axis line"
                                      (plist-get tk :label) (plist-get view :id))
                              out)))
            ("left" (unless (< (car (car (last cells))) (floor (aref line 0) (car cell)))
                      (push (format "side: y axis label %S of view %s is not left of its axis line"
                                    (plist-get tk :label) (plist-get view :id))
                            out)))))))
    (nreverse out)))

(defun eas-text-check--mark-cells (view mark cells)
  "Every cell CELLS records for MARK of VIEW."
  (cl-loop for i below (length (plist-get mark :items))
           append (gethash (list (plist-get view :id) (plist-get mark :id) i) cells)))

(defun eas-text-check--covered-p (view mark cells)
  "Non-nil when marks drawn after MARK in VIEW cover each of its cells:
painter's order hid it, as SVG would (a halo under its line)."
  (let* ((later (cdr (memq mark (append (plist-get view :marks) nil))))
         (over (make-hash-table :test 'equal)))
    (dolist (m later) (dolist (c (eas-text-check--mark-cells view m cells)) (puthash c t over)))
    (seq-every-p (lambda (c) (gethash c over)) (eas-text-check--mark-cells view mark cells))))

(defun eas-text-check-contrast (scene mode)
  "Contrast problems of SCENE drawn as text on a MODE (light or dark) background."
  (let* ((text (let ((eas-text-background-mode mode)) (eas-text-render scene)))
         (bg (eas-text-ink-background mode)) (seen nil) (out nil) (pos 0))
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'face text) (length text)))
            (face (get-text-property pos 'face text)))
        (when-let* ((fg (and (consp face) (plist-get face :foreground))))
          (when (and (stringp fg) (not (member fg seen)) (eas-text-ink--rgb fg)
                     (string-match-p "[^[:space:]]" (substring-no-properties text pos next)))
            (push fg seen)
            (let ((ratio (eas-text-ink-contrast fg bg)))
              (when (< ratio eas-text-ink-min-contrast)
                (push (format "contrast: %s on the %s background %s is %.2f:1 (mark %s)"
                              fg mode bg ratio (get-text-property pos 'eas-mark text))
                      out)))))
        (setq pos next)))
    (nreverse out)))

(defun eas-text-check (scene)
  "Problems (strings) of SCENE's text rendering; nil when it holds.
SCENE must be compiled for the text target."
  (let* ((cells (make-hash-table :test 'equal))
         (text (let ((eas-text-trace (lambda (col row) (push (cons col row) (gethash eas-text-trace-item cells)))))
                 (eas-text-render scene)))
         (lines (vconcat (split-string text "\n")))
         (cw (car (eas-text-check--cell scene))) (ch (cdr (eas-text-check--cell scene)))
         (shown (make-hash-table :test 'equal))
         (out nil))
    (when (string-empty-p (string-trim text)) (push "empty: the rendering is blank" out))
    ;; Which (VIEW MARK) pairs survive in the final grid.
    (let ((pos 0))
      (while (< pos (length text))
        (when-let* ((m (get-text-property pos 'eas-mark text)))
          (puthash (list (get-text-property pos 'eas-view text) m) t shown))
        (setq pos (or (next-single-property-change pos 'eas-mark text) (length text)))))
    (seq-doseq (view (plist-get scene :views))
      (let ((bounds (plist-get view :bounds)))
        (seq-doseq (mark (plist-get view :marks))
          (let ((type (plist-get mark :mark)) (any nil))
            (unless (equal type "brush")
              (seq-do-indexed
               (lambda (item i)
                 (when (eas-text-check--visible-p item type bounds)
                   (setq any t)
                   (unless (gethash (list (plist-get view :id) (plist-get mark :id) i) cells)
                     (push (format "item: %s %s item %d of view %s draws no cell"
                                   type (plist-get mark :id) i (plist-get view :id))
                           out))))
               (plist-get mark :items))
              (when (and any (not (gethash (list (plist-get view :id) (plist-get mark :id)) shown))
                         (not (eas-text-check--covered-p view mark cells)))
                (push (format "mark: %s %s of view %s shows in no cell" type (plist-get mark :id) (plist-get view :id))
                      out)))))
        (setq out (append (reverse (eas-text-check--baselines view cells cw ch)) out))
        (seq-doseq (legend (plist-get view :legends))
          (seq-doseq (e (plist-get legend :entries))
            (when (and (stringp (plist-get e :label)) (plist-get e :lx))
              (unless (eas-text-check--shown-p scene lines (plist-get e :lx) (plist-get e :ly) (plist-get e :label) "left")
                (push (format "legend: entry %S of view %s is not shown" (plist-get e :label) (plist-get view :id))
                      out)))))))
    (delete-dups (append (nreverse out) (eas-text-check--labels scene lines) (eas-text-check--sides scene)
                         (eas-text-check-contrast scene 'light) (eas-text-check-contrast scene 'dark)))))

(defun eas-text-check-kind (problem)
  "The kind of PROBLEM, a string from `eas-text-check'."
  (car (split-string problem ":")))

(provide 'eas-text-check)
;;; eas-text-check.el ends here
