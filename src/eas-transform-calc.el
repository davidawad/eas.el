;;; eas-transform-calc.el --- lookup, regression, loess and quantile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Vega-Lite transforms of the gallery's "Advanced Calculations"
;; beyond the core set, ported from vega-transforms, vega-regression
;; and vega-statistics so that output matches Vega's point for point:
;;
;;   lookup      join a secondary inline dataset on a key
;;   regression  linear/log/exp/pow/quad/poly fits: the curve, or its
;;               coefficients and R^2 with "params": true
;;   loess       locally weighted regression (bandwidth 0.3, 2 robust passes)
;;   quantile    quantiles (R-7) at "probs" or every "step" from step/2
;;
;; `eas-transform-calc-key' names the transform a row of the transform
;; array is (or nil); `eas-transform-calc-apply' runs it.

;;; Code:

(require 'eas-core)
(require 'eas-transform-agg)

(defconst eas-transform-calc-keys '(:lookup :regression :loess :quantile)
  "Transforms this file implements.")

(defun eas-transform-calc-key (tr)
  "The key of TR when it is one of `eas-transform-calc-keys', else nil."
  (seq-find (lambda (k) (plist-member tr k)) eas-transform-calc-keys))

(defun eas-transform-calc--groups (rows groupby)
  "ROWS partitioned by GROUPBY field names: ((KEY-VALUES . ROWS) ...)."
  (eas-agg--groups rows (mapcar #'eas-key groupby)))

(defun eas-transform-calc--dims (groupby key-values)
  "A row plist holding GROUPBY's KEY-VALUES."
  (cl-loop for g in (append groupby nil) for v in key-values append (list (eas-key g) v)))

;;; lookup

(defun eas-transform-calc--lookup (tr rows path)
  "Vega-Lite lookup TR over ROWS."
  (let* ((from (plist-get tr :from))
         (values (plist-get (plist-get from :data) :values))
         (key (and (plist-get from :key) (eas-key (plist-get from :key))))
         (field (eas-key (plist-get tr :lookup)))
         (fields (append (plist-get from :fields) nil))
         (as (let ((a (plist-get tr :as))) (if (stringp a) (list a) (append a nil))))
         (default (if (plist-member tr :default) (plist-get tr :default) :null))
         (index (make-hash-table :test 'equal)))
    (unless (and (vectorp values) key)
      (eas-signal "UNSUPPORTED_FEATURE" "lookup needs from.data with inline values (or a url) and from.key"
                  :path (concat path "/from") :feature "transform/lookup"))
    (seq-doseq (r values) (puthash (plist-get r key) r index))
    (seq-map (lambda (row)
               (let ((hit (gethash (plist-get row field) index)))
                 (if fields
                     (cl-loop for f in fields for i from 0
                              do (setq row (eas-plist-put row (eas-key (or (nth i as) f))
                                                          (if hit (let ((v (plist-member hit (eas-key f)))) (if v (cadr v) :null))
                                                            default))))
                   (setq row (eas-plist-put row (eas-key (car as)) (or hit default))))
                 row))
             rows)))

;;; regression

(defun eas-transform-calc--xy (rows x y)
  "Valid (X . Y) number pairs of ROWS for field keys X and Y."
  (cl-loop for r across (vconcat rows)
           for u = (plist-get r x) for v = (plist-get r y)
           when (and (numberp u) (numberp v) (not (isnan (float u))) (not (isnan (float v))))
           collect (cons (float u) (float v))))

(defun eas-transform-calc--ols (ux uy uxy ux2)
  "Vega's ordinary least squares: (INTERCEPT SLOPE) from the means."
  (let* ((delta (- ux2 (* ux ux)))
         (slope (if (< (abs delta) 1e-24) 0.0 (/ (- uxy (* ux uy)) delta))))
    (list (- uy (* slope ux)) slope)))

(defun eas-transform-calc--means (pts fx fy)
  "Running means of FX(x), FY(y), FX*FY and FX^2 over PTS, as Vega computes them."
  (let ((X 0.0) (Y 0.0) (XY 0.0) (X2 0.0) (n 0))
    (dolist (p pts)
      (let ((dx (funcall fx (car p))) (dy (funcall fy (cdr p))))
        (setq n (1+ n)
              X (+ X (/ (- dx X) n)) Y (+ Y (/ (- dy Y) n))
              XY (+ XY (/ (- (* dx dy) XY) n)) X2 (+ X2 (/ (- (* dx dx) X2) n)))))
    (list X Y XY X2)))

(defun eas-transform-calc--r2 (pts predict)
  "Vega's R^2 of PREDICT over PTS."
  (let* ((uy (/ (apply #'+ (mapcar #'cdr pts)) (float (length pts)))) (sse 0.0) (sst 0.0))
    (dolist (p pts)
      (setq sse (+ sse (expt (- (cdr p) (funcall predict (car p))) 2))
            sst (+ sst (expt (- (cdr p) uy) 2))))
    (- 1 (/ sse sst))))

(defun eas-transform-calc--solve (a b)
  "Solve the square system A x = B (lists of rows) by Gaussian elimination."
  (let* ((n (length b))
         (m (vconcat (cl-loop for row in a for v in b collect (vconcat (append row (list v)))))))
    (dotimes (c n)
      (let ((p (cl-loop with best = c for r from c below n
                        when (> (abs (aref (aref m r) c)) (abs (aref (aref m best) c))) do (setq best r)
                        finally return best)))
        (cl-rotatef (aref m c) (aref m p))
        (cl-loop for r from 0 below n unless (= r c)
                 do (let ((f (/ (aref (aref m r) c) (aref (aref m c) c))))
                      (dotimes (k (1+ n)) (aset (aref m r) k (- (aref (aref m r) k) (* f (aref (aref m c) k)))))))))
    (cl-loop for r below n collect (/ (aref (aref m r) n) (aref (aref m r) r)))))

(defun eas-transform-calc--poly (pts order)
  "Least-squares polynomial of ORDER through PTS, centred as Vega fits it.\nReturn (COEF . PREDICT)."
  (let* ((ux (/ (apply #'+ (mapcar #'car pts)) (float (length pts))))
         (uy (/ (apply #'+ (mapcar #'cdr pts)) (float (length pts))))
         (k (1+ order))
         (a (cl-loop for i below k collect
                     (cl-loop for j below k collect
                              (apply #'+ (mapcar (lambda (p) (expt (- (car p) ux) (+ i j))) pts)))))
         (b (cl-loop for i below k collect
                     (apply #'+ (mapcar (lambda (p) (* (- (cdr p) uy) (expt (- (car p) ux) i))) pts))))
         (c (eas-transform-calc--solve a b))
         (predict (lambda (x) (+ uy (cl-loop for ci in c for i from 0 sum (* ci (expt (- x ux) i)))))))
    (cons (eas-transform-calc--uncenter c (- ux) uy) predict)))

(defun eas-transform-calc--uncenter (coef x y)
  "Vega's uncenter: COEF (about the mean) shifted by X and Y.
The result is plain polynomial coefficients."
  (let* ((k (length coef)) (z (make-vector k 0.0)))
    (cl-loop for i downfrom (1- k) to 0
             do (let ((v (nth i coef)) (c 1.0))
                  (aset z i (+ (aref z i) v))
                  (cl-loop for j from 1 to i
                           do (setq c (* c (/ (float (- (1+ i) j)) j)))
                           (aset z (- i j) (+ (aref z (- i j)) (* v (expt x j) c))))))
    (aset z 0 (+ (aref z 0) y))
    z))

(defun eas-transform-calc--fit (method pts order)
  "Fit METHOD to PTS: (COEF PREDICT)."
  (pcase method
    ("constant" (let ((uy (/ (apply #'+ (mapcar #'cdr pts)) (float (length pts)))))
                  (list (vector uy) (lambda (_) uy))))
    ("linear" (let ((c (apply #'eas-transform-calc--ols (eas-transform-calc--means pts #'identity #'identity))))
                (list (vconcat c) (lambda (x) (+ (nth 0 c) (* (nth 1 c) x))))))
    ("log" (let ((c (apply #'eas-transform-calc--ols (eas-transform-calc--means pts #'log #'identity))))
             (list (vconcat c) (lambda (x) (+ (nth 0 c) (* (nth 1 c) (log x)))))))
    ("exp" (let* ((m (eas-transform-calc--means pts #'identity #'log))
                  (c (apply #'eas-transform-calc--ols m)) (a (exp (nth 0 c))) (b (nth 1 c)))
             (list (vector a b) (lambda (x) (* a (exp (* b x)))))))
    ("pow" (let* ((c (apply #'eas-transform-calc--ols (eas-transform-calc--means pts #'log #'log)))
                  (a (exp (nth 0 c))) (b (nth 1 c)))
             (list (vector a b) (lambda (x) (* a (expt x b))))))
    ((or "quad" "poly") (let ((f (eas-transform-calc--poly pts (if (equal method "quad") 2 order))))
                          (list (car f) (cdr f))))
    (_ (eas-signal "INVALID_INPUT" (format "Unknown regression method %s" method)
                   :feature "transform/regression"))))

(defun eas-transform-calc--sample-curve (f lo hi &optional min-steps max-steps)
  "Vega's adaptive sampling of F over [LO HI]: a list of (X Y)."
  (let* ((min-steps (or min-steps 25)) (max-steps (max min-steps (or max-steps 200)))
         (point (lambda (x) (list x (funcall f x))))
         (span (- hi lo)) (stop (/ span max-steps))
         (prev (list (funcall point lo))) (next (list (funcall point hi))))
    (cl-loop for i downfrom (1- min-steps) above 0
             do (setq next (append next (list (funcall point (+ lo (* (/ i (float min-steps)) span)))))))
    (let* ((ys (mapcar #'cadr (cons (car prev) next)))
           (sx (/ 1.0 span)) (sy (/ 1.0 (- (apply #'max ys) (apply #'min ys))))
           (angle (lambda (p q r) (abs (- (atan (* sy (- (cadr r) (cadr p))) (* sx (- (car r) (car p))))
                                          (atan (* sy (- (cadr q) (cadr p))) (* sx (- (car q) (car p))))))))
           (min-rad (/ (* 0.5 float-pi) 180))
           (p0 (car prev)) (p1 (car (last next))) (out (list p0)))
      ;; NEXT is a stack whose top is its last element, as in Vega.
      (while p1
        (let ((pm (funcall point (/ (+ (car p0) (car p1)) 2.0))))
          (if (and (>= (- (car pm) (car p0)) stop) (> (funcall angle p0 pm p1) min-rad))
              (setq next (append next (list pm)))
            (setq p0 p1 out (cons p1 out) next (butlast next))))
        (setq p1 (car (last next))))
      (nreverse out))))

(defun eas-transform-calc--regression (tr rows _path)
  "Vega-Lite regression TR over ROWS."
  (let* ((y (plist-get tr :regression)) (x (plist-get tr :on))
         (method (or (plist-get tr :method) "linear"))
         (order (or (plist-get tr :order) 3))
         (dof (pcase method ("poly" order) ("quad" 2) (_ 1)))
         (groupby (plist-get tr :groupby))
         (as (or (plist-get tr :as) (vector x y)))
         (extent (plist-get tr :extent))
         out)
    (dolist (g (eas-transform-calc--groups rows groupby))
      (let ((pts (eas-transform-calc--xy (cdr g) (eas-key x) (eas-key y)))
            (dims (eas-transform-calc--dims groupby (car g))))
        (when (> (length pts) dof)
          (let* ((fit (eas-transform-calc--fit method pts order))
                 (predict (nth 1 fit)))
            (if (eq (plist-get tr :params) t)
                (push (append dims (list :keys (vconcat (car g)) :coef (nth 0 fit)
                                         :rSquared (eas-transform-calc--r2 pts predict)))
                      out)
              (let* ((xs (mapcar #'car pts))
                     (lo (if extent (aref extent 0) (apply #'min xs)))
                     (hi (if extent (aref extent 1) (apply #'max xs))))
                (dolist (p (if (member method '("linear" "constant"))
                               (list (list lo (funcall predict lo)) (list hi (funcall predict hi)))
                             (eas-transform-calc--sample-curve predict lo hi 25 200)))
                  (push (append dims (list (eas-key (aref as 0)) (car p) (eas-key (aref as 1)) (cadr p))) out))))))))
    (vconcat (nreverse out))))

;;; loess

(defun eas-transform-calc--tricube (x)
  "The tricube weight of X."
  (let ((x (- 1 (* x x x)))) (* x x x)))

(defun eas-transform-calc--median (v)
  "Median of float vector V."
  (let* ((s (sort (copy-sequence v) #'<)) (n (length s)))
    (if (cl-oddp n) (aref s (/ n 2)) (/ (+ (aref s (1- (/ n 2))) (aref s (/ n 2))) 2.0))))

(defun eas-transform-calc--loess-fit (pts bandwidth)
  "Vega's loess over PTS (x-sorted (X . Y)) with BANDWIDTH: a list of (X Y)."
  (let* ((n (length pts))
         (ux (/ (apply #'+ (mapcar #'car pts)) (float n)))
         (uy (/ (apply #'+ (mapcar #'cdr pts)) (float n)))
         (xv (vconcat (mapcar (lambda (p) (- (car p) ux)) pts)))
         (yv (vconcat (mapcar (lambda (p) (- (cdr p) uy)) pts)))
         (bw (max 2 (truncate (* bandwidth n))))
         (yhat (make-vector n 0.0)) (residuals (make-vector n 0.0))
         (robust (make-vector n 1.0)) (eps 1e-12) (iter 0) (done nil))
    (while (and (not done) (<= iter 2))
      (let ((i0 0) (i1 (1- bw)) (last nil))
        (dotimes (i n)
          (let ((dx (aref xv i)))
            ;; The fit depends only on x and the window, so a repeated x
            ;; in the same window reuses it (exactly what Vega computes).
            (unless (and last (= (nth 0 last) dx) (= (nth 1 last) i0) (= (nth 2 last) i1))
              (let* ((edge (if (> (- dx (aref xv i0)) (- (aref xv i1) dx)) i0 i1))
                     (d (abs (- (aref xv edge) dx))) (denom (/ 1.0 (if (zerop d) 1 d)))
                     (W 0.0) (X 0.0) (Y 0.0) (XY 0.0) (X2 0.0))
                (cl-loop for k from i0 to i1
                         do (let* ((xk (aref xv k)) (yk (aref yv k))
                                   (w (* (eas-transform-calc--tricube (* (abs (- dx xk)) denom)) (aref robust k)))
                                   (xkw (* xk w)))
                              (setq W (+ W w) X (+ X xkw) Y (+ Y (* yk w)) XY (+ XY (* yk xkw)) X2 (+ X2 (* xk xkw)))))
                (let ((c (eas-transform-calc--ols (/ X W) (/ Y W) (/ XY W) (/ X2 W))))
                  (setq last (list dx i0 i1 (+ (nth 0 c) (* (nth 1 c) dx)))))))
            (aset yhat i (nth 3 last))
            (aset residuals i (abs (- (aref yv i) (aref yhat i))))
            ;; Vega's updateInterval: slide the window toward the next x.
            (let ((j (1+ i)))
              (when (< j n)
                (let ((val (aref xv j)) (left i0) (right (1+ i1)))
                  (when (< right n)
                    (while (and (> j left) (< right n) (<= (- (aref xv right) val) (- val (aref xv left))))
                      (setq left (1+ left) i0 left i1 right right (1+ right))))))))))
      (if (= iter 2) (setq done t)
        (let ((med (eas-transform-calc--median residuals)))
          (if (< (abs med) eps) (setq done t)
            (dotimes (i n)
              (let ((arg (/ (aref residuals i) (* 6 med))))
                (aset robust i (if (>= arg 1) eps (let ((w (- 1 (* arg arg)))) (* w w)))))))))
      (setq iter (1+ iter)))
    ;; Average fits at equal x, then shift back by the means.
    (let (out prev (cnt 0))
      (dotimes (i n)
        (let ((v (+ (aref xv i) ux)))
          (if (and prev (= (car prev) v))
              (progn (setq cnt (1+ cnt))
                     (setcar (cdr prev) (+ (cadr prev) (/ (- (aref yhat i) (cadr prev)) cnt))))
            (setq cnt 0 prev (list v (aref yhat i)))
            (push prev out))))
      (mapcar (lambda (p) (list (car p) (+ (cadr p) uy))) (nreverse out)))))

(defun eas-transform-calc--loess (tr rows _path)
  "Vega-Lite loess TR over ROWS."
  (let* ((y (plist-get tr :loess)) (x (plist-get tr :on))
         (groupby (plist-get tr :groupby))
         (as (or (plist-get tr :as) (vector x y)))
         out)
    (dolist (g (eas-transform-calc--groups rows groupby))
      (let ((pts (sort (eas-transform-calc--xy (cdr g) (eas-key x) (eas-key y)) (lambda (a b) (< (car a) (car b)))))
            (dims (eas-transform-calc--dims groupby (car g))))
        (when pts
          (dolist (p (eas-transform-calc--loess-fit pts (or (plist-get tr :bandwidth) 0.3)))
            (push (append dims (list (eas-key (aref as 0)) (car p) (eas-key (aref as 1)) (cadr p))) out)))))
    (vconcat (nreverse out))))

;;; quantile

(defun eas-transform-calc--quantile-sorted (v p)
  "d3's quantileSorted (R-7) of sorted vector V at P."
  (let ((n (length v)))
    (cond ((zerop n) :null)
          ((or (<= p 0) (< n 2)) (aref v 0))
          ((>= p 1) (aref v (1- n)))
          (t (let* ((i (* (1- n) p)) (i0 (floor i)) (a (aref v i0)) (b (aref v (1+ i0))))
               (+ a (* (- b a) (- i i0))))))))

(defun eas-transform-calc--quantile (tr rows _path)
  "Vega-Lite quantile TR over ROWS."
  (let* ((field (eas-key (plist-get tr :quantile)))
         (groupby (plist-get tr :groupby))
         (as (or (plist-get tr :as) ["prob" "value"]))
         (step (or (plist-get tr :step) 0.01))
         (probs (if (plist-get tr :probs) (append (plist-get tr :probs) nil)
                  ;; d3 range(step/2, 1 - 1e-14, step)
                  (let ((n (max 0 (ceiling (/ (- (- 1 1e-14) (/ step 2.0)) step)))))
                    (cl-loop for i below n collect (+ (/ step 2.0) (* i step))))))
         out)
    (dolist (g (eas-transform-calc--groups rows groupby))
      (let ((v (vconcat (sort (cl-loop for r in (cdr g) for x = (plist-get r field)
                                       when (and (numberp x) (not (isnan (float x)))) collect (float x))
                              #'<)))
            (dims (eas-transform-calc--dims groupby (car g))))
        (dolist (p probs)
          (push (append dims (list (eas-key (aref as 0)) p
                                   (eas-key (aref as 1)) (eas-transform-calc--quantile-sorted v p)))
                out))))
    (vconcat (nreverse out))))

(defun eas-transform-calc-apply (tr rows path)
  "Apply TR (one of `eas-transform-calc-keys') to ROWS; PATH is its JSON pointer."
  (pcase (eas-transform-calc-key tr)
    (:lookup (eas-transform-calc--lookup tr rows path))
    (:regression (eas-transform-calc--regression tr rows path))
    (:loess (eas-transform-calc--loess tr rows path))
    (:quantile (eas-transform-calc--quantile tr rows path))))

(provide 'eas-transform-calc)
;;; eas-transform-calc.el ends here
