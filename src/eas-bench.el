;;; eas-bench.el --- the performance ladder and its regression budget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fc-qx1.9.  `eas-bench-ladder' measures the engine at 1k, 10k and
;; 100k points on two fixed workloads, 800x400 (text 100x30):
;;
;;   line    a line with the Vega-Lite crosshair idiom (a rule layer
;;           filtered by a nearest pointermove point selection)
;;   points  a scatter of the same rows (the grid hit-test index)
;;
;; Stages, each (:mean MS :max MS :reps N):
;;
;;   compile-svg compile-text     whole scene, both targets
;;   render-svg render-text       scene -> SVG string / text grid
;;   hover-first                  the first pointermove (builds indexes)
;;   hover                        one pointermove: reduce, hit-test,
;;                                patch, inspect
;;   hover-svg hover-text         a pointermove plus the redraw the glue
;;                                does, before librsvg and redisplay
;;   hit-line hit-points          one eas-hit query
;;   lttb                         decimating N points to 800
;;   compile-points               the scatter scene
;;
;; GC follows the glue: "deferred" (default) runs at
;; `eas-gc-cons-threshold' as an interaction does; "default" keeps
;; this Emacs's own threshold.  Each stage starts after a collection.
;;
;; `eas-bench-check' compares a ladder with bench-budget.json: each
;; stage's mean may reach reference x tolerance x machine factor (or the
;; floor), where the factor is this machine's calibration time over the
;; reference machine's.  Targets (hover under 50 ms at 10k, fc-qx1.14's
;; hypothesis) are reported, not enforced.  Interpreted code is 4-10x
;; slower than byte-compiled, so the check only compares like with like.

;;; Code:

(require 'eas-core)
(require 'eas-compile)
(require 'eas-scene)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-hit)
(require 'eas-lttb)
(require 'eas-view)
(require 'eas-gc)

(defconst eas-bench-points '(1000 10000 100000) "The ladder's default sizes.")

(defconst eas-bench-size '(800 . 400) "Pixel size of the SVG workloads.")

(defconst eas-bench-text-size '(:cols 100 :rows 30) "Cell size of the text workloads.")

(defconst eas-bench-stages
  '("compile-svg" "render-svg" "compile-text" "render-text" "hover-first" "hover"
    "hover-svg" "hover-text" "hit-line" "compile-points" "hit-points" "lttb")
  "Stages of one ladder rung, in order.")

(defconst eas-bench-budget-file
  (expand-file-name "bench-budget.json"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Reference numbers and tolerances for the CI regression check.")

;;; Workloads

(defun eas-bench-rows (n)
  "N deterministic rows (:t I :p PRICE), t ascending."
  (let ((rows (make-vector n nil)))
    (dotimes (i n)
      (aset rows i (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))) (* 0.01 (mod (* i 7919) 101))))))
    rows))

(defun eas-bench-line-spec (rows)
  "The line workload over ROWS: a line and a crosshair rule."
  (list :data (list :values rows)
        :layer (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t
                                                                     :encodings ["x"]))]
                             :mark "line"
                             :encoding '(:x (:field "t" :type "quantitative")
                                         :y (:field "p" :type "quantitative" :scale (:zero :false))))
                       (list :transform [(:filter (:param "hover" :empty :false))]
                             :mark "rule" :encoding '(:x (:field "t" :type "quantitative"))))))

(defun eas-bench-points-spec (rows)
  "The scatter workload over ROWS."
  (list :data (list :values rows) :mark "point"
        :encoding '(:x (:field "t" :type "quantitative")
                    :y (:field "p" :type "quantitative" :scale (:zero :false)))))

;;; Timing

(defun eas-bench-compiled-p ()
  "Non-nil when the engine is byte- or natively compiled."
  (let ((f (symbol-function 'eas-dispatch)))
    (or (byte-code-function-p f) (and (fboundp 'native-comp-function-p) (native-comp-function-p f))
        (and (fboundp 'subr-native-elisp-p) (subr-native-elisp-p f)))))

(defun eas-bench-time (reps fn &optional cold)
  "Call FN REPS times after one warm-up (none when COLD); return the timing.
The result is (:mean MS :max MS :reps REPS); a collection runs first."
  (unless cold (funcall fn))
  (garbage-collect)
  (let ((total 0.0) (worst 0.0))
    (dotimes (_ reps)
      (let ((start (float-time)))
        (funcall fn)
        (let ((ms (* 1000.0 (- (float-time) start))))
          (setq total (+ total ms) worst (max worst ms)))))
    (list :mean (eas-scene-round (/ total reps)) :max (eas-scene-round worst) :reps reps)))

(defvar eas-bench-calibration-runs 5 "Runs of the calibration workload; the best counts.")

(defun eas-bench-calibrate ()
  "Return the best time of a fixed workload, in ms.
The best is over `eas-bench-calibration-runs' passes.  This is the
machine's speed, against which budget limits scale."
  (let ((best nil))
    (dotimes (_ eas-bench-calibration-runs)
      (garbage-collect)
      (let ((start (float-time)))
        (let ((rows (make-vector 20000 nil)))
          (dotimes (i 20000)
            (aset rows i (list :t i :p (* 1.5 (sin i)) :k (number-to-string (mod i 97)))))
          (let ((table (make-hash-table :test 'equal)))
            (seq-doseq (r rows) (puthash (plist-get r :k) (+ (plist-get r :p) (gethash (plist-get r :k) table 0.0)) table))
            (sort (append rows nil) (lambda (a b) (< (plist-get a :p) (plist-get b :p))))))
        (let ((ms (* 1000.0 (- (float-time) start))))
          (setq best (if best (min best ms) ms)))))
    (eas-scene-round best)))

(defun eas-bench--sweep (bounds k)
  "Pointer K of a sweep across plot BOUNDS [x y w h], mid-height."
  (vector (+ (aref bounds 0) (* (aref bounds 2) (/ (+ (mod (* k 37) 97) 0.5) 97.0)))
          (+ (aref bounds 1) (/ (aref bounds 3) 2.0))))

(defun eas-bench--bounds (scene)
  "Plot bounds of SCENE's first view."
  (plist-get (aref (plist-get scene :views) 0) :bounds))

(defun eas-bench--reps (n reps)
  "REPS scaled down for N points: at least 2, fewer above 10k."
  (max 2 (min reps (/ (* reps 10000) n))))

(defun eas-bench-rung (n reps)
  "Measure every stage at N points, REPS samples each; return the rung."
  (let* ((rows (eas-bench-rows n)) (spec (eas-bench-line-spec rows))
         (r (eas-bench--reps n reps)) (fast (* 10 reps))
         (eas-views (make-hash-table :test 'equal))
         (svg (eas-view-open spec :id "bench" :size eas-bench-size))
         (text (eas-view-open spec :id "bench-text" :size eas-bench-text-size :target 'text))
         (b (eas-bench--bounds (eas-view-scene svg)))
         (tb (eas-bench--bounds (eas-view-scene text)))
         (k 0) (out nil))
    (cl-flet ((stage (name timing) (setq out (append out (list (intern (concat ":" name)) timing))))
              (move (view bounds) (eas-dispatch view (list :type "pointermove"
                                                             :px (eas-bench--sweep bounds (cl-incf k))))))
      (stage "compile-svg" (eas-bench-time r (lambda () (eas-compile spec :size eas-bench-size))))
      (stage "hover-first" (eas-bench-time 1 (lambda () (move svg b)) t))
      (stage "render-svg" (eas-bench-time r (lambda () (eas-svg-render (eas-view-scene svg)))))
      (stage "compile-text" (eas-bench-time r (lambda () (eas-compile spec :size eas-bench-text-size
                                                                          :target 'text))))
      (move text tb)
      (stage "render-text" (eas-bench-time r (lambda () (eas-text-render (eas-view-scene text)))))
      (stage "hover" (eas-bench-time fast (lambda () (move svg b))))
      (stage "hover-svg" (eas-bench-time fast (lambda () (move svg b) (eas-svg-render (eas-view-scene svg)))))
      (stage "hover-text" (eas-bench-time fast (lambda () (move text tb)
                                                    (eas-text-render (eas-view-scene text)))))
      (let ((scene (eas-view-scene svg)))
        (stage "hit-line" (eas-bench-time (* 10 fast) (lambda () (eas-hit scene nil (eas-bench--sweep b (cl-incf k)) t)))))
      (let* ((pspec (eas-bench-points-spec rows)) (pscene nil))
        (stage "compile-points" (eas-bench-time r (lambda () (setq pscene (eas-compile pspec :size eas-bench-size)))))
        (let ((pb (eas-bench--bounds pscene)))
          (stage "hit-points" (eas-bench-time (* 10 fast)
                                                (lambda () (eas-hit pscene nil
                                                                      (vector (aref (eas-bench--sweep pb (cl-incf k)) 0)
                                                                              (+ (aref pb 1) (* (aref pb 3) (/ (mod k 13) 13.0))))))))))
      (let ((xs (vconcat (number-sequence 0 (1- n))))
            (ys (vconcat (mapcar (lambda (row) (plist-get row :p)) rows))))
        (stage "lttb" (eas-bench-time r (lambda () (eas-lttb-indices xs ys (car eas-bench-size))))))
      (append (list :points n :items (eas-bench--items (eas-view-scene svg))) out))))

(defun eas-bench--items (scene)
  "Items drawn in SCENE, over every view and mark."
  (cl-loop for v across (plist-get scene :views)
           sum (cl-loop for m across (plist-get v :marks) sum (length (plist-get m :items)))))

(cl-defun eas-bench-ladder (&key points reps gc)
  "Measure every stage at each of POINTS (default 1k, 10k, 100k).
REPS (default 5) samples the slow stages; hover takes 10x that.  GC is
`deferred' (default, as the glue runs) or `default'."
  (let* ((gc (or gc 'deferred))
         (gc-cons-threshold (if (eq gc 'deferred)
                                (max gc-cons-threshold (or eas-gc-cons-threshold 0))
                              gc-cons-threshold))
         (reps (or reps 5)))
    (list :contract "eas-bench/v1"
          :emacs emacs-version :system (symbol-name system-type)
          :compiled (if (eas-bench-compiled-p) t :false)
          :batch (if noninteractive t :false)
          :gc (symbol-name gc) :gc-threshold gc-cons-threshold
          :calibration-ms (eas-bench-calibrate)
          :size (list :w (car eas-bench-size) :h (cdr eas-bench-size)
                      :cols (plist-get eas-bench-text-size :cols) :rows (plist-get eas-bench-text-size :rows))
          :ladder (vconcat (mapcar (lambda (n) (eas-bench-rung n reps)) (or points eas-bench-points))))))

;;; Budget

(defun eas-bench-read-budget (&optional file)
  "The budget in FILE (default `eas-bench-budget-file')."
  (eas-json-read-file (or file eas-bench-budget-file)))

(defun eas-bench--reference (budget n)
  "BUDGET's reference stages for N points, or nil."
  (plist-get (seq-find (lambda (r) (equal (plist-get r :points) n)) (plist-get budget :reference)) :stages))

(defun eas-bench-factor (result budget)
  "Return how much slower RESULT's machine is than BUDGET's, clamped.
This is the ratio of their calibration times."
  (min 8.0 (max 0.5 (/ (float (plist-get result :calibration-ms)) (plist-get budget :calibration-ms)))))

(defun eas-bench-check (result budget)
  "Compare ladder RESULT with BUDGET; return the verdict plist.
:status is pass, fail or skipped; :violations name each stage whose mean
passed its limit; :targets says which absolute targets were met."
  (let* ((like (eq (eq (plist-get result :compiled) t) (eq (plist-get budget :compiled) t)))
         (factor (eas-scene-round (eas-bench-factor result budget)))
         (tolerance (plist-get budget :tolerance)) (floor-ms (plist-get budget :floor-ms))
         (violations nil) (checked 0))
    (when like
      (seq-doseq (rung (plist-get result :ladder))
        (let ((ref (eas-bench--reference budget (plist-get rung :points))))
          (cl-loop for (key ms) on ref by #'cddr
                   for measured = (plist-get (plist-get rung key) :mean)
                   for limit = (eas-scene-round (max (* ms tolerance factor) (* floor-ms factor)))
                   when measured do (cl-incf checked)
                   when (and measured (> measured limit))
                   do (push (list :points (plist-get rung :points) :stage (eas-key-name key)
                                  :ms measured :limit limit :reference ms)
                            violations)))))
    (list :status (cond ((not like) "skipped") (violations "fail") (t "pass"))
          :reason (if like :null
                    (format "the budget was measured %s; this run is %s"
                            (if (eq (plist-get budget :compiled) t) "byte-compiled" "interpreted")
                            (if (eq (plist-get result :compiled) t) "byte-compiled" "interpreted")))
          :factor factor :tolerance tolerance :floor-ms floor-ms :checked checked
          :violations (vconcat (nreverse violations))
          :targets (vconcat
                    (seq-map (lambda (target)
                               (let* ((rung (seq-find (lambda (r) (equal (plist-get r :points) (plist-get target :points)))
                                                      (plist-get result :ladder)))
                                      (ms (and rung (plist-get (plist-get rung (eas-key (plist-get target :stage))) :mean))))
                                 (append target (list :measured (or ms :null)
                                                      :met (cond ((null ms) :null) ((<= ms (plist-get target :ms)) t)
                                                                 (t :false))))))
                             (plist-get budget :targets))))))

(defun eas-bench-budget-from (result budget)
  "BUDGET's tolerances and targets with references from ladder RESULT."
  (list :contract "eas-bench-budget/v1"
        :note (or (plist-get budget :note) :null)
        :measured (list :date (format-time-string "%F" nil t) :emacs (plist-get result :emacs)
                        :system (plist-get result :system) :gc (plist-get result :gc))
        :compiled (plist-get result :compiled)
        :calibration-ms (plist-get result :calibration-ms)
        :tolerance (plist-get budget :tolerance) :floor-ms (plist-get budget :floor-ms)
        :targets (plist-get budget :targets)
        :reference (vconcat (seq-map (lambda (rung)
                                       (list :points (plist-get rung :points)
                                             :stages (cl-loop for s in eas-bench-stages
                                                              for k = (intern (concat ":" s))
                                                              when (plist-get rung k)
                                                              append (list k (plist-get (plist-get rung k) :mean)))))
                                     (plist-get result :ladder)))))

(provide 'eas-bench)
;;; eas-bench.el ends here
