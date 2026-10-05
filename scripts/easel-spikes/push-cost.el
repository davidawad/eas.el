;;; push-cost.el --- spike: cost of one live-data frame -*- lexical-binding: t; -*-

;; Usage: scripts/easel-spikes/run-compiled.sh scripts/easel-spikes/push-cost.el
;; A line with a pointermove crosshair (the Vega-Lite filter idiom) at
;; 800x400, holding N rows in a full window.  One frame is an
;; easel-dispatch of a windowed push of K new rows: append, trim,
;; reduce and a full recompile (data changed, so no patching).  Mean
;; ms of the frame, plus the SVG serialize and the 100x30 text compile
;; and render that follow it.  Redisplay and librsvg are not included.

(require 'easel)
(require 'benchmark)

(defun spike--row (i) (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))))))

(defun spike--spec (n)
  (list :data (list :values (vconcat (mapcar #'spike--row (number-sequence 0 (1- n)))))
        :layer (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                             :mark "line" :encoding '(:x (:field "t" :type "quantitative")
                                                      :y (:field "p" :type "quantitative" :scale (:zero :false))))
                       (list :transform [(:filter (:param "hover" :empty :false))]
                             :mark "rule" :encoding '(:x (:field "t" :type "quantitative"))))))

(defun spike--ms (reps fn)
  (funcall fn) (garbage-collect)
  (/ (* 1000.0 (car (benchmark-call fn reps))) reps))

(let ((easel-stream-use-timers nil))
  (princ "| N | K | push frame | SVG serialize | text compile+render |\n|---|---|---|---|---|\n")
  (dolist (n '(1000 10000))
    (dolist (k '(1 50))
      (let* ((view (easel-view-open (spike--spec n) :size '(800 . 400)))
             (text (easel-view-open (spike--spec n) :target 'text :size '(:cols 100 :rows 30)))
             (next n)
             (frame (lambda (v) (let ((rows (vconcat (mapcar #'spike--row (number-sequence next (+ next k -1))))))
                                  (setq next (+ next k))
                                  (easel-dispatch v (list :type "push" :rows rows :window n)))))
             (reps (if (> n 5000) 10 40))
             (push-ms (spike--ms reps (lambda () (funcall frame view))))
             (svg-ms (spike--ms reps (lambda () (easel-svg-render (easel-view-scene view)))))
             (text-ms (spike--ms reps (lambda () (funcall frame text) (easel-text-render (easel-view-scene text))))))
        (princ (format "| %d | %d | %.1f | %.1f | %.1f |\n" n k push-ms svg-ms text-ms))))))
