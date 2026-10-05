;;; dispatch-cost.el --- spike: hover latency through the real runtime -*- lexical-binding: t; -*-

;; Usage: scripts/easel-spikes/run-compiled.sh scripts/easel-spikes/dispatch-cost.el
;; For N in (1000 10000 100000), two crosshair specs at 800x400:
;;   cond    a rule per datum whose opacity is a condition on the param
;;   filter  the Vega-Lite idiom: a rule layer filtered by the param
;; Measures, mean ms:
;;   compile    easel-compile of the whole scene
;;   hover      easel-dispatch pointermove that moves the crosshair
;;              (reduce + hit-test + recompile)
;;   svg        easel-svg-render of the resulting scene (Lisp only)
;;   text       easel-text-render at 100x30 cells
;; librsvg rasterization and redisplay are not included (no GUI here).

(require 'easel)
(require 'benchmark)

(defun spike--spec (n &optional filter)
  (if filter (spike--filter-spec n)
  (list :data (list :values (vconcat (mapcar (lambda (i) (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))))))
                                             (number-sequence 0 (1- n)))))
        :layer (vector (list :mark "line" :encoding '(:x (:field "t" :type "quantitative")
                                                      :y (:field "p" :type "quantitative" :scale (:zero :false))))
                       (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                             :mark "rule"
                             :encoding '(:x (:field "t" :type "quantitative")
                                         :opacity (:condition (:param "hover" :empty :false :value 1) :value 0)))))))

(defun spike--filter-spec (n)
  (list :data (list :values (vconcat (mapcar (lambda (i) (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))))))
                                             (number-sequence 0 (1- n)))))
        :layer (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                             :mark "line" :encoding '(:x (:field "t" :type "quantitative")
                                                      :y (:field "p" :type "quantitative" :scale (:zero :false))))
                       (list :transform [(:filter (:param "hover" :empty :false))]
                             :mark "rule" :encoding '(:x (:field "t" :type "quantitative"))))))

(defun spike--ms (reps fn)
  (funcall fn) (garbage-collect)
  (/ (* 1000.0 (car (benchmark-call fn reps))) reps))

(dolist (variant '(nil t))
  (dolist (n '(1000 10000 100000))
    (let* ((reps (if (= n 100000) 3 10))
           (spec (spike--spec n variant))
           (easel-views (make-hash-table :test 'equal))
           (view (easel-view-open spec :id "spike" :size '(800 . 400)))
           (k 0))
      (princ (format "%-6s n=%-6d compile=%7.1fms hover=%7.1fms svg=%6.1fms text=%7.1fms\n"
                     (if variant "filter" "cond") n
                     (spike--ms reps (lambda () (easel-compile spec :size '(800 . 400))))
                     (spike--ms reps (lambda () (setq k (1+ k))
                                       (easel-dispatch view (list :type "pointermove" :px (vector (+ 100 (* 37 k)) 200)))))
                     (spike--ms reps (lambda () (easel-svg-render (easel-view-scene view))))
                     (spike--ms reps (lambda () (easel-text-render (easel-compile spec :target 'text :size '(:cols 100 :rows 30))))))))))
