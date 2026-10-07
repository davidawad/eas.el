;;; eas-update-test.el --- the update path: patches equal full compiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.3.  Every incremental frame (a hover patched with values
;; outside an explicit domain, transform prefixes reused within and
;; across compiles) must draw what a full compile from nothing draws.
;; Random push, param, hover and tick sequences check that.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-compile)
(require 'eas-compile-memo)
(require 'eas-compile-patch)
(require 'eas-text)
(require 'eas-template)
(require 'eas-transform)
(require 'eas-transform-index)
(require 'eas-expr)

(defun eas-update-test--full (view)
  "The scene a full compile from nothing makes of VIEW: no kept runs."
  (let ((state (eas-view-state view))
        (eas-compile-memo--frames (make-hash-table :test 'eq :weakness 'key)))
    (eas-params-with-state state
      (eas-compile-scene (eas-compile-plan (eas-view-spec view) :rows (plist-get (eas-view-data view) :rows)
                                           :size (eas-view-size view) :target (eas-view-target view)
                                           :state state)
                         state))))

(defvar eas-update-test--last (make-hash-table :test 'eq)
  "View -> the scene `eas-update-test--same' saw last.")

(defun eas-update-test--dirty-sound (view)
  "Assert VIEW's dirty marks name every mark that differs from the last frame."
  (let ((old (gethash view eas-update-test--last)) (new (eas-view-scene view))
        (dirty (eas-view-dirty view)))
    (when (and old (not (eq dirty t)))
      (should (= (length (plist-get old :views)) (length (plist-get new :views))))
      (cl-loop for o across (plist-get old :views) for n across (plist-get new :views)
               for listed = (cdr (assoc (plist-get n :id) dirty))
               do (should (equal (plist-get o :axes) (plist-get n :axes)))
               (cl-loop for a across (plist-get o :marks) for b across (plist-get n :marks)
                        unless (member (plist-get b :id) listed)
                        do (should (eq a b)))))
    (puthash view new eas-update-test--last)))

(defun eas-update-test--same (view)
  "Assert VIEW's scene (and its text) equal a full compile's.
Also assert its dirty marks are sound (`eas-update-test--dirty-sound')."
  (let ((full (eas-update-test--full view)))
    (eas-update-test--dirty-sound view)
    (should (equal (eas-view-scene view) full))
    (when (eq (eas-view-target view) 'text)
      (should (equal (eas-text-render (eas-view-scene view)) (eas-text-render full))))))

(defmacro eas-update-test--with (&rest body)
  "Run BODY with a fresh view registry and kept-run table."
  (declare (indent 0) (debug t))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-update-test--last (make-hash-table :test 'eq))
         (eas-plays (make-hash-table :test 'equal))
         (eas-compile-memo--frames (make-hash-table :test 'eq :weakness 'key)))
     ,@body))

(defun eas-update-test--next (seed)
  "The pseudo-random successor of SEED."
  (% (+ (* seed 1103515245) 12345) 2147483648))

(defun eas-update-test--rows (seed n)
  "N rows with ids, x/y in [0, 100] and targets tx/ty in [-60, 160]."
  (let ((s seed))
    (vconcat (cl-loop for i below n
                      collect (list :id (format "r%d" i)
                                    :x (% (setq s (eas-update-test--next s)) 101)
                                    :y (% (setq s (eas-update-test--next s)) 101)
                                    :tx (- (% (setq s (eas-update-test--next s)) 221) 60)
                                    :ty (- (% (setq s (eas-update-test--next s)) 221) 60)
                                    :size (% (setq s (eas-update-test--next s)) 30))))))

(defun eas-update-test--spoke-spec (explicit &optional autosize-none)
  "Points with a hover selection and rules from the hovered point.
EXPLICIT gives x and y the domain [0, 100]; rules reach outside it.
AUTOSIZE-NONE fixes the canvas as airport-connections does."
  (let ((scale (if explicit (list :domain [0 100] :nice :false :zero :false) (list :zero :false))))
    (append
     (list :width 300 :height 200)
     (and autosize-none (list :autosize "none" :padding 0))
     (list :layer
           (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t
                                                            :fields ["id"]))]
                         :mark "circle"
                         :encoding (list :x (list :field "x" :type "quantitative" :scale scale)
                                         :y (list :field "y" :type "quantitative" :scale scale)
                                         :color (list :condition (list :param "hover" :value "red" :empty :false)
                                                      :value "steelblue")))
                   (list :transform [(:filter (:param "hover" :empty :false))]
                         :mark "rule"
                         :encoding (list :x (list :field "x" :type "quantitative" :scale scale)
                                         :y (list :field "y" :type "quantitative" :scale scale)
                                         :x2 (list :field "tx") :y2 (list :field "ty"))))))))

(ert-deftest eas-update-hover-outside-explicit-domain-patches ()
  "Hovering points whose rules leave an explicit domain patches, not compiles.
Over random pointer sweeps the patched scene equals a full compile, for
both targets, sized and autosize none; a data domain still compiles."
  (eas-update-test--with
    (let* ((plans 0) (made 0) (count (lambda (&rest _) (cl-incf plans))))
      (advice-add 'eas-compile-plan :before count)
      (unwind-protect
          (cl-loop
           for (explicit autosize-none size target) in
           '((t nil (400 . 300) svg) (t t nil svg) (t nil (:cols 80 :rows 30) text)
             (nil nil (400 . 300) svg) (nil nil (:cols 80 :rows 30) text))
           for k from 0 do
           (let* ((view (eas-view-open (eas-json-encode (eas-update-test--spoke-spec explicit autosize-none))
                                       :id (format "spokes%d" k) :size size :target target
                                       :rows (eas-update-test--rows (+ 3 k) 25)))
                  (scene-size (plist-get (eas-view-scene view) :size))
                  (w (plist-get scene-size :w)) (h (plist-get scene-size :h))
                  (seed (+ 11 k)))
             (setq made 0)
             (dotimes (_ 12)
               (setq seed (eas-update-test--next seed))
               (let ((before plans))
                 (eas-dispatch view (list :type "pointermove"
                                          :px (vector (* w (/ (% seed 97) 97.0))
                                                      (* h (/ (% (/ seed 97) 89) 89.0)))))
                 (cl-incf made (- plans before)))
               (eas-update-test--same view))
             ;; Explicit domains never need a full compile on hover.
             (when explicit (should (equal (list k made) (list k 0))))
             (eas-view-close view)))
        (advice-remove 'eas-compile-plan count)))))

(ert-deftest eas-update-hover-outside-explicit-domain-plan-count ()
  "A hover whose rules leave an explicit domain makes no new plan."
  (eas-update-test--with
    (let* ((view (eas-view-open (eas-json-encode (eas-update-test--spoke-spec t t)) :id "spokes"
                                :rows (eas-update-test--rows 5 25)))
           (scene-size (plist-get (eas-view-scene view) :size))
           (plans 0) (count (lambda (&rest _) (cl-incf plans))))
      (advice-add 'eas-compile-plan :before count)
      (unwind-protect
          (dotimes (i 20)
            (eas-dispatch view (list :type "pointermove"
                                     :px (vector (* (plist-get scene-size :w) (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                 (* (plist-get scene-size :h) (/ (+ 0.5 (% (* 3 i) 10)) 10.0))))))
        (advice-remove 'eas-compile-plan count))
      ;; Each patch builds its probe plan's scales, never a plan.
      (should (= plans 0)))))

(defconst eas-update-test--slider
  (list :data (list :sequence (list :start 1 :stop 301 :as "n"))
        :params [(:name "cut" :value 150 :bind (:input "range" :min 10 :max 300 :step 1))]
        :transform [(:calculate "[random(), random()]" :as "xy")
                    (:calculate "datum.xy[0]" :as "x")
                    (:calculate "datum.xy[1]" :as "y")
                    (:filter "datum.n <= cut")]
        :hconcat [(:width 150 :height 150 :mark "point"
                          :encoding (:x (:field "x" :type "quantitative" :scale (:domain [0 1]))
                                     :y (:field "y" :type "quantitative" :scale (:domain [0 1]))
                                     :color (:condition (:test "datum.x * datum.x + datum.y * datum.y <= 1"
                                                               :value "blue")
                                                        :value "orange")))
                  (:width 150 :height 150
                          :transform [(:calculate "datum.x * datum.x + datum.y * datum.y < 1 ? 1 : 0" :as "inside")
                                      (:window [(:op "sum" :field "inside" :as "hits")] :sort [(:field "n")])
                                      (:calculate "4 * datum.hits / datum.n" :as "estimate")]
                          :layer [(:mark "line" :encoding (:x (:field "n" :type "quantitative")
                                                           :y (:field "estimate" :type "quantitative"
                                                                  :scale (:zero :false))))
                                  (:transform [(:filter "datum.n == cut")]
                                              :mark "text"
                                              :encoding (:x (:field "n" :type "quantitative")
                                                         :y (:field "estimate" :type "quantitative")
                                                         :text (:field "estimate" :type "quantitative")))])])
  "pi-monte-carlo in small: random calculates, then a filter on a slider.")

(ert-deftest eas-update-slider-reuses-transform-prefixes ()
  "Moving a slider reruns only the transforms after the ones it reads.
Over random slider values the scene equals a full compile, and the
random calculates before the filter run once per data vector."
  (eas-update-test--with
    (dolist (target '(svg text))
      (let* ((eas-compile-memo-stats (list :hits 0 :frame-hits 0 :steps 0))
             (view (eas-view-open (eas-json-encode eas-update-test--slider) :id "slider" :target target
                                  :size (if (eq target 'text) '(:cols 90 :rows 30) nil)))
             (seed 5))
        (dotimes (_ 8)
          (setq seed (eas-update-test--next seed))
          (eas-dispatch view (list :type "param" :param "cut" :value (+ 10 (% seed 291))))
          (eas-update-test--same view))
        (should (> (plist-get eas-compile-memo-stats :frame-hits) 4))
        (eas-view-close view)))))

(ert-deftest eas-update-memo-prefix-runs-equal-plain-runs ()
  "Stepwise, prefix-shared transform runs give what a plain run gives."
  (let* ((rows (eas-compile--tag (eas-update-test--rows 9 40)))
         (base [(:calculate "datum.x + datum.y" :as "s")
                (:filter "datum.s > lim")])
         (longer (vconcat base [(:window [(:op "rank" :as "r")] :sort [(:field "s")])]))
         (other (vconcat base [(:aggregate [(:op "sum" :field "s" :as "t")] :groupby ["size"])])))
    (eas-compile-memo-source rows rows)
    (dolist (lim '(20 80 150))
      (let ((env (list :lim lim)))
        (eas-compile-memo
         (dolist (tr (list longer base other longer))
           (should (equal (eas-compile-memo-transform-run tr rows env "/transform")
                          (eas-transform-run tr rows env "/transform")))))))))

(ert-deftest eas-update-random-push-param-hover-sequences ()
  "Random mixes of keyed pushes, slider moves and hovers equal full compiles."
  (eas-update-test--with
    (let ((spec (list :width 300 :height 200
                      :params [(:name "cut" :value 15 :bind (:input "range" :min 0 :max 30))]
                      :transform [(:calculate "datum.x * 2" :as "x2") (:filter "datum.size <= cut")]
                      :layer (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove"
                                                                                   :nearest t :fields ["id"]))]
                                           :mark "point"
                                           :encoding (list :x (list :field "x" :type "quantitative"
                                                                    :scale (list :domain [0 100]))
                                                           :y (list :field "y" :type "quantitative"
                                                                    :scale (list :domain [0 100]))
                                                           :size (list :condition (list :param "hover" :value 200
                                                                                        :empty :false)
                                                                       :value 30)))
                                     (list :transform [(:filter (:param "hover" :empty :false))]
                                           :mark "rule"
                                           :encoding (list :x (list :field "x" :type "quantitative"
                                                                    :scale (list :domain [0 100]))
                                                           :y (list :field "y" :type "quantitative"
                                                                    :scale (list :domain [0 100]))
                                                           :x2 (list :field "tx") :y2 (list :field "ty")))))))
      (dolist (target '(svg text))
        (let* ((view (eas-view-open (eas-json-encode spec) :id "mix" :target target
                                    :size (if (eq target 'text) '(:cols 80 :rows 30) '(400 . 300))
                                    :rows (eas-update-test--rows 2 20)))
               (size (plist-get (eas-view-scene view) :size))
               (seed (if (eq target 'text) 77 41)))
          (dotimes (_ 25)
            (setq seed (eas-update-test--next seed))
            (pcase (% seed 3)
              (0 (eas-dispatch view (list :type "push" :key "id"
                                          :rows (seq-take (eas-update-test--rows seed (+ 2 (% seed 25))) (1+ (% seed 6))))))
              (1 (eas-dispatch view (list :type "param" :param "cut" :value (% (/ seed 3) 31))))
              (_ (eas-dispatch view (list :type "pointermove"
                                          :px (vector (* (plist-get size :w) (/ (% (/ seed 3) 97) 97.0))
                                                      (* (plist-get size :h) (/ (% (/ seed 7) 89) 89.0)))))))
            (eas-update-test--same view))
          (eas-view-close view))))))

(ert-deftest eas-update-clock-ticks-equal-full-compiles ()
  "Timer ticks of the clock template draw what a full compile draws."
  (eas-update-test--with
    (let* ((now 1.7e9) (eas-play-clock (lambda () now)))
      (dolist (target '(svg text))
        (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target target
                                   :size (and (eq target 'text) '(:cols 60 :rows 30)))))
          (dotimes (_ 3)
            (setq now (+ now 61.0))
            (eas-play-tick view now)
            (eas-update-test--same view))
          (eas-play-detach view)
          (eas-view-close view))))))

(ert-deftest eas-update-dirty-marks-name-what-changed ()
  "A hover that patches one layer leaves the others' marks as they were.
The scene's untouched marks are the last frame's (eq) and absent from
`eas-view-dirty'; an unchanged frame is nil, an open is t."
  (eas-update-test--with
    (let* ((spec (list :width 300 :height 200
                       :layer (vector (list :mark "rule" :encoding (list :y (list :datum 50 :type "quantitative"
                                                                                  :scale (list :domain [0 100]))))
                                      (list :params [(:name "hover" :select (:type "point" :on "pointermove"
                                                                                    :nearest t :fields ["id"]))]
                                            :mark "circle"
                                            :encoding (list :x (list :field "x" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :y (list :field "y" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :color (list :condition (list :param "hover" :value "red"
                                                                                          :empty :false)
                                                                         :value "steelblue"))))))
           (view (eas-view-open (eas-json-encode spec) :id "dirty" :size '(400 . 300)
                                :rows (eas-update-test--rows 12 25)))
           (size (plist-get (eas-view-scene view) :size)) (seen nil))
      (should (eq (eas-view-dirty view) t))
      (dotimes (i 10)
        (eas-dispatch view (list :type "pointermove"
                                 :px (vector (* (plist-get size :w) (/ (+ 0.5 (% (* 7 i) 10)) 10.0))
                                             (* (plist-get size :h) 0.5))))
        (eas-update-test--same view)
        (let ((dirty (eas-view-dirty view)))
          (when (consp dirty)
            (push dirty seen)
            ;; The rule never changes.
            (should-not (member "main/0" (cdr (assoc "main" dirty)))))))
      (should seen))))

(ert-deftest eas-update-literal-domain-stream-keeps-scales ()
  "Pushes into a view whose every domain is literal build no scales.
Random keyed pushes, sizes past the domain included, still equal a
full compile."
  (eas-update-test--with
    (let ((spec (list :width 300 :height 200
                      :transform [(:window [(:op "sum" :field "size" :as "depth")]
                                           :sort [(:field "x")] :groupby ["side"])]
                      :mark "area"
                      :encoding (list :x (list :field "x" :type "quantitative" :scale (list :domain [0 100]))
                                      :y (list :field "depth" :type "quantitative" :scale (list :domain [0 400]))
                                      :color (list :field "side" :type "nominal"
                                                   :scale (list :domain ["a" "b"])))))
          (scales 0))
      (cl-flet ((rows (seed n) (vconcat (seq-map-indexed (lambda (r i) (append (list :side (if (cl-oddp i) "a" "b")) r))
                                                         (eas-update-test--rows seed n)))))
        (dolist (target '(svg text))
          (let ((view (eas-view-open (eas-json-encode spec) :id "fixed" :target target
                                     :size (if (eq target 'text) '(:cols 80 :rows 30) '(400 . 300))
                                     :rows (rows 3 20)))
                (seed 7) (count (lambda (&rest _) (cl-incf scales))))
            (advice-add 'eas-compile--scales :before count)
            (unwind-protect
                (dotimes (_ 8)
                  (setq seed (eas-update-test--next seed))
                  (eas-dispatch view (list :type "push" :key "id" :rows (seq-take (rows seed 20) (1+ (% seed 5)))))
                  (eas-update-test--same view))
              (advice-remove 'eas-compile--scales count))
            (eas-view-close view))))
      ;; Only the full compiles the test makes to compare against.
      (should (= scales 16)))))

;;; Compiled expressions, indexed filters, running windows

(defconst eas-update-test--exprs
  '("datum.x + datum.y * 2" "datum.x / (datum.y - 50)" "datum.x % 7 - -datum.y"
    "datum.id + '-' + datum.x" "datum.x > 50 ? 'hi' : datum.x < 20 ? 'lo' : null"
    "!datum.missing && datum.x >= cut" "datum.x == cut || datum['y'] === 3"
    "datum.x != datum.y" "sqrt(datum.x * datum.x + datum.y * datum.y) <= 100"
    "[datum.x, datum.y][1] + [1, 2].length" "datum.id.length + datum.id[0]"
    "{a: datum.x, b: 'k'}.a" "random() + random()" "PI * cut + E" "+datum.id"
    "format(datum.x, ',.2f')" "test(regexp('r1'), datum.id) ? 1 : 0" "datum.missing.deeper")
  "Expressions the compiled and the interpreted evaluator must agree on.")

(ert-deftest eas-update-compiled-expressions-equal-interpreted ()
  "`eas-expr-function' gives what `eas-expr-eval' gives, errors too."
  (let ((rows (eas-compile--tag (eas-update-test--rows 4 30))))
    (dolist (expr (append eas-update-test--exprs '("nosuchname + 1")))
      (seq-doseq (row rows)
        (dolist (env '((:cut 40) (:cut 40.5) nil))
          (let ((interpreted (condition-case err
                                 (let ((eas-expr--random-calls 'fresh))
                                   (eas-expr-eval (eas-expr-parse expr) row env))
                               (error (list 'error (car err)))))
                (compiled (condition-case err
                              (let ((eas-expr--random-calls 'fresh))
                                (funcall (eas-expr-function expr) row env))
                            (error (list 'error (car err))))))
            (should (equal (list expr compiled) (list expr interpreted)))))))))

(ert-deftest eas-update-indexed-filters-equal-row-filters ()
  "Comparisons of a field with a param answered from an index equal row tests.
Monotone, shuffled, duplicate, float and non-numeric fields; every
operator, both sides, params inside, between and outside the values."
  (let ((eas-transform-index-min-rows 8) (seed 3))
    (dolist (shape '(monotone shuffled dups mixed))
      (let ((rows (vconcat (cl-loop for i below 60
                                    collect (list :n (pcase shape
                                                       ('monotone (+ i (if (cl-oddp i) 0.5 0)))
                                                       ('shuffled (% (* i 37) 61))
                                                       ('dups (/ (% (* i 13) 60) 6))
                                                       (_ (if (= i 30) "x" i)))
                                                  :_eas_row i)))))
        (dolist (op '("<" "<=" ">" ">=" "==" "==="))
          (dolist (x '(-1 0 10 10.5 29 30 59 60 1000))
            (setq seed (eas-update-test--next seed))
            (let* ((expr (if (cl-oddp seed) (format "datum.n %s lim" op) (format "lim %s datum.n" op)))
                   (env (list :lim x))
                   (plain (vconcat (seq-filter (lambda (r) (eas-transform-predicate expr r env)) rows))))
              (should (equal (list expr x (vconcat (eas-transform--filter expr rows env)))
                             (list expr x plain))))))))))

(ert-deftest eas-update-running-windows-equal-framed-windows ()
  "Windows framed from the first row (run as one pass) equal bounded ones."
  (let ((rows (eas-compile--tag (eas-update-test--rows 6 50))))
    (dolist (op '("count" "valid" "missing" "sum" "mean"))
      (dolist (sort '([(:field "x")] [(:field "size")] []))
        (dolist (hi '(0 2 :null))
          (let ((tr (lambda (lo) (vector (list :window (vector (list :op op :field "ty" :as "w"))
                                               :frame (vector lo hi) :sort sort :groupby ["size"])))))
            ;; A bound below every row frames from the first row too.
            (should (equal (eas-transform-run (funcall tr :null) rows)
                           (eas-transform-run (funcall tr -1000) rows)))))))))

;;; Probe skipped for literal domains

(ert-deftest eas-update-param-filter-with-literal-and-data-domains ()
  "A param filter patches without the scale probe only when no domain can move.
Random slider values over a layer whose colours follow the data (probe
needed) and one whose scales are all literal (probe skipped)."
  (eas-update-test--with
    (let* ((spec (list :width 300 :height 200
                       :params [(:name "cut" :value 15 :bind (:input "range" :min 0 :max 30))]
                       :layer (vector (list :transform [(:filter "datum.size <= cut")
                                                        (:calculate "datum.size % 3 == 0 ? 'a' : 'b'" :as "g")]
                                            :mark "point"
                                            :encoding (list :x (list :field "x" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :y (list :field "y" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :color (list :field "g" :type "nominal")))
                                      (list :transform [(:filter "datum.size > cut")]
                                            :mark "square"
                                            :encoding (list :x (list :field "tx" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :y (list :field "y" :type "quantitative"
                                                                     :scale (list :domain [0 100]))
                                                            :text (list :field "id")
                                                            :size (list :field "size" :type "quantitative"
                                                                        :scale (list :domain [0 30])))))))
           (probes 0) (count (lambda (&rest _) (cl-incf probes))))
      (advice-add 'eas-patch--same-scales-p :before count)
      (unwind-protect
          (dolist (target '(svg text))
            (let ((view (eas-view-open (eas-json-encode spec) :id "lit" :target target
                                       :size (if (eq target 'text) '(:cols 80 :rows 30) '(400 . 300))
                                       :rows (eas-update-test--rows 8 30)))
                  (seed (if (eq target 'text) 5 9)))
              (dotimes (_ 12)
                (setq seed (eas-update-test--next seed))
                (eas-dispatch view (list :type "param" :param "cut" :value (% (/ seed 5) 31)))
                (eas-update-test--same view))
              (eas-view-close view)))
        (advice-remove 'eas-patch--same-scales-p count))
      ;; The data-coloured layer needed the probe.
      (should (> probes 0))
      (should (eas-patch--fixed-scales-p
               (list :encoding '(:x (:field "x" :scale (:domain [0 1])) :text (:field "t") :color (:value "red")))
               (list :encoding '(:x (:field "x" :scale (:domain [0 1])) :text (:field "t") :color (:value "red")))))
      (should-not (eas-patch--fixed-scales-p
                   (list :encoding '(:x (:field "x" :scale (:domain [0 1])) :color (:field "g")))
                   (list :encoding '(:x (:field "x" :scale (:domain [0 1])) :color (:field "g"))))))))

;;; Templates and streams

(ert-deftest eas-update-template-sequences-equal-full-compiles ()
  "Pacman ticks and pi-monte-carlo slider steps (up, down, repeated,
past the refused-patch skip) draw what a full compile draws."
  (eas-update-test--with
    (let* ((now 1.7e9) (eas-play-clock (lambda () now)))
      (dolist (target '(svg text))
        (let ((view (eas-play-open "pacman" :bindings (eas-template-example "pacman") :target target
                                   :size (and (eq target 'text) '(:cols 60 :rows 30)))))
          (dotimes (_ 4)
            (setq now (+ now 1.0))
            (eas-play-tick view now)
            (eas-update-test--same view))
          (eas-play-detach view)
          (eas-view-close view))
        (let ((view (eas-view-open "pi-monte-carlo"
                                   :bindings (plist-put (plist-put (eas-template-example "pi-monte-carlo")
                                                                   :points 300)
                                                        :max_points 1200)
                                   :target target :size (and (eq target 'text) '(:cols 100 :rows 40))))
              (seed 13))
          (dotimes (_ 12)
            (setq seed (eas-update-test--next seed))
            (eas-dispatch view (list :type "param" :param "num_points"
                                     :value (if (zerop (% seed 4)) 300 (+ 10 (% (/ seed 3) 1190)))))
            (eas-update-test--same view))
          (eas-view-close view))))))

(defun eas-update-test--book (seed levels)
  "A LEVELS-per-side order book drawn from SEED."
  (let ((s seed))
    (vconcat (cl-loop for side in '("ask" "bid")
                      append (cl-loop for i below levels
                                      collect (list :id (format "%s%d" side i) :side side
                                                    :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                             (- 99.99 (* 0.01 i)))
                                                    :size (+ 1 (% (setq s (eas-update-test--next s)) 40))))))))

(ert-deftest eas-update-random-ladder-and-candle-pushes ()
  "A 2x50 ladder and a windowed candle stream with a moving average:
random keyed pushes (updates, new candles, sizes past the domain) draw
what a full compile draws."
  (eas-update-test--with
    (let ((ladder (list :width 400 :height 300
                        :encoding (list :y (list :field "price" :type "ordinal" :sort "descending"))
                        :layer (vector (list :mark "bar"
                                             :encoding (list :x (list :field "size" :type "quantitative"
                                                                      :scale (list :domain [0 40]))
                                                             :color (list :field "side" :type "nominal")))
                                       (list :mark (list :type "text" :align "left")
                                             :encoding (list :x (list :field "size" :type "quantitative")
                                                             :text (list :field "size" :type "quantitative"))))))
          (candles (list :width 300 :height 200
                         :transform [(:window [(:op "mean" :field "close" :as "sma")] :frame [-4 0]
                                              :sort [(:field "t")])
                                     (:window [(:op "sum" :field "close" :as "cum")] :sort [(:field "t")])]
                         :encoding (list :x (list :field "t" :type "quantitative" :scale (list :zero :false)))
                         :layer (vector (list :mark "rule"
                                              :encoding (list :y (list :field "low" :type "quantitative"
                                                                       :scale (list :domain [80 120]))
                                                              :y2 (list :field "high")))
                                        (list :mark "line" :encoding (list :y (list :field "sma"
                                                                                    :type "quantitative")))))))
      (dolist (target '(svg text))
        (let ((view (eas-view-open (eas-json-encode ladder) :id "ladder" :target target
                                   :size (if (eq target 'text) '(:cols 100 :rows 40) '(640 . 360))
                                   :rows (eas-update-test--book 1 50)))
              (seed 21))
          (dotimes (i 8)
            (setq seed (eas-update-test--next seed))
            (let ((fresh (eas-update-test--book seed 50)))
              (eas-dispatch view (list :type "push" :key "id"
                                       :rows (vconcat (cl-loop for k below 20
                                                               collect (aref fresh (% (+ seed (* k 7) i) 100)))))))
            (eas-update-test--same view))
          (eas-view-close view))
        (let* ((candle (lambda (s t0)
                         (let ((o (+ 90 (% s 20))) (c (+ 90 (% (/ s 20) 20))))
                           (list :t t0 :close c :high (+ (max o c) 2) :low (- (min o c) (% s 3))))))
               (view (eas-view-open (eas-json-encode candles) :id "candles" :target target
                                    :size (if (eq target 'text) '(:cols 100 :rows 40) '(640 . 360))
                                    :rows (vconcat (cl-loop for i below 30 collect (funcall candle (* 7 i) i)))))
               (seed 4) (last 29))
          (dotimes (_ 10)
            (setq seed (eas-update-test--next seed))
            (when (zerop (% seed 3)) (cl-incf last))
            (eas-dispatch view (list :type "push" :key "t" :window 30
                                     :rows (vector (funcall candle (/ seed 11) last))))
            (eas-update-test--same view))
          (eas-view-close view))))))

(provide 'eas-update-test)
;;; eas-update-test.el ends here
