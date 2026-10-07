;;; eas-vega-interaction-test.el --- Vega gallery: interaction, animation, games -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The templates of bead eas-7r1.9 (templates/vega/NAME.json with
;; examples/vega/NAME.data.json) and the runtime they need: x-eas.timer
;; and x-eas.on (eas-play.el), and the "params" event.  Every
;; interaction runs headlessly through `eas-dispatch', `eas-play-tick'
;; and `eas-play-key'.  The :gallery test renders each example and
;; compares it with test/vega-examples/ref/NAME.png; it needs
;; rsvg-convert and skips, with the reason, without it.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-play)
(require 'eas-png)

(defconst eas-vega-interaction-names
  '("crossfilter-flights" "overview-plus-detail" "brushing-scatter-plots" "zoomable-scatter-plot"
    "zoomable-binned-plot" "global-development" "interactive-legend" "stock-index-chart"
    "pi-monte-carlo" "table-scrollbar" "bar-line-toggle" "pacman" "platformer"
    "hypothetical-outcome-plots" "clock" "watch" "parallel-coordinates-interactive")
  "The Vega gallery examples this bead makes templates of.")

(defconst eas-vega-interaction-max-shift 16
  "Largest alignment offset tried against a reference.
Vega pads its canvas for overhanging labels by more than the
conformance gallery's few pixels.")

(defmacro eas-vega-interaction-with (&rest body)
  "Run BODY with fresh view and play registries, timers off, in UTC."
  (declare (indent 0))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-plays (make-hash-table :test 'equal))
         (eas-time-zone nil))
     ,@body))

(defun eas-vega-interaction-template (name)
  "The template plist of Vega example NAME."
  (eas-template-get (concat "vega/" name)))

(defun eas-vega-interaction-open (name &optional bindings)
  "Open a view of Vega example NAME with BINDINGS (default its example)."
  (let ((template (concat "vega/" name)))
    (eas-view-open template :bindings (or bindings (eas-template-example template)) :id name)))

(defun eas-vega-interaction-param (view name)
  "The value of param NAME in VIEW: its state's, else the spec's."
  (plist-get (eas-compile--env (eas-view-spec view) (eas-view-state view)) (eas-key name)))

(defun eas-vega-interaction-mark (view view-id index)
  "Mark \"VIEW-ID/INDEX\" (the INDEXth flattened layer) of VIEW's scene.
Ids, not positions: an active brush adds a mark in front."
  (let ((v (seq-find (lambda (v) (equal (plist-get v :id) view-id)) (plist-get (eas-view-scene view) :views)))
        (id (format "%s/%d" view-id index)))
    (seq-find (lambda (m) (equal (plist-get m :id) id)) (plist-get v :marks))))

(defun eas-vega-interaction-rows (view view-id index)
  "The rows of mark INDEX of scene view VIEW-ID in VIEW."
  (plist-get (eas-vega-interaction-mark view view-id index) :rows))

(defun eas-vega-interaction-items (view view-id index)
  "How many items mark INDEX of scene view VIEW-ID in VIEW draws."
  (length (plist-get (eas-vega-interaction-mark view view-id index) :items)))

(defun eas-vega-interaction-summaries (view)
  "VIEW's log summaries, oldest first."
  (mapcar (lambda (e) (plist-get e :summary)) (append (eas-view-log-entries view) nil)))

(defun eas-vega-interaction-compare (name)
  "Render Vega example NAME natively and compare it with its reference.
Return `eas-png-compare's plist; the caller skips without rsvg-convert."
  (let* ((eas-time-zone nil)
         (eas-spec-supported-function nil)
         (spec (eas-resolve (concat "vega/" name) (eas-template-example (concat "vega/" name))))
         (png (make-temp-file "eas-vega" nil ".png")))
    (unwind-protect
        (progn (eas-chart-rasterize (eas-svg-render (eas-compile spec)) png)
               (eas-png-compare (eas-png-read png)
                                (eas-png-read (eas-test-file "test/vega-examples/ref" (concat name ".png")))
                                eas-vega-interaction-max-shift))
      (delete-file png))))

;;; Templates

(ert-deftest eas-vega-interaction-templates-declare-their-verdict ()
  "Every example has a vega/NAME template, an example and a recorded verdict."
  (dolist (name eas-vega-interaction-names)
    (let* ((template (eas-vega-interaction-template name))
           (meta (plist-get template :meta))
           (vega (plist-get meta :vega)))
      (should (equal (plist-get template :name) (concat "vega/" name)))
      (should (equal (plist-get template :path) (eas-test-file "templates/vega" (concat name ".json"))))
      (should (file-exists-p (eas-template-example-file template)))
      (should (member (plist-get vega :status) '("pass" "partial" "unsupported")))
      (should (> (length (plist-get vega :note)) 20))
      (should (numberp (plist-get vega :threshold))))))

(ert-deftest eas-vega-interaction-templates-render-natively ()
  "Every example opens as an interactive native view and draws as SVG and text."
  (eas-vega-interaction-with
    (dolist (name eas-vega-interaction-names)
      (let ((view (eas-vega-interaction-open name)))
        (should (eq (eas-view-interactive view) t))
        (should (equal (eas-view-warnings view) nil))
        (should (string-prefix-p "<svg" (eas-svg-render (eas-view-scene view))))
        (should (> (length (string-trim (eas-text-render
                                         (eas-compile (eas-view-spec view) :rows (plist-get (eas-view-data view) :rows)
                                                      :target 'text :size '(:cols 80 :rows 24)))))
                   0))))))

(ert-deftest eas-vega-interaction-templates-take-other-data ()
  "Field slots let a caller bind their own data and column names."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open
                 "overview-plus-detail"
                 '(:data [(:day "2024-01-01" :close 10) (:day "2024-02-01" :close 12) (:day "2024-03-01" :close 9)]
                   :x "day" :y "close"))))
      (should (= (length (eas-vega-interaction-rows view "vconcat_0" 0)) 3)))
    (eas-test-should-code "FIELD_MISSING"
      (eas-vega-interaction-open "interactive-legend" '(:data [(:a 1 :b 2 :c "x")])))))

(ert-deftest eas-vega-interaction-matches-references ()
  "Each example's native rendering is within its recorded threshold of the
Vega reference (needs rsvg-convert)."
  :tags '(:gallery)
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s not on PATH; it rasterizes the native SVG for the reference comparison"
                           eas-chart-rsvg-program)))
  (let (failures)
    (dolist (name eas-vega-interaction-names)
      (let* ((vega (plist-get (plist-get (eas-vega-interaction-template name) :meta) :vega))
             (ratio (plist-get (eas-vega-interaction-compare name) :ratio)))
        (unless (<= ratio (plist-get vega :threshold))
          (push (format "%s: ratio %.4f over threshold %s" name ratio (plist-get vega :threshold)) failures))))
    (should (equal (nreverse failures) nil))))

;;; Interactions on Vega-Lite params

(ert-deftest eas-vega-interaction-crossfilter-brush-filters-the-others ()
  "Brushing one histogram filters the other two but not itself."
  (eas-vega-interaction-with
    (let* ((view (eas-vega-interaction-open "crossfilter-flights"))
           (count (lambda (id) (apply #'+ (mapcar (lambda (r) (plist-get r :count))
                                                  (eas-vega-interaction-rows view id 0)))))
           (before (mapcar count '("vconcat_0" "vconcat_1" "vconcat_2"))))
      (eas-dispatch view '(:type "brush" :param "brush_first" :x [0 20]))
      (let ((after (mapcar count '("vconcat_0" "vconcat_1" "vconcat_2"))))
        (should (= (nth 0 after) (nth 0 before)))
        (should (< (nth 1 after) (nth 1 before)))
        (should (< (nth 2 after) (nth 2 before)))))))

(ert-deftest eas-vega-interaction-overview-brush-zooms-the-detail ()
  "Brushing the overview sets the detail view's x domain."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "overview-plus-detail")))
      (eas-dispatch view '(:type "brush" :param "brush" :x ["2004-01-01" "2005-01-01"]))
      (let ((domain (plist-get (plist-get (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :scales) :x) :domain)))
        (should (equal (format-time-string "%Y-%m-%d" (seconds-to-time (/ (aref domain 0) 1000.0)) t) "2004-01-01"))
        (should (equal (format-time-string "%Y-%m-%d" (seconds-to-time (/ (aref domain 1) 1000.0)) t) "2005-01-01"))))))

(ert-deftest eas-vega-interaction-sliders-and-legend ()
  "Bound inputs and the legend drive global-development, bar-line-toggle and interactive-legend."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "global-development")))
      (eas-dispatch view '(:type "param" :param "year" :value 2000))
      (should (equal (plist-get (aref (eas-vega-interaction-rows view "main" 1) 0) :year) 2000)))
    (let ((view (eas-vega-interaction-open "bar-line-toggle")))
      (should (= (eas-vega-interaction-items view "main" 0) 25))
      (eas-dispatch view '(:type "param" :param "points" :value 120))
      (should (= (eas-vega-interaction-items view "main" 0) 0))
      (should (> (eas-vega-interaction-items view "main" 2) 0)))
    (let ((view (eas-vega-interaction-open "interactive-legend")))
      (eas-dispatch view '(:type "param" :param "origin" :value [(:Origin "Japan")]))
      (let ((items (plist-get (eas-vega-interaction-mark view "main" 0) :items)))
        (should (seq-some (lambda (i) (equal (plist-get i :stroke) "#ccc")) items))
        (should (seq-some (lambda (i) (not (equal (plist-get i :stroke) "#ccc"))) items))))))

(ert-deftest eas-vega-interaction-stock-index-follows-the-pointer ()
  "The index date starts at Jan 2005 and follows a pointermove."
  (eas-vega-interaction-with
    (let* ((view (eas-vega-interaction-open "stock-index-chart"))
           (label (lambda () (plist-get (aref (eas-vega-interaction-rows view "main" 5) 0) :index_label))))
      (should (equal (funcall label) "Jan 2005"))
      (eas-dispatch view '(:type "pointermove" :px [60 100]))
      (should-not (equal (funcall label) "Jan 2005")))))

;;; eas-play: timer and keys

(ert-deftest eas-vega-interaction-timer-drives-params ()
  "Timer handlers dispatch param events; a replay redraws them without a timer."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "hypothetical-outcome-plots"))
          (values (lambda (v) (mapcar (lambda (r) (plist-get r :value)) (eas-vega-interaction-rows v "main" 0)))))
      (let ((first (funcall values view)))
        (eas-play-tick view 100.0)
        (eas-play-tick view 101.0)
        (should (= (eas-vega-interaction-param view "sample") 3))
        (should (equal (last (eas-vega-interaction-summaries view) 2) '("param sample = 2" "param sample = 3")))
        (should-not (equal (funcall values view) first))
        (let ((again (eas-view-open "vega/hypothetical-outcome-plots"
                                    :bindings (eas-template-example "vega/hypothetical-outcome-plots") :id "again")))
          (eas-replay again (eas-view-log view))
          (should (equal (funcall values again) (funcall values view))))))
    ;; The clock reads the local wall clock of the tick's time.
    (let* ((view (eas-vega-interaction-open "clock"))
           (eas-time-zone nil)
           (at (float-time (encode-time (list 15 30 10 1 1 2026 nil nil t)))))
      (cl-letf (((symbol-function 'current-time-zone) (lambda (&rest _) (list 3600 "CET"))))
        (eas-play-tick view at))
      (should (= (eas-vega-interaction-param view "now") (* 1000.0 (+ at 3600))))
      (let ((hands (eas-vega-interaction-rows view "main" 5)))
        (should (= (plist-get (aref hands 0) :h) 11.5))
        (should (= (plist-get (aref hands 0) :s) 15))))))

(ert-deftest eas-vega-interaction-keys-drive-params ()
  "Key handlers scroll the table; keys without handlers do nothing."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "table-scrollbar"))
          (first-row (lambda (v) (plist-get (aref (eas-vega-interaction-rows v "main" 0) 0) :label))))
      (should (equal (funcall first-row view) "Test Row #1"))
      (eas-play-key view "down")
      (should (equal (funcall first-row view) "Test Row #2"))
      (eas-play-key view "pagedown")
      (should (= (eas-vega-interaction-param view "scroll") 12))
      (eas-play-key view "end")
      (should (= (eas-vega-interaction-param view "scroll") 241))
      (eas-play-key view "down")
      (should (= (eas-vega-interaction-param view "scroll") 241))
      (eas-play-key view "home")
      (eas-play-key view "pageup")
      (should (= (eas-vega-interaction-param view "scroll") 1))
      (should (null (eas-play-key view "x")))
      (should (equal (plist-get (eas-play-inspect view) :keys) ["down" "up" "pagedown" "pageup" "home" "end"])))))

(ert-deftest eas-vega-interaction-params-event ()
  "A params event sets several params with one redraw, and is checked."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "hypothetical-outcome-plots")))
      (eas-dispatch view '(:type "params" :values (:noise 0 :trend 0 :sample 5)))
      (should (equal (car (last (eas-vega-interaction-summaries view))) "params noise = 0, trend = 0, sample = 5"))
      (should (seq-every-p (lambda (r) (= (plist-get r :value) 5)) (eas-vega-interaction-rows view "main" 0)))
      (eas-test-should-code "EVENT_INVALID" (eas-dispatch view '(:type "params" :values [1 2])))
      (eas-test-should-code "EVENT_INVALID" (eas-dispatch view '(:type "params" :values (:nope 1)))))))

(ert-deftest eas-vega-interaction-play-config-is-checked ()
  "Bad x-eas.timer and x-eas.on fail as data, naming the path; describe lists them."
  (should (equal (plist-get (plist-get (eas-describe) :play) :min-interval) eas-play-min-interval))
  (should (equal (eas-play-config "vega/clock")
                 (list :timer '(:interval 1000) :on [(:events "timer" :param "now" :update "event.local")])))
  (eas-test-should-code "INVALID_INPUT" (eas-play-check '(:timer (:interval 5))))
  (eas-test-should-code "INVALID_INPUT" (eas-play-check '(:timer (:interval 100 :every 2))))
  (should (equal (plist-get (eas-test-should-code "INVALID_INPUT"
                              (eas-play-check '(:on [(:events "click" :param "a" :update "1")])))
                            :path)
                 "/x-eas/on/0/events"))
  (eas-test-should-code "INVALID_INPUT" (eas-play-check '(:on [(:events "timer" :param "a" :update "1" :when t)])))
  (eas-test-should-code "PARSE_ERROR" (eas-play-check '(:on [(:events "timer" :param "a" :update "1 +")])))
  (should (equal (eas-play-check '(:timer (:interval 100) :on [(:events ["key:a" "timer"] :param "a" :update "a + 1")]))
                 '(:interval 100 :handlers ((:events ("key:a" "timer") :param "a" :update "a + 1")))))
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "clock")))
      (eas-test-should-code "INVALID_INPUT"
        (eas-play-attach view '(:on [(:events "timer" :param "nope" :update "1")])))
      (eas-test-should-code "INVALID_INPUT"
        (eas-play-attach (eas-vega-interaction-open "crossfilter-flights"))))))

(ert-deftest eas-vega-interaction-timer-pauses-when-hidden ()
  "The timer callback skips ticks while the view's buffer is not visible."
  (eas-vega-interaction-with
    (let* ((view (eas-vega-interaction-open "hypothetical-outcome-plots"))
           (play (eas-play-attach view))
           (visible nil)
           (eas-play-visible-function (lambda (_) visible)))
      (eas-play--timer-fire "hypothetical-outcome-plots")
      (should (= (eas-play-skipped play) 1))
      (should (= (eas-play-ticks play) 0))
      (setq visible t)
      (eas-play--timer-fire "hypothetical-outcome-plots")
      (should (= (eas-play-ticks play) 1))
      ;; A closed view's timer detaches itself.
      (eas-view-close view)
      (eas-play--timer-fire "hypothetical-outcome-plots")
      (should (null (gethash "hypothetical-outcome-plots" eas-plays))))
    ;; Headless views have no buffer: not visible.
    (should-not (eas-play--visible-p (eas-vega-interaction-open "clock")))))

(ert-deftest eas-vega-interaction-show-starts-timer-and-binds-keys ()
  "`eas-show' starts a declared timer and binds the handlers' keys."
  (eas-vega-interaction-with
    (let ((buffers nil))
      (unwind-protect
          (cl-letf (((symbol-function 'eas-mode--window-size) (lambda (_window _target) '(:cols 60 :rows 20))))
            (let* ((view (eas-vega-interaction-open "table-scrollbar"))
                   (buffer (eas-show view 'text)))
              (push buffer buffers)
              (with-current-buffer buffer
                (should eas-play-keys-mode)
                (should (eq (lookup-key (cdr (assq 'eas-play-keys-mode minor-mode-overriding-map-alist)) [next])
                            #'eas-play-key-command))
                (should (eq (key-binding [down]) #'eas-play-key-command))
                (execute-kbd-macro [down])
                (should (= (eas-vega-interaction-param view "scroll") 2))
                (should-not (eas-play-timer (eas-play-get view)))))
            ;; Character keys: z dashes in the platformer.
            (let* ((view (eas-vega-interaction-open "platformer"))
                   (buffer (eas-show view 'text)))
              (push buffer buffers)
              (with-current-buffer buffer
                (execute-kbd-macro "z")
                (should (= (eas-vega-interaction-param view "dash") 6))))
            (let* ((view (eas-vega-interaction-open "clock"))
                   (buffer (eas-show view 'text)))
              (push buffer buffers)
              (should (eas-play-timer (eas-play-get view)))
              (should (= (eas-play-ticks (eas-play-get view)) 0))
              (with-current-buffer buffer (should-not eas-play-keys-mode))
              (kill-buffer buffer)
              (should-not (eas-play-timer (eas-play-get view)))))
        (dolist (b buffers) (when (buffer-live-p b) (kill-buffer b)))
        (maphash (lambda (_ play) (eas-play--cancel play)) eas-plays)))))

;;; Games

(ert-deftest eas-vega-interaction-pacman-eats-and-is-caught ()
  "The arrows steer; a gum scores 10; a ghost on the eater restarts."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "pacman")))
      (eas-play-key view "left")
      (should (equal (eas-vega-interaction-param view "dir") "left"))
      (eas-play-tick view 1.0)
      (should (equal (eas-vega-interaction-param view "pac") '(:x 6.0 :y 7)))
      (should (= (eas-vega-interaction-param view "score") 10))
      (should (equal (eas-vega-interaction-param view "eaten") ",6-7,"))
      ;; A wall to the left of (6, 7) stops it.
      (eas-play-tick view 2.0)
      (should (equal (plist-get (eas-vega-interaction-param view "pac") :x) 6.0))
      ;; The eaten gum is no longer drawn.
      (should-not (seq-find (lambda (r) (and (= (plist-get r :x) 6) (= (plist-get r :y) 7)))
                            (eas-vega-interaction-rows view "main" 2)))
      (eas-dispatch view '(:type "param" :param "ghosts"
                                 :value [(:x 6 :y 6 :color "red") (:x 14 :y 0 :color "steelblue")
                                         (:x 0 :y 14 :color "green") (:x 14 :y 14 :color "orange")]))
      (eas-play-key view "up")
      (eas-play-tick view 3.0)
      (should (eq (eas-vega-interaction-param view "caught") t))
      (should (equal (eas-vega-interaction-param view "pac") '(:x 7 :y 7)))
      (should (= (eas-vega-interaction-param view "score") 0))
      (should (= (eas-vega-interaction-param view "hi") 10)))))

(ert-deftest eas-vega-interaction-pacman-ghosts-are-path-symbols ()
  "Ghosts are Vega's ghost path at its own size: scale null keeps the size."
  (eas-vega-interaction-with
    (let* ((view (eas-vega-interaction-open "pacman"))
           (items (mapcan (lambda (m) (append (plist-get m :items) nil))
                          (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))
           (ghost (seq-find (lambda (i) (string-prefix-p "M" (or (plist-get i :shape) ""))) items)))
      (should (string-prefix-p "M13.95" (plist-get ghost :shape)))
      (should (= (plist-get ghost :size) 4))
      (should (string-match-p "translate(5,4.44) scale(1)" (eas-svg-render (eas-view-scene view)))))
    (let* ((spec '(:data (:values [(:v 1 :s 50) (:v 2 :s 200)]) :mark "point"
                   :encoding (:x (:field "v" :type "quantitative")
                              :size (:field "s" :type "quantitative" :scale :null))))
           (marks (plist-get (aref (plist-get (eas-compile spec) :views) 0) :marks)))
      (should (equal (mapcar (lambda (i) (plist-get i :size)) (plist-get (aref marks 0) :items)) '(50 200))))))

(ert-deftest eas-vega-interaction-platformer-falls-lands-and-jumps ()
  "Gravity pulls the player onto the terrain; up jumps off it."
  (eas-vega-interaction-with
    (let ((view (eas-vega-interaction-open "platformer")))
      (dotimes (i 60) (eas-play-tick view (float i)))
      (should (eq (eas-vega-interaction-param view "ground") t))
      (let ((landed (eas-vega-interaction-param view "py")))
        (should (< 80 landed 105))
        (eas-play-tick view 61.0)
        (should (= (eas-vega-interaction-param view "py") landed))
        (eas-play-key view "up")
        (eas-play-tick view 62.0)
        (should (< (eas-vega-interaction-param view "py") landed))
        (should (< (eas-vega-interaction-param view "vy") 0)))
      (eas-play-key view "right")
      (should (= (eas-vega-interaction-param view "hold") 6))
      (eas-play-tick view 63.0)
      (should (= (eas-vega-interaction-param view "vx") 1)))))

(provide 'eas-vega-interaction-test)
;;; eas-vega-interaction-test.el ends here
