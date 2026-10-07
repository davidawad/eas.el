;;; eas-tty-test.el --- tests for terminal parity (fc-qx1.8) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-tty-test--line
  '(:data (:values [(:t "2026-01-01" :p 10) (:t "2026-01-02" :p 12) (:t "2026-01-03" :p 11)
                    (:t "2026-01-04" :p 15) (:t "2026-01-05" :p 13) (:t "2026-01-06" :p 18)])
    :width 300 :height 150
    :params [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
    :layer [(:mark "line" :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
            (:params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "rule"
             :encoding (:x (:field "t" :type "temporal")
                        :opacity (:condition (:param "hover" :empty :false :value 1) :value 0)))])
  "A temporal line with a crosshair, zoomable on x.")

(defconst eas-tty-test--points
  '(:data (:values [(:x 1 :y 3 :c "u") (:x 2 :y 5 :c "v") (:x 3 :y 4 :c "u") (:x 4 :y 8 :c "v") (:x 5 :y 6 :c "u")])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))
             (:name "pick" :select "point")
             (:name "legend" :select (:type "point" :fields ["c"]) :bind "legend")]
    :mark "point"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
               :color (:field "c" :type "nominal")
               :opacity (:condition (:param "legend" :value 1) :value 0.2)))
  "Points with a brush, a click selection and a legend toggle.")

(defconst eas-tty-test--text-size '(:cols 72 :rows 24) "Text view size for lockstep replays.")

(defmacro eas-tty-test--with (bindings &rest body)
  "Fresh registry, quiet hooks; BINDINGS as in `let*'; then BODY."
  (declare (indent 1))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-brush-functions nil) (eas-action-inhibit t) (inhibit-message t))
     (let* ,bindings ,@body)))

(defun eas-tty-test--px (view x y)
  "Scene pixel of data X, Y in VIEW's first scene view."
  (let ((scales (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :scales)))
    (vector (eas-scale-apply (plist-get scales :x) x) (eas-scale-apply (plist-get scales :y) y))))

(defun eas-tty-test--should-agree (result)
  "Assert parity RESULT is ok, showing its mismatches when not."
  (should (equal (plist-get result :mismatches) []))
  (should (eq (plist-get result :ok) t)))

;;; Translation and comparison

(ert-deftest eas-tty-translate-lands-on-the-same-datum ()
  (eas-tty-test--with ((gui (eas-view-open eas-tty-test--points :id "gui"))
                         (tty (eas-view-open eas-tty-test--points :id "tty" :target 'text
                                               :size eas-tty-test--text-size)))
    (let* ((px (eas-tty-test--px gui 4 8))
           (out (eas-parity-translate-px px (eas-view-scene gui) (eas-view-scene tty))))
      (should (equal (plist-get (eas-hit (eas-view-scene tty) nil out) :datum) 3))
      (should (< (plist-get (eas-hit (eas-view-scene tty) nil out) :distance) 1e-6)))
    ;; Legend entries map to the same entry, keys pass through untouched.
    (let* ((legend (aref (plist-get (aref (plist-get (eas-view-scene gui) :views) 0) :legends) 0))
           (b (plist-get (aref (plist-get legend :entries) 1) :bounds))
           (px (vector (+ (aref b 0) 1.0) (+ (aref b 1) 1.0)))
           (out (eas-parity-translate (list :type "click" :px px) (eas-view-scene gui) (eas-view-scene tty))))
      (should (equal (eas-parity--legend-entry (eas-view-scene tty) (aref (plist-get out :px) 0)
                                                 (aref (plist-get out :px) 1))
                     (list (plist-get (aref (plist-get (eas-view-scene tty) :views) 0) :id) "color" "v"))))
    (should (equal (eas-parity-translate '(:type "key" :key "+") nil nil) '(:type "key" :key "+")))))

(ert-deftest eas-tty-parity-diff-ignores-pixels-and-names-paths ()
  (should-not (eas-parity-diff '(:a [1.0 2.0] :b "x") '(:a [1.0000000000001 2.0] :b "x")))
  (should (equal (eas-parity-diff '(:a [1 2] :b (:c 1)) '(:a [1 3] :b (:c "1")))
                 '((:path "/a/1" :a 2 :b 3) (:path "/b/c" :a 1 :b "1")))))

;;; One log, both backends

(ert-deftest eas-tty-parity-zoom-pan-hover-log ()
  (eas-tty-test--with ((gui (eas-view-open eas-tty-test--line :id "gui"))
                         (tty (eas-view-open eas-tty-test--line :id "tty" :target 'text
                                               :size eas-tty-test--text-size))
                         (d2 (eas-tty-test--px gui "2026-01-02" 12))
                         (d3 (eas-tty-test--px gui "2026-01-03" 11))
                         (d5 (eas-tty-test--px gui "2026-01-05" 13))
                         (log (list (list :type "pointermove" :px d2)
                                    (list :type "wheel" :px d3 :delta -2)
                                    (list :type "wheel" :px d3 :delta -1)
                                    '(:type "key" :key "+") '(:type "key" :key "right")
                                    (list :type "drag" :from d3 :to d2)
                                    (list :type "pointerdown" :px d2) (list :type "pointermove" :px d3)
                                    (list :type "pointerup" :px d5)
                                    '(:type "key" :key "[") '(:type "key" :key "[") '(:type "key" :key "]")
                                    (list :type "pointermove" :px d5)
                                    (list :type "dblclick" :px d3) '(:type "key" :key "0")
                                    '(:type "pointerleave"))))
    (let ((result (eas-parity-replay log gui tty)))
      (eas-tty-test--should-agree result)
      (should (= (plist-get result :steps) (length log))))
    ;; The replay really moved things: history and the zoom happened.
    (should (> (plist-get (eas-parity-state tty) :history) 3))))

(ert-deftest eas-tty-parity-brush-click-legend-log ()
  (eas-tty-test--with ((gui (eas-view-open eas-tty-test--points :id "gui"))
                         (tty (eas-view-open eas-tty-test--points :id "tty" :target 'text
                                               :size eas-tty-test--text-size))
                         (legend (aref (plist-get (aref (plist-get (eas-view-scene gui) :views) 0) :legends) 0))
                         (b (plist-get (aref (plist-get legend :entries) 1) :bounds))
                         (log (list (list :type "pointermove" :px (eas-tty-test--px gui 2 5))
                                    (list :type "click" :px (eas-tty-test--px gui 3 4))
                                    (list :type "drag" :from (eas-tty-test--px gui 1.5 5) :to (eas-tty-test--px gui 4.5 5))
                                    '(:type "key" :key "z") '(:type "key" :key "[")
                                    '(:type "brush" :x [2 4])
                                    (list :type "click" :px (vector (+ (aref b 0) 2.0) (+ (aref b 1) 2.0)))
                                    (list :type "pointermove" :px (eas-tty-test--px gui 5 6))
                                    '(:type "key" :key "escape"))))
    (eas-tty-test--should-agree (eas-parity-replay log gui tty))
    ;; z zoomed to the brush, [ undid it, escape cleared the selections.
    (should (equal (plist-get (eas-parity-state tty) :future) 1))
    (should-not (plist-get (eas-parity-state tty) :params))))

;;; Driving a text buffer, replaying its log on the GUI backend

(defun eas-tty-test--replay-on-gui (spec tty-view)
  "Replay TTY-VIEW's log on a fresh text view and a fresh GUI view of SPEC."
  (let ((text (eas-view-open spec :id "text2" :target 'text :size (eas-view-size tty-view)))
        (gui (eas-view-open spec :id "gui2")))
    (prog1 (eas-parity-replay (eas-view-log tty-view) text gui)
      (should-not (eas-parity-diff (eas-parity-state text) (eas-parity-state tty-view))))))

(ert-deftest eas-tty-keyboard-session-replays-on-the-gui-backend ()
  (eas-tty-test--with ((view (eas-view-open eas-tty-test--line :id "tty"))
                         (buffer (eas-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (execute-kbd-macro "nn")
          (should (equal (plist-get (plist-get (eas-view-state view) :hover) :datum) 1))
          (should (get-text-property (point) 'eas-datum))
          (execute-kbd-macro [right right])
          ;; A zoom redraws with new labels; point keeps its cell, so the
          ;; hover is not re-sent.
          (let ((cell (cons (line-number-at-pos) (current-column))))
            (execute-kbd-macro "+")
            (should (equal (cons (line-number-at-pos) (current-column)) cell))
            (should (equal (plist-get (car (eas-view-log view)) :type) "key")))
          (execute-kbd-macro [S-right])
          (execute-kbd-macro "p")
          (execute-kbd-macro "-[")
          (execute-kbd-macro "\e\e")
          (should-not (plist-get (eas-view-state view) :hover))
          (should (>= (length (eas-view-log view)) 8))
          (eas-tty-test--should-agree (eas-tty-test--replay-on-gui eas-tty-test--line view)))
      (kill-buffer buffer))))

(ert-deftest eas-tty-brush-session-replays-on-the-gui-backend ()
  (eas-tty-test--with ((view (eas-view-open eas-tty-test--points :id "tty"))
                         (buffer (eas-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((from (text-property-any (point-min) (point-max) 'eas-datum 0))
                (to (text-property-any (point-min) (point-max) 'eas-datum 3)))
            (set-mark from)
            (goto-char to)
            (eas-brush-region)
            (should (plist-get (plist-get (eas-view-state view) :params) :brush))
            (execute-kbd-macro "z")
            (execute-kbd-macro "n")
            (execute-kbd-macro (kbd "RET"))
            (should (plist-get (plist-get (eas-view-state view) :params) :pick))
            (eas-tty-test--should-agree (eas-tty-test--replay-on-gui eas-tty-test--points view))))
      (kill-buffer buffer))))

;;; xterm-mouse in a text buffer

(defun eas-tty-test--mouse (type pos)
  "A terminal mouse event of TYPE at buffer position POS."
  (list type (list (selected-window) pos '(0 . 0) 0 nil pos '(0 . 0) nil '(0 . 0) '(1 . 1))))

(ert-deftest eas-tty-xterm-mouse-drag-brushes-and-point-does-not-interfere ()
  (eas-tty-test--with ((view (eas-view-open eas-tty-test--points :id "tty"))
                         (buffer (eas-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let* ((from (text-property-any (point-min) (point-max) 'eas-datum 1))
                 (to (text-property-any (point-min) (point-max) 'eas-datum 4))
                 (run (lambda (cmd ev)
                        (let ((last-command-event ev))
                          (funcall cmd ev)
                          (eas-mode--post-command)))))
            (funcall run #'eas-mode-pointer (eas-tty-test--mouse 'mouse-movement from))
            (should (equal (plist-get (plist-get (eas-view-state view) :hover) :datum) 1))
            (funcall run #'eas-mode-down (eas-tty-test--mouse 'down-mouse-1 from))
            (funcall run #'eas-mode-pointer (eas-tty-test--mouse 'mouse-movement to))
            (funcall run #'eas-mode-up (eas-tty-test--mouse 'mouse-1 to))
            ;; Point stayed at the top: no pointermove from it broke the drag.
            (should (equal (mapcar (lambda (e) (plist-get e :type)) (reverse (eas-view-log view)))
                           '("pointermove" "pointerdown" "pointermove" "pointerup")))
            (let ((store (plist-get (plist-get (eas-view-state view) :params) :brush)))
              (should store)
              (should (eas-parity--same-number (aref (plist-get store :x) 0) 2))
              (should (eas-parity--same-number (aref (plist-get store :x) 1) 5)))
            ;; A key after the mouse keeps the mouse's hover (point did not
            ;; move), and n steps on from it.
            (funcall run #'eas-mode-pointer (eas-tty-test--mouse 'mouse-movement from))
            (execute-kbd-macro "+")
            (should (equal (plist-get (car (eas-view-log view)) :type) "key"))
            (should (equal (plist-get (plist-get (eas-view-state view) :hover) :datum) 1))
            (execute-kbd-macro "n")
            (should (equal (plist-get (plist-get (eas-view-state view) :hover) :datum) 2))
            (eas-tty-test--should-agree (eas-tty-test--replay-on-gui eas-tty-test--points view))))
      (kill-buffer buffer))))

(ert-deftest eas-tty-keymap-and-mouse-capability ()
  (should (eq (lookup-key eas-view-mode-map "n") #'eas-tty-next-datum))
  (should (eq (lookup-key eas-view-mode-map "p") #'eas-tty-previous-datum))
  (should (functionp (lookup-key eas-view-mode-map (kbd "ESC ESC"))))
  (dolist (term '("xterm-256color" "tmux-256color" "screen" "xterm-kitty" "alacritty"))
    (should (eas-tty-mouse-capable-p term)))
  (dolist (term '("dumb" "linux" "vt100"))
    (should-not (eas-tty-mouse-capable-p term)))
  ;; fc-qx1.24: emacs-plus NS -nw under tmux reports $TERM "dumb".
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'tty-type) (lambda (&rest _) "xterm-256color"))
            ((symbol-function 'getenv) (lambda (&rest _) "dumb")))
    (should (eas-tty-mouse-capable-p)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'tty-type) (lambda (&rest _) "dumb"))
            ((symbol-function 'getenv) (lambda (&rest _) "xterm-256color")))
    (should (eas-tty-mouse-capable-p)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'tty-type) (lambda (&rest _) "linux"))
            ((symbol-function 'getenv) (lambda (&rest _) "xterm-256color")))
    (should-not (eas-tty-mouse-capable-p)))
  ;; Batch never turns the global mode on.
  (let ((eas-tty-xterm-mouse t))
    (with-temp-buffer
      (eas-tty--enable-mouse)
      (should-not (bound-and-true-p xterm-mouse-mode)))))

(ert-deftest eas-tty-step-clamps-at-both-ends ()
  (eas-tty-test--with ((view (eas-view-open eas-tty-test--points :id "s")))
    (let ((scene (eas-view-scene view)))
      (should (equal (eas-tty-step-px scene nil 1) (eas-tty-test--px view 1 3)))
      (should (equal (eas-tty-step-px scene nil -1) (eas-tty-test--px view 5 6)))
      (eas-dispatch view (list :type "pointermove" :px (eas-tty-step-px scene nil -1)))
      (should (equal (eas-tty-step-px scene (plist-get (eas-view-state view) :hover) 3)
                     (eas-tty-test--px view 5 6)))
      (should (equal (eas-tty-step-px scene (plist-get (eas-view-state view) :hover) -2)
                     (eas-tty-test--px view 3 4))))))

(provide 'eas-tty-test)
;;; eas-tty-test.el ends here
