;;; eas-zoom-test.el --- tests for zoom and pan (bind "scales") -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-zoom-test--spec
  '(:data (:values [(:t 1 :p 2) (:t 2 :p 20) (:t 3 :p 200) (:t 4 :p 50) (:t 5 :p 5)])
    :width 300 :height 150
    :params [(:name "grid" :select "interval" :bind "scales")]
    :mark "line"
    :encoding (:x (:field "t" :type "quantitative")
               :y (:field "p" :type "quantitative" :scale (:type "log"))))
  "A line zoomable on both x and a log y.")

(defconst eas-zoom-test--static
  '(:data (:values [(:t 1 :p 2) (:t 2 :p 3)]) :mark "line"
    :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")))
  "No scales-bound param: nothing zooms.")

(defmacro eas-zoom-test--with-view (var spec &rest body)
  "Open SPEC as view VAR in a fresh registry and run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal)))
     (let ((,var (eas-view-open ,spec :id "z")))
       ,@body)))

(defun eas-zoom-test--scale (view channel)
  "VIEW's first scene view scale for CHANNEL."
  (plist-get (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :scales) channel))

(defun eas-zoom-test--domain (view channel)
  "VIEW's CHANNEL domain as inspect reports it."
  (plist-get (plist-get (aref (plist-get (eas-inspect view) :views) 0) :domains) channel))

(defun eas-zoom-test--near (a b &optional eps)
  "Non-nil when A and B differ by less than EPS (default 1e-6, relative)."
  (< (abs (- a b)) (* (or eps 1e-6) (max 1.0 (abs a) (abs b)))))

;;; Domain arithmetic

(ert-deftest eas-zoom-domain-keeps-the-anchor-on-linear-log-and-reversed-scales ()
  (let ((linear (eas-scale-continuous "linear" 0 100 [0 200]))
        (log (eas-scale-continuous "log" 1 100 [0 100]))
        (reversed (eas-scale-continuous "linear" 0 10 [100 0])))
    (should (equal (eas-zoom-domain linear 0.5 50) [12.5 62.5]))
    (should (equal (eas-zoom-domain linear 0.5) [25.0 75.0]))
    ;; Log zooms geometrically: the centre pixel is 10 before and after.
    (let ((d (eas-zoom-domain log 0.5)))
      (should (eas-zoom-test--near (aref d 0) (sqrt 10)))
      (should (eas-zoom-test--near (aref d 1) (* 10 (sqrt 10)))))
    (dolist (case (list (list linear 37.0) (list log 81.0) (list reversed 30.0)))
      (let* ((scale (car case)) (px (cadr case))
             (before (eas-scale-invert scale px))
             (zoomed (plist-put (copy-sequence scale) :domain (eas-zoom-domain scale 0.3 px))))
        (should (eas-zoom-test--near (eas-scale-invert zoomed px) before))))
    ;; Degenerate results are refused, not applied.
    (should-not (eas-zoom-domain linear 1e-12 50))
    (should-not (eas-zoom-domain log 1e5 0))
    (should-not (eas-zoom-domain linear 1e305))))

(ert-deftest eas-zoom-pan-moves-the-picture-by-pixels ()
  (let ((linear (eas-scale-continuous "linear" 0 100 [0 200]))
        (log (eas-scale-continuous "log" 1 100 [100 0])))
    (should (equal (eas-zoom-pan-domain linear 20) [-10.0 90.0]))
    (should (seq-every-p #'identity (seq-mapn #'eas-zoom-test--near (eas-zoom-step-domain linear 0.1) [10 110])))
    ;; Up on a bottom-up log y shows larger values, by a constant ratio.
    (let ((d (eas-zoom-step-domain log 0.5)))
      (should (eas-zoom-test--near (aref d 0) 10))
      (should (eas-zoom-test--near (aref d 1) 1000)))))

;;; Reducer through dispatch

(ert-deftest eas-zoom-wheel-on-a-log-axis-keeps-the-datum-under-the-pointer ()
  ;; Checked against the scales the pointer was over: the next compile may
  ;; move the plot when the y labels change width.
  (eas-zoom-test--with-view v eas-zoom-test--spec
    (let* ((xs (eas-zoom-test--scale v :x)) (ys (eas-zoom-test--scale v :y))
           (px (vector (eas-scale-apply xs 3) (eas-scale-apply ys 200)))
           (span (lambda (d) (- (aref d 1) (aref d 0))))
           (state (lambda (ch) (plist-get (plist-get (plist-get (eas-view-state v) :domains) :main) ch))))
      (eas-dispatch v (list :type "wheel" :px px :delta -3))
      (dolist (case (list (list :x xs 0 3) (list :y ys 1 200)))
        (pcase-let ((`(,ch ,scale ,i ,datum) case))
          (should (eas-zoom-test--near
                   (eas-scale-invert (plist-put (copy-sequence scale) :domain (funcall state ch)) (aref px i))
                   datum))))
      (should (eas-zoom-test--near (funcall span (funcall state :x))
                                     (/ (funcall span (plist-get xs :domain)) (expt 1.2 3))))
      (should (eas-zoom-test--near (/ (aref (funcall state :y) 1) (aref (funcall state :y) 0))
                                     (expt (/ (aref (plist-get ys :domain) 1) (aref (plist-get ys :domain) 0))
                                           (/ 1 (expt 1.2 3)))))
      (should (equal (plist-get (eas-zoom-test--scale v :y) :type) "log"))
      (should (> (aref (plist-get (eas-zoom-test--scale v :y) :domain) 0) 0)))))

(ert-deftest eas-zoom-a-wheel-gesture-is-one-history-entry ()
  (eas-zoom-test--with-view v eas-zoom-test--spec
    (let ((home (eas-zoom-test--domain v :x)))
      (dotimes (_ 4) (eas-dispatch v '(:type "wheel" :px [120 60] :delta -0.5)))
      (should (= (length (plist-get (eas-view-state v) :history)) 1))
      ;; A new pointer position starts a new gesture.
      (eas-dispatch v '(:type "wheel" :px [200 60] :delta -1))
      (should (= (length (plist-get (eas-view-state v) :history)) 2))
      ;; So does any other domain change in between.
      (eas-dispatch v '(:type "key" :key "+"))
      (eas-dispatch v '(:type "wheel" :px [200 60] :delta -1))
      (should (= (length (plist-get (eas-view-state v) :history)) 4))
      (dotimes (_ 4) (eas-dispatch v '(:type "key" :key "[")))
      (should (equal (eas-zoom-test--domain v :x) home))
      (should (eq (plist-get (aref (plist-get (eas-inspect v) :views) 0) :zoomed) :false))
      (dotimes (_ 4) (eas-dispatch v '(:type "key" :key "]")))
      (should (= (length (plist-get (eas-view-state v) :history)) 4))
      (should (null (plist-get (eas-view-state v) :future))))))

(ert-deftest eas-zoom-no-op-zooms-leave-history-alone ()
  (eas-zoom-test--with-view v eas-zoom-test--static
    (let ((scene (eas-view-scene v)))
      (dolist (e '((:type "wheel" :px [100 50] :delta -1) (:type "key" :key "+")
                   (:type "key" :key "left") (:type "key" :key "0")))
        (eas-dispatch v e))
      (should (null (eas-view-state v)))
      (should (eq scene (eas-view-scene v)))))
  (eas-zoom-test--with-view v eas-zoom-test--spec
    (eas-dispatch v '(:type "key" :key "+"))
    (eas-dispatch v '(:type "key" :key "["))
    ;; Resetting an unzoomed view must not wipe the forward stack.
    (eas-dispatch v '(:type "key" :key "0"))
    (should (= (length (plist-get (eas-view-state v) :future)) 1))
    ;; Zooming in forever saturates instead of collapsing the scale.
    (dotimes (_ 400) (eas-dispatch v '(:type "key" :key "+")))
    (let ((d (plist-get (eas-zoom-test--scale v :x) :domain)))
      (should (< (aref d 0) (aref d 1))))
    (should (<= (length (plist-get (eas-view-state v) :history)) eas-reduce-history-limit))))

(ert-deftest eas-zoom-drag-pans-a-log-axis-by-pixels ()
  (eas-zoom-test--with-view v eas-zoom-test--spec
    (let* ((y (eas-zoom-test--scale v :y))
           (from (vector 150 (eas-scale-apply y 20)))
           (to (vector 150 (eas-scale-apply y 200))))
      (eas-dispatch v (list :type "drag" :from from :to to))
      ;; The datum grabbed at 20 now sits where 200 was.
      (should (eas-zoom-test--near (eas-scale-invert (eas-zoom-test--scale v :y) (aref to 1)) 20))
      (should (= (length (plist-get (eas-view-state v) :history)) 1)))))

(ert-deftest eas-zoom-state-survives-refresh-backend-switch-and-push ()
  (eas-zoom-test--with-view v eas-zoom-test--spec
    (eas-dispatch v '(:type "brush" :param "grid" :x [2 4]))
    (let ((zoomed (eas-zoom-test--domain v :x)))
      (should (equal zoomed [2.0 4.0]))
      ;; Refresh at a new size, then in a terminal frame: same data window.
      (eas-view-resize v '(500 . 300))
      (should (equal (eas-zoom-test--domain v :x) zoomed))
      (eas-view-resize v '(:cols 60 :rows 16) 'text)
      (should (equal (eas-zoom-test--domain v :x) zoomed))
      (should (equal (plist-get (plist-get (aref (plist-get (eas-inspect v) :views) 0) :visible) :n) 3))
      ;; Live rows arrive outside the window: it stays put, history intact.
      (eas-push v [(:t 6 :p 9) (:t 7 :p 90)])
      (should (equal (eas-zoom-test--domain v :x) zoomed))
      (should (= (plist-get (eas-inspect v) :rows) 7))
      (eas-dispatch v '(:type "key" :key "["))
      (should (equal (eas-zoom-test--domain v :x) [1.0 7.0])))))

(ert-deftest eas-zoom-keys-work-in-a-terminal-buffer ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let* ((view (eas-view-open eas-zoom-test--spec :id "tty"))
           (buffer (eas-show view 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((before (buffer-string)))
              (execute-kbd-macro "+")
              (should (eq (plist-get (aref (plist-get (eas-inspect view) :views) 0) :zoomed) t))
              (should-not (equal (buffer-string) before))
              (let ((x (aref (eas-zoom-test--domain view :x) 0)))
                (execute-kbd-macro [S-right])
                (should (> (aref (eas-zoom-test--domain view :x) 0) x))
                (execute-kbd-macro "<")
                (should (eas-zoom-test--near (aref (eas-zoom-test--domain view :x) 0) x)))
              (execute-kbd-macro "0")
              (should (equal (buffer-string) before))))
        (kill-buffer buffer)))))

;;; Native input

(ert-deftest eas-zoom-trackpad-input-becomes-wheel-deltas ()
  (let ((posn (list (selected-window) 1 '(10 . 10) 0)))
    (should (= (eas-zoom-wheel-delta (list 'wheel-down posn)) 1))
    (should (= (eas-zoom-wheel-delta (list 'mouse-4 posn)) -1))
    (should (= (eas-zoom-wheel-delta (list 'wheel-up posn 1 3 '(0 . 80.0))) -2.0))
    (should (= (eas-zoom-wheel-delta (list 'wheel-down posn 1 1 '(0 . 10.0))) 0.25)))
  (should (eas-zoom-test--near (eas-zoom-pinch-delta 1.2 1.0) -1.0))
  (should (eas-zoom-test--near (eas-zoom-pinch-delta 1.0 1.2) 1.0))
  (should (= (eas-zoom-pinch-delta 0 1.0) 0)))

(ert-deftest eas-zoom-pinch-and-horizontal-scroll-reach-the-reducer ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let ((view (eas-view-open eas-zoom-test--spec :id "gui")))
      (with-temp-buffer
        (setq eas-mode--view view)
        (let* ((image '(image :type svg :data ""))
               (pinch (lambda (scale dx)
                        (list 'pinch (list (selected-window) 1 '(10 . 10) 0 nil 1 '(0 . 0) image
                                           '(150 . 80) '(300 . 150))
                              dx 0.0 scale 0.0)))
               (span (lambda () (let ((d (plist-get (eas-zoom-test--scale view :x) :domain)))
                                  (- (aref d 1) (aref d 0)))))
               (home (funcall span)))
          (eas-mode-pinch (funcall pinch 1.0 0.0))
          (should (null (eas-view-state view)))
          (eas-mode-pinch (funcall pinch 1.44 3.0))
          (should (eas-zoom-test--near (funcall span) (/ home 1.44) 1e-3))
          ;; A new gesture starts from 1.0 again rather than the old scale.
          (eas-mode-pinch (funcall pinch 1.0 0.0))
          (should (eas-zoom-test--near (funcall span) (/ home 1.44) 1e-3))
          (let ((x (aref (plist-get (eas-zoom-test--scale view :x) :domain) 0)))
            (eas-mode-hscroll (list 'wheel-right (list (selected-window) 1 '(10 . 10) 0)))
            (should (> (aref (plist-get (eas-zoom-test--scale view :x) :domain) 0) x))))))))

(ert-deftest eas-zoom-replays-identically ()
  (let ((eas-views (make-hash-table :test 'equal))
        (events '((:type "wheel" :px [120 60] :delta -1) (:type "wheel" :px [120 60] :delta -1)
                  (:type "drag" :from [100 60] :to [140 40]) (:type "key" :key "up")
                  (:type "key" :key "[") (:type "wheel" :px [200 90] :delta 2))))
    (let ((a (eas-view-open eas-zoom-test--spec :id "a"))
          (b (eas-view-open eas-zoom-test--spec :id "b")))
      (dolist (e events) (eas-dispatch a e))
      (eas-replay b (eas-view-log a))
      (should (equal (eas-view-state a) (eas-view-state b)))
      (should (equal (eas-scene-to-json (eas-view-scene a)) (eas-scene-to-json (eas-view-scene b)))))))

(provide 'eas-zoom-test)
;;; eas-zoom-test.el ends here
