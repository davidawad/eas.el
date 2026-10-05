;;; eas-runtime-test.el --- tests for view/v1, event/v1, reducers and glue -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-runtime-test--spec
  '(:data (:values [(:t "2026-01-01" :p 10 :s "a") (:t "2026-01-02" :p 12 :s "b") (:t "2026-01-03" :p 11 :s "a")
                    (:t "2026-01-04" :p 15 :s "b")])
    :width 300 :height 150
    :params [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
    :layer [(:mark "line" :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
            (:params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "rule"
             :encoding (:x (:field "t" :type "temporal")
                        :opacity (:condition (:param "hover" :empty :false :value 1) :value 0)))])
  "Line + crosshair rule, zoomable on x.")

(defconst eas-runtime-test--brush-spec
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

(defmacro eas-runtime-test--with-view (var spec &rest body)
  "Open SPEC as view VAR (id \"t\") in a fresh registry and run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal)))
     (let ((,var (eas-view-open ,spec :id "t")))
       ,@body)))

(defun eas-runtime-test--x (view value)
  "Scene x pixel of data VALUE in VIEW's first scene view."
  (eas-scale-apply (plist-get (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :scales) :x) value))

(ert-deftest eas-runtime-view-ids-are-stable-and-readable ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (should (equal (eas-view-id (eas-view-open "line" :bindings (eas-template-example "line") :subject "daily"))
                   "line:daily"))
    (should (equal (eas-view-id (eas-view-open "line" :bindings (eas-template-example "line") :subject "daily"))
                   "line:daily<2>"))
    (should (equal (eas-view-ids) '("line:daily" "line:daily<2>")))
    (should (equal (plist-get (eas-test-should-code "VIEW_NOT_FOUND" (eas-inspect "nope")) :view) "nope"))))

(ert-deftest eas-runtime-events-are-validated ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (dolist (case '(((:type "teleport") . "type") ((:type "pointermove") . "px")
                    ((:type "wheel" :px [1 2]) . "delta") ((:type "key" :key "x") . "key")
                    ((:type "brush") . "x") ((:type "drag" :from [1 2]) . "to")))
      (should (equal (plist-get (eas-test-should-code "EVENT_INVALID" (eas-dispatch v (car case))) :field)
                     (cdr case))))
    (should (plist-get (eas-dispatch v "{\"type\":\"pointermove\",\"px\":[150,60]}") :hover))))

(ert-deftest eas-runtime-crosshair-follows-the-pointer ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let* ((px (vector (+ 2 (eas-runtime-test--x v "2026-01-02")) 60))
           (inspect (eas-dispatch v (list :type "pointermove" :px px))))
      (should (equal (plist-get (plist-get (plist-get inspect :hover) :row) :t) "2026-01-02"))
      (should (equal (plist-get (aref (plist-get inspect :params) 1) :summary) "1 value of t"))
      (should (equal (seq-map (lambda (i) (plist-get i :opacity))
                              (plist-get (eas-scene-mark (eas-view-scene v) "main/1") :items))
                     '(0 1 0 0)))
      (eas-dispatch v '(:type "pointerleave"))
      (should (eq (plist-get (eas-inspect v) :hover) :null))
      (should (equal (seq-map (lambda (i) (plist-get i :opacity))
                              (plist-get (eas-scene-mark (eas-view-scene v) "main/1") :items))
                     '(0 0 0 0))))))

(ert-deftest eas-runtime-recompiles-only-when-visible-output-changes ()
  (eas-runtime-test--with-view v eas-runtime-test--brush-spec
    (let ((scene (eas-view-scene v)))
      (eas-dispatch v '(:type "pointermove" :px [100 50]))
      (should (eq scene (eas-view-scene v)))
      (eas-dispatch v '(:type "brush" :param "brush" :x [2 4]))
      (should-not (eq scene (eas-view-scene v))))))

(ert-deftest eas-runtime-drag-brushes-and-exports-the-selection ()
  (eas-runtime-test--with-view v eas-runtime-test--brush-spec
    (let ((a (eas-runtime-test--x v 1.5)) (b (eas-runtime-test--x v 4.5)))
      (eas-dispatch v (list :type "pointerdown" :px (vector a 50)))
      (eas-dispatch v (list :type "pointermove" :px (vector (/ (+ a b) 2) 50)))
      (eas-dispatch v (list :type "pointerup" :px (vector b 50)))
      (let ((store (plist-get (plist-get (eas-view-state v) :params) :brush)))
        (should (equal (plist-get store :fields) '(:x "x")))
        (should (< (abs (- (aref (plist-get store :x) 0) 1.5)) 1e-9)))
      (should (eas-scene-mark (eas-view-scene v) "main/brush:brush"))
      (should (equal (eas-selection v "brush") [(:x 2 :y 5 :c "v") (:x 3 :y 4 :c "u") (:x 4 :y 8 :c "v")]))
      (should (equal (eas-selection v "brush" 'json)
                     "[{\"x\":2,\"y\":5,\"c\":\"v\"},{\"x\":3,\"y\":4,\"c\":\"u\"},{\"x\":4,\"y\":8,\"c\":\"v\"}]"))
      (should (string-prefix-p "| x | y | c |\n|---+---+---|\n| 2 | 5 | v |" (eas-selection v "brush" 'org)))
      (should (string-match-p "x 1.5..4.5" (plist-get (aref (plist-get (eas-inspect v) :params) 0) :summary)))
      ;; A click without movement clears the brush.
      (eas-dispatch v (list :type "pointerdown" :px (vector a 50)))
      (eas-dispatch v (list :type "pointerup" :px (vector a 50)))
      (should-not (plist-get (plist-get (eas-view-state v) :params) :brush)))))

(ert-deftest eas-runtime-click-selects-shift-toggles-empty-clears ()
  (eas-runtime-test--with-view v eas-runtime-test--brush-spec
    (let* ((items (plist-get (eas-scene-mark (eas-view-scene v) "main/0") :items))
           (at (lambda (i) (vector (plist-get (aref items i) :x) (plist-get (aref items i) :y)))))
      (eas-dispatch v (list :type "click" :px (funcall at 1)))
      (should (equal (eas-selection v "pick") [(:x 2 :y 5 :c "v")]))
      (eas-dispatch v (list :type "click" :px (funcall at 3) :shift t))
      (should (= (length (eas-selection v "pick")) 2))
      (eas-dispatch v (list :type "click" :px (funcall at 3) :shift t))
      (should (= (length (eas-selection v "pick")) 1))
      (let ((b (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds)))
        (eas-dispatch v (list :type "click" :px (vector (+ 10 (aref (funcall at 0) 0)) (+ 2 (aref b 1))))))
      (should (equal (eas-selection v "pick") [])))))

(ert-deftest eas-runtime-legend-click-toggles-a-legend-bound-selection ()
  (eas-runtime-test--with-view v eas-runtime-test--brush-spec
    (let* ((legend (aref (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :legends) 0))
           (entry (aref (plist-get legend :entries) 1))
           (b (plist-get entry :bounds)))
      (eas-dispatch v (list :type "click" :px (vector (+ (aref b 0) 2) (+ (aref b 1) 2))))
      (should (equal (plist-get (plist-get (eas-view-state v) :params) :legend)
                     '(:type "point" :fields ["c"] :values [["v"]])))
      (should (equal (seq-map (lambda (i) (plist-get i :opacity))
                              (plist-get (eas-scene-mark (eas-view-scene v) "main/0") :items))
                     '(0.2 1 0.2 1 0.2))))))

(ert-deftest eas-runtime-wheel-zooms-around-the-pointer ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let* ((px (vector (eas-runtime-test--x v "2026-01-02T06:00:00Z") 60))
           (scale (lambda () (plist-get (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :scales) :x)))
           (anchor (eas-scale-invert (funcall scale) (aref px 0)))
           (span (lambda () (let ((d (plist-get (funcall scale) :domain))) (- (aref d 1) (aref d 0)))))
           (before (funcall span)))
      (eas-dispatch v (list :type "wheel" :px px :delta -2))
      (should (< (abs (- (eas-scale-invert (funcall scale) (aref px 0)) anchor)) 1e-3))
      (should (< (abs (- (funcall span) (/ before (* 1.2 1.2)))) 1))
      (should (eq (plist-get (aref (plist-get (eas-inspect v) :views) 0) :zoomed) t))
      (should (eq (plist-get (eas-view-scene v) :views) (plist-get (eas-view-scene v) :views))))))

(ert-deftest eas-runtime-keys-zoom-pan-reset-and-history ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let ((domain (lambda () (plist-get (aref (plist-get (eas-inspect v) :views) 0) :domains))))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01" "2026-01-04"]))
      (eas-dispatch v '(:type "key" :key "+"))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01T07:12:00Z" "2026-01-03T16:48:00Z"]))
      (eas-dispatch v '(:type "key" :key "right"))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01T12:57:36Z" "2026-01-03T22:33:36Z"]))
      (eas-dispatch v '(:type "key" :key "["))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01T07:12:00Z" "2026-01-03T16:48:00Z"]))
      (eas-dispatch v '(:type "key" :key "["))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01" "2026-01-04"]))
      (eas-dispatch v '(:type "key" :key "]"))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01T07:12:00Z" "2026-01-03T16:48:00Z"]))
      (eas-dispatch v '(:type "key" :key "0"))
      (should (equal (plist-get (funcall domain) :x) ["2026-01-01" "2026-01-04"])))))

(ert-deftest eas-runtime-drag-pans-and-dblclick-resets ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let* ((x0 (eas-runtime-test--x v "2026-01-02")) (x1 (eas-runtime-test--x v "2026-01-03")))
      (eas-dispatch v (list :type "drag" :from (vector x1 60) :to (vector x0 60)))
      (should (equal (plist-get (plist-get (aref (plist-get (eas-inspect v) :views) 0) :domains) :x)
                     ["2026-01-02" "2026-01-05"]))
      (eas-dispatch v (list :type "dblclick" :px (vector x0 60)))
      (should (equal (plist-get (plist-get (aref (plist-get (eas-inspect v) :views) 0) :domains) :x)
                     ["2026-01-01" "2026-01-04"])))))

(ert-deftest eas-runtime-agent-brush-on-a-scales-param-zooms ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let ((inspect (eas-dispatch v '(:type "brush" :param "zoom" :x ["2026-01-02" "2026-01-03"]))))
      (should (equal (plist-get (plist-get (aref (plist-get inspect :views) 0) :domains) :x)
                     ["2026-01-02" "2026-01-03"]))
      (should (equal (plist-get (plist-get (aref (plist-get inspect :views) 0) :visible) :n) 2)))
    (eas-test-should-code "EVENT_INVALID"
      (let ((eas-views (make-hash-table :test 'equal)))
        (eas-dispatch (eas-view-open '(:data (:values [(:a 1)]) :mark "point" :encoding (:x (:field "a"))))
                        '(:type "brush" :x [0 1]))))))

(ert-deftest eas-runtime-push-streams-rows-through-the-schema ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let ((inspect (eas-push v [(:t "2026-01-05" :p 20 :s "a")])))
      (should (= (plist-get inspect :rows) 5))
      (should (equal (plist-get (plist-get (aref (plist-get inspect :views) 0) :visible) :last) 20))
      (should (= (plist-get (eas-view-state v) :stream-cursor) 1)))
    (should (equal (plist-get (eas-test-should-code "SHAPE_INVALID" (eas-push v [(:t "2026-01-06" :q 1)])) :field)
                   "q"))))

(ert-deftest eas-runtime-replay-reproduces-state ()
  (let ((eas-views (make-hash-table :test 'equal))
        (events (list '(:type "pointermove" :px [120 60]) '(:type "key" :key "+")
                      '(:type "wheel" :px [200 80] :delta 1) '(:type "key" :key "left")
                      '(:type "drag" :from [100 60] :to [160 60]))))
    (let ((a (eas-view-open eas-runtime-test--spec :id "a"))
          (b (eas-view-open eas-runtime-test--spec :id "b")))
      (dolist (e events) (eas-dispatch a e))
      (eas-replay b (eas-view-log a))
      (should (equal (eas-view-state a) (eas-view-state b)))
      (should (equal (eas-scene-to-json (eas-view-scene a)) (eas-scene-to-json (eas-view-scene b))))
      (should (equal (mapcar (lambda (e) (plist-get e :summary)) (eas-view-log-entries a))
                     '("pointermove at [120 60]" "key +" "wheel at [200 80]" "key left" "drag [100 60] -> [160 60]"))))))

(ert-deftest eas-runtime-log-is-bounded ()
  (eas-runtime-test--with-view v eas-runtime-test--spec
    (let ((eas-view-log-size 3))
      (dotimes (i 5) (eas-dispatch v (list :type "pointermove" :px (vector (+ 60 i) 60))))
      (should (= (length (eas-view-log v)) 3))
      (should (= (plist-get (aref (eas-view-log-entries v) 0) :seq) 3)))))

(ert-deftest eas-runtime-params-semantics ()
  (let ((point '(:type "point" :fields ["t"] :values [["2026-01-02"]]))
        (interval (list :type "interval" :fields '(:x "t") :x (vector (eas-time-parse "2026-01-02") (eas-time-parse "2026-01-03")))))
    (should (eas-params-contains point '(:t "2026-01-02T00:00:00Z")))
    (should-not (eas-params-contains point '(:t "2026-01-03")))
    (should (eas-params-contains interval '(:t "2026-01-02T12:00:00Z")))
    (should-not (eas-params-contains interval '(:t "2026-01-04")))
    (should (eas-params-test nil "x" '(:t 1) t))
    (should-not (eas-params-test nil "x" '(:t 1) nil))
    (should (equal (eas-params-toggle (eas-params-toggle nil '("c") '(:c "a")) '("c") '(:c "a")) nil))))

(ert-deftest eas-runtime-patched-scenes-equal-full-compiles ()
  "Selection-only updates patch the plan; the result must match a fresh compile."
  (let ((specs (list eas-runtime-test--spec eas-runtime-test--brush-spec
                     ;; The Vega-Lite crosshair idiom: a rule layer filtered by the param.
                     '(:data (:values [(:t 1 :p 3) (:t 2 :p 5) (:t 3 :p 4) (:t 4 :p 8)])
                       :layer [(:mark "line" :encoding (:x (:field "t" :type "quantitative")
                                                        :y (:field "p" :type "quantitative")))
                               (:params [(:name "h" :select (:type "point" :on "pointermove" :nearest t
                                                                  :encodings ["x"]))]
                                :mark "point" :encoding (:x (:field "t" :type "quantitative")
                                                         :y (:field "p" :type "quantitative")
                                                         :size (:condition (:param "h" :empty :false :value 80)
                                                                :value 10)))
                               (:transform [(:filter (:param "h" :empty :false))]
                                :mark "rule" :encoding (:x (:field "t" :type "quantitative")))]))))
    (dolist (spec specs)
      (let ((eas-views (make-hash-table :test 'equal)))
        (let ((v (eas-view-open spec :id "p")))
          (dolist (e '((:type "pointermove" :px [60 50]) (:type "pointermove" :px [120 50])
                       (:type "click" :px [90 40]) (:type "pointermove" :px [150 60])
                       (:type "click" :px [150 60] :shift t) (:type "pointerleave")))
            (eas-dispatch v e)
            (should (equal (eas-scene-to-json (eas-view-scene v))
                           (eas-scene-to-json
                            (eas-params-with-state (eas-view-state v)
                              (eas-compile (eas-view-spec v) :state (eas-view-state v))))))))))))

;;; Glue

(ert-deftest eas-runtime-gui-posn-divides-by-image-scale ()
  (let* ((image '(image :type svg :data "" :scale 2))
         (posn (list (selected-window) 1 '(10 . 10) 0 nil 1 '(0 . 0) image '(200 . 100) '(400 . 300)))
         (event (list 'mouse-movement posn)))
    (should (equal (eas-mode-event-px event) [100.0 50.0]))))

(ert-deftest eas-runtime-text-buffer-hover-follows-point ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let* ((view (eas-view-open eas-runtime-test--spec :id "glue"))
           (buffer (eas-show view 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq major-mode 'eas-view-mode))
            (should (string-match-p "┬" (buffer-string)))
            (goto-char (point-min))
            (let ((pos (text-property-any (point-min) (point-max) 'eas-datum 3)))
              (should pos)
              (goto-char pos)
              (eas-mode--post-command)
              (should (equal (plist-get (plist-get (plist-get (eas-inspect view) :hover) :row) :p) 15))
              (should (string-match-p "p=15" (format "%s" header-line-format))))
            (eas-mode--send '(:type "key" :key "+"))
            (should (eq (plist-get (aref (plist-get (eas-inspect view) :views) 0) :zoomed) t)))
        (kill-buffer buffer)))))

(provide 'eas-runtime-test)
;;; eas-runtime-test.el ends here
