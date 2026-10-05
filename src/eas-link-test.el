;;; eas-link-test.el --- tests for linked views (fc-qx1.6) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-agent)

(defun eas-link-test--rows (field n &optional offset)
  "N daily rows from 2026-03-01 with date FIELD and close OFFSET+i."
  (vconcat (cl-loop for i from 0 below n
                    collect (list (eas-key field) (format "2026-03-%02d" (1+ i)) :close (+ (or offset 100) i)))))

(defun eas-link-test--spec (field)
  "A crosshair + zoom line chart over date FIELD, the line template's idiom."
  `(:width 300 :height 100
    :layer [(:name "hit"
             :params [(:name "crosshair" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))
                      (:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
             :mark "line"
             :encoding (:x (:field ,field :type "temporal") :y (:field "close" :type "quantitative")))
            (:transform [(:filter (:param "crosshair" :empty :false))] :mark "rule"
             :encoding (:x (:field ,field :type "temporal")))]))

(defun eas-link-test--panes (&optional top-params)
  "Two vconcat panes over one data set; TOP-PARAMS go on the concat."
  `(:data (:values ,(eas-link-test--rows "date" 20))
    ,@(and top-params (list :params top-params))
    :vconcat [(:name "price" :width 300 :height 100 :mark "line"
               :encoding (:x (:field "date" :type "temporal") :y (:field "close" :type "quantitative")))
              (:name "volume" :width 300 :height 50 :mark "bar"
               :encoding (:x (:field "date" :type "temporal") :y (:field "close" :type "quantitative")))]))

(defmacro eas-link-test--fresh (&rest body)
  "Run BODY with empty view and bus registries."
  (declare (indent 0))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-link-buses (make-hash-table :test 'equal)))
     ,@body))

(defun eas-link-test--view (view id)
  "Scene view ID of live VIEW."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get (eas-view-scene view) :views)))

(defun eas-link-test--px (view id &optional fx)
  "A pixel inside scene view ID of VIEW, FX (default 0.5) across its plot."
  (let ((b (plist-get (eas-link-test--view view id) :bounds)))
    (vector (+ (aref b 0) (* (or fx 0.5) (aref b 2))) (+ (aref b 1) (/ (aref b 3) 2.0)))))

(defun eas-link-test--domain (view id)
  "Scene view ID's x domain in VIEW."
  (plist-get (plist-get (plist-get (eas-link-test--view view id) :scales) :x) :domain))

(defun eas-link-test--store (view name)
  "VIEW's store for selection NAME."
  (plist-get (plist-get (eas-view-state (eas-view-get view)) :params) (eas-key name)))

;;; Within one spec

(ert-deftest eas-link-concat-params-apply-to-every-view ()
  "A select param on a concat is defined in every child view (Vega-Lite top-level params)."
  (let* ((scene (eas-compile (eas-link-test--panes
                              [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")])))
         (params (eas-params-of scene)))
    (should (equal (mapcar (lambda (p) (list (plist-get p :name) (plist-get p :view))) params)
                   '(("zoom" "price") ("zoom" "volume"))))
    (should-not (plist-get (car params) :views))))

(ert-deftest eas-link-concat-params-honour-views ()
  "\"views\" limits a concat param to the named views (or their layers)."
  (let ((scene (eas-compile (eas-link-test--panes
                             [(:name "hover" :select (:type "point" :on "pointermove") :views ["volume"])]))))
    (should (equal (mapcar (lambda (p) (plist-get p :view)) (eas-params-of scene)) '("volume")))))

(ert-deftest eas-link-shared-scale-bind-zooms-every-view ()
  "Zooming, panning and resetting one view of a shared scales param moves all of them."
  (eas-link-test--fresh
    (let ((v (eas-view-open (eas-link-test--panes
                             [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")])
                            :id "panes")))
      ;; The bar pane pads its domain, so each view has its own unzoomed one.
      (let ((full (eas-link-test--domain v "price")) (full-volume (eas-link-test--domain v "volume")))
        (eas-dispatch v (list :type "wheel" :px (eas-link-test--px v "volume" 0.3) :delta -2))
        (should-not (equal (eas-link-test--domain v "volume") full-volume))
        (should (equal (eas-link-test--domain v "price") (eas-link-test--domain v "volume")))
        (eas-dispatch v (list :type "pointermove" :px (eas-link-test--px v "price")))
        (eas-dispatch v '(:type "key" :key "right"))
        (should (equal (eas-link-test--domain v "price") (eas-link-test--domain v "volume")))
        (eas-dispatch v '(:type "key" :key "0"))
        (should (equal (eas-link-test--domain v "price") full))
        (should (equal (eas-link-test--domain v "volume") full-volume))
        (should (eq (plist-get (aref (plist-get (eas-inspect v) :views) 1) :zoomed) :false))
        ;; Undo restores every linked view at once.
        (eas-dispatch v '(:type "key" :key "["))
        (should-not (equal (eas-link-test--domain v "volume") full-volume))
        (should (equal (eas-link-test--domain v "price") (eas-link-test--domain v "volume")))))))

(ert-deftest eas-link-wheel-gesture-is-one-history-entry ()
  "Linked zoom keeps consecutive wheel steps at one spot as one history entry."
  (let* ((scene (eas-compile (eas-link-test--panes
                              [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")])))
         (b (plist-get (aref (plist-get scene :views) 0) :bounds))
         (px (vector (+ (aref b 0) 50) (+ (aref b 1) 20)))
         (s1 (eas-reduce nil (list :type "wheel" :px px :delta -1) scene))
         (s2 (eas-reduce s1 (list :type "wheel" :px px :delta -1) scene)))
    (should (= (length (plist-get s2 :history)) 1))
    (should (equal (plist-get (plist-get s2 :domains) :price) (plist-get (plist-get s2 :domains) :volume)))))

(ert-deftest eas-link-shared-hover-and-brush-across-concat ()
  "One concat-level crosshair param: hovering either pane fills the one store."
  (eas-link-test--fresh
    (let ((v (eas-view-open (eas-link-test--panes
                             [(:name "crosshair" :select (:type "point" :on "pointermove" :nearest t
                                                         :encodings ["x"]))])
                            :id "panes")))
      (eas-dispatch v (list :type "pointermove" :px (eas-link-test--px v "volume" 0.0)))
      (should (equal (plist-get (eas-link-test--store v "crosshair") :values) [["2026-03-01"]]))
      (eas-dispatch v (list :type "pointermove" :px (eas-link-test--px v "price" 1.0)))
      (should (equal (plist-get (eas-link-test--store v "crosshair") :values) [["2026-03-20"]])))))

(defconst eas-link-test--overview
  `(:data (:values ,(vconcat (cl-loop for i from 0 below 40 collect (list :t i :v (* i i)))))
    :vconcat [(:name "detail" :width 300 :height 100 :mark "line"
               :encoding (:x (:field "t" :type "quantitative" :scale (:domain (:param "brush")))
                          :y (:field "v" :type "quantitative")))
              (:name "overview" :width 300 :height 40 :mark "area"
               :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
               :encoding (:x (:field "t" :type "quantitative") :y (:field "v" :type "quantitative")))])
  "Overview + detail: the detail's x domain follows the overview's brush.")

(ert-deftest eas-link-scale-domain-follows-selection ()
  "scale.domain {param} follows the interval; empty, the data decides.
The view clips its marks either way, as Vega-Lite does."
  (eas-link-test--fresh
    (let* ((v (eas-view-open eas-link-test--overview :id "od"))
           (full (eas-link-test--domain v "detail")))
      (should (eq (plist-get (eas-link-test--view v "detail") :clip) t))
      (should (eq (plist-get (eas-link-test--view v "overview") :clip) :false))
      (eas-dispatch v '(:type "brush" :param "brush" :x [10 20]))
      (should (equal (eas-link-test--domain v "detail") [10.0 20.0]))
      (should (equal (eas-link-test--domain v "overview") full))
      (should (eq (plist-get (eas-link-test--view v "detail") :clip) t))
      ;; Dragging a new brush in the overview moves the detail (no stale patch).
      (eas-dispatch v (list :type "drag" :from (eas-link-test--px v "overview" 0.5)
                            :to (eas-link-test--px v "overview" 0.75)))
      (let ((d (eas-link-test--domain v "detail")))
        (should (< 19 (aref d 0) 21))
        (should (< 29 (aref d 1) 31)))
      (eas-dispatch v (list :type "dblclick" :px (eas-link-test--px v "overview")))
      (should (equal (eas-link-test--domain v "detail") full)))))

(defun eas-link-test--gallery (name data)
  "Official gallery spec NAME with its url data inlined from DATA rows."
  (let ((spec (eas-json-read-file (eas-test-file "test/vl-examples/interactive" (concat name ".vl.json")))))
    (plist-put spec :data (list :values data))))

(defun eas-link-test--sp500 ()
  "sp500.csv rows with \"Jan 1 2000\" dates as ISO."
  (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (vconcat (mapcar (lambda (row)
                       (let ((parts (split-string (plist-get row :date))))
                         (list :date (format "%s-%02d-%02d" (nth 2 parts)
                                             (1+ (cl-position (nth 0 parts) months :test #'equal))
                                             (string-to-number (nth 1 parts)))
                               :price (plist-get row :price))))
                     (plist-get (eas-data-from "csv" (eas-test-file "test/vl-examples/data/sp500.csv")) :rows)))))

(ert-deftest eas-link-gallery-overview-detail ()
  "The official interactive_overview_detail: brushing the overview zooms the detail."
  (eas-link-test--fresh
    (let ((v (eas-view-open (eas-link-test--gallery "interactive_overview_detail" (eas-link-test--sp500))
                            :id "od")))
      (should (eas-view-interactive v))
      (eas-dispatch v '(:type "brush" :param "brush" :x ["2005-01-01" "2006-01-01"]))
      (let ((detail (aref (plist-get (eas-inspect v) :views) 0)))
        (should (equal (plist-get (plist-get detail :domains) :x) ["2005-01-01" "2006-01-01"]))
        (should (= (plist-get (plist-get detail :visible) :n) 13))))))

(ert-deftest eas-link-gallery-concat-layer-cross-highlight ()
  "The official interactive_concat_layer: a genre click filters the other view's points."
  (eas-link-test--fresh
    (let* ((movies (eas-json-read-file (eas-test-file "test/vl-examples/data/movies.json")))
           (v (eas-view-open (eas-link-test--gallery "interactive_concat_layer" movies) :id "cl"))
           (points (lambda () (length (plist-get (aref (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :marks) 1)
                                                 :items))))
           (all (funcall points)))
      (eas-dispatch v (list :type "click" :px (eas-link-test--px v "vconcat_1" 0.02)))
      (should (eas-link-test--store v "pts"))
      (should (< 0 (funcall points) all)))))

;;; The link event

(ert-deftest eas-link-event-validates ()
  (eas-test-should-code "EVENT_INVALID" (eas-event-parse '(:type "link" :store :null)))
  (eas-test-should-code "EVENT_INVALID" (eas-event-parse '(:type "link" :param "p" :store (:type "lasso"))))
  (should (eas-event-parse "{\"type\":\"link\",\"param\":\"p\",\"store\":null}"))
  (should (equal (eas-event-describe '(:type "link" :param "zoom" :store (:type "interval" :x [1 2]) :from "A"))
                 "link zoom x 1..2 from A")))

;;; The bus

(defun eas-link-test--pair ()
  "Views A (field date, 20 days) and B (field day, 15 days) on bus \"t\"."
  (let ((a (eas-view-open (eas-link-test--spec "date") :id "A" :rows (eas-link-test--rows "date" 20)))
        (b (eas-view-open (eas-link-test--spec "day") :id "B" :rows (eas-link-test--rows "day" 15 50))))
    (eas-link-join a "t")
    (eas-link-join b "t")
    (list a b)))

(ert-deftest eas-link-bus-hover-follows-by-x ()
  "Hovering A sets B's crosshair at the same x though B names the field differently."
  (eas-link-test--fresh
    (pcase-let ((`(,a ,b) (eas-link-test--pair)))
      (eas-dispatch a (list :type "pointermove" :px (eas-link-test--px a "main" (/ 4.0 19))))
      (should (equal (plist-get (eas-link-test--store a "crosshair") :values) [["2026-03-05"]]))
      (should (equal (eas-link-test--store b "crosshair")
                     '(:type "point" :fields ["day"] :values [["2026-03-05"]])))
      ;; The rule layer draws in B.
      (should (= 1 (length (plist-get (aref (plist-get (eas-link-test--view b "main") :marks) 1) :items))))
      ;; A received nothing back: no echo.
      (should-not (seq-find (lambda (e) (equal (plist-get e :type) "link")) (eas-view-log-entries a)))
      (should (equal (plist-get (aref (eas-view-log-entries b) 0) :summary) "link crosshair 1 point from A"))
      (eas-dispatch a '(:type "pointerleave"))
      (should-not (eas-link-test--store b "crosshair")))))

(ert-deftest eas-link-bus-snaps-to-nearest ()
  "A date B lacks snaps to B's nearest datum; one outside B's range matches nothing."
  (eas-link-test--fresh
    (let ((a (eas-view-open (eas-link-test--spec "date") :id "A" :rows (eas-link-test--rows "date" 20)))
          (b (eas-view-open (eas-link-test--spec "date") :id "B"
                            :rows [(:date "2026-03-01" :close 1) (:date "2026-03-04" :close 2)
                                   (:date "2026-03-10" :close 3)])))
      (eas-link-join a "t") (eas-link-join b "t")
      (eas-dispatch b '(:type "link" :param "crosshair" :store (:type "point" :encodings ["x"] :values [["2026-03-05"]])))
      (should (equal (plist-get (eas-link-test--store b "crosshair") :values) [["2026-03-04"]]))
      (eas-dispatch b '(:type "link" :param "crosshair" :store (:type "point" :encodings ["x"] :values [["2026-03-19"]])))
      (should (equal (plist-get (eas-link-test--store b "crosshair") :values) [["2026-03-19"]]))
      ;; A link event is never forwarded.
      (should (zerop (length (eas-view-log-entries a)))))))

(ert-deftest eas-link-bus-zoom-and-brush ()
  "A scales-bound param carries domains; an interval brush carries its range."
  (eas-link-test--fresh
    (pcase-let ((`(,a ,b) (eas-link-test--pair)))
      (eas-dispatch a '(:type "key" :key "+"))
      (should (equal (eas-link-test--domain b "main") (eas-link-test--domain a "main")))
      (eas-dispatch b '(:type "key" :key "0"))
      (should (eq (plist-get (aref (plist-get (eas-inspect a) :views) 0) :zoomed) :false))
      (eas-dispatch a '(:type "brush" :param "zoom" :x ["2026-03-03" "2026-03-06"]))
      (should (equal (plist-get (plist-get (aref (plist-get (eas-inspect b) :views) 0) :domains) :x)
                     ["2026-03-03" "2026-03-06"])))))

(ert-deftest eas-link-bus-brush-maps-fields ()
  "An interval selection arrives under the receiver's own field."
  (eas-link-test--fresh
    (let* ((spec (lambda (field)
                   `(:mark "point" :width 200 :height 100
                     :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
                     :encoding (:x (:field ,field :type "temporal") :y (:field "close" :type "quantitative")))))
           (a (eas-view-open (funcall spec "date") :id "A" :rows (eas-link-test--rows "date" 10)))
           (b (eas-view-open (funcall spec "day") :id "B" :rows (eas-link-test--rows "day" 10))))
      (eas-link-join a "t" '("brush")) (eas-link-join b "t")
      (eas-dispatch a '(:type "brush" :param "brush" :x ["2026-03-02" "2026-03-04"]))
      (let ((store (eas-link-test--store b "brush")))
        (should (equal (plist-get store :fields) '(:x "day")))
        (should (= 3 (length (eas-selection b "brush"))))))))

(ert-deftest eas-link-bus-late-join-replay-and-prune ()
  "A late member catches up; a receiver's log replays alone; closed views leave."
  (eas-link-test--fresh
    (pcase-let ((`(,a ,b) (eas-link-test--pair)))
      (eas-dispatch a '(:type "key" :key "+"))
      (eas-dispatch a (list :type "pointermove" :px (eas-link-test--px a "main" 0.5)))
      (let ((c (eas-view-open (eas-link-test--spec "date") :id "C" :rows (eas-link-test--rows "date" 20))))
        (eas-link-join c "t" '("zoom" "crosshair"))
        (should (equal (eas-link-test--domain c "main") (eas-link-test--domain a "main")))
        (should (equal (eas-link-test--store c "crosshair") (eas-link-test--store a "crosshair")))
        ;; Replaying B's log on a fresh, unlinked copy reproduces B.
        (let ((copy (eas-view-open (eas-link-test--spec "day") :id "B2" :rows (eas-link-test--rows "day" 15 50))))
          (eas-replay copy (eas-view-log b))
          (should (equal (eas-view-state copy) (eas-view-state b))))
        (eas-view-close c)
        (eas-dispatch a '(:type "pointerleave"))
        (should (equal (append (plist-get (eas-link-describe-bus "t") :members) nil) '("A" "B")))
        (should (equal (eas-link-buses-of "A") '("t")))
        (eas-link-leave "A")
        (eas-link-leave "B")
        (should-not (eas-link-bus-names))))))

(ert-deftest eas-link-bus-params-filter ()
  "A bus carries only its named params."
  (eas-link-test--fresh
    (pcase-let ((`(,a ,b) (eas-link-test--pair)))
      (eas-link-join a "t" '("zoom"))
      (eas-dispatch a (list :type "pointermove" :px (eas-link-test--px a "main" 0.5)))
      (should-not (eas-link-test--store b "crosshair"))
      (should-error (eas-link-describe-bus "nope") :type 'eas-error))))

;;; Agent surface

(ert-deftest eas-link-agent-verbs ()
  (eas-link-test--fresh
    (eas-view-open (eas-link-test--spec "date") :id "A" :rows (eas-link-test--rows "date" 20))
    (eas-view-open (eas-link-test--spec "day") :id "B" :rows (eas-link-test--rows "day" 15))
    (let ((env (eas-agent "link" "A" "t" :params "crosshair,zoom")))
      (should (eq (plist-get env :ok) t))
      (should (equal (plist-get (plist-get env :data) :params) ["crosshair" "zoom"])))
    (eas-agent "link" "B" "t")
    (should (equal (plist-get (aref (plist-get (eas-agent "buses") :data) 0) :members) ["A" "B"]))
    (eas-agent "dispatch" "A" "{\"type\":\"key\",\"key\":\"+\"}")
    (should (eq (plist-get (aref (plist-get (plist-get (eas-agent "inspect" "B") :data) :views) 0) :zoomed) t))
    (should (eq (plist-get (eas-agent "unlink" "B" :bus "t") :ok) t))
    (should (equal (plist-get (eas-agent "link" "nope" "t") :reason) "VIEW_NOT_FOUND"))
    (should (plist-get (plist-get (eas-agent "describe" "link") :data) :link))))

;;; Demo: panes template, two tickers

(ert-deftest eas-link-panes-template ()
  "price + volume + RSI: hovering one pane draws the crosshair in all three; zoom moves all."
  (eas-link-test--fresh
    (let ((v (eas-view-open "panes" :bindings (eas-template-example "panes") :subject "TSM")))
      (should (equal (mapcar (lambda (sv) (plist-get sv :id)) (plist-get (eas-view-scene v) :views))
                     '("price" "volume" "rsi")))
      (eas-dispatch v (list :type "pointermove" :px (eas-link-test--px v "volume" 0.5)))
      (dolist (id '("price" "volume" "rsi"))
        (let ((rule (car (last (append (plist-get (eas-link-test--view v id) :marks) nil)))))
          (should (= 1 (length (plist-get rule :items))))))
      (eas-dispatch v '(:type "key" :key "+"))
      (should (equal (eas-link-test--domain v "price") (eas-link-test--domain v "rsi")))
      (should (equal (eas-link-test--domain v "price") (eas-link-test--domain v "volume")))
      (let ((rsi (plist-get (aref (plist-get (eas-inspect v) :views) 2) :visible)))
        (should (equal (plist-get rsi :field) "rsi"))
        (should (<= 0 (plist-get rsi :min) (plist-get rsi :max) 100))))))

(ert-deftest eas-link-panes-golden ()
  (eas-test-golden "resolve-panes.json"
                   (eas-json-pretty (eas-resolve "panes" (eas-template-example "panes"))))
  (eas-test-golden "text-panes.txt"
                   (eas-text-render (eas-compile (eas-resolve "panes" (eas-template-example "panes"))
                                                 :target 'text :size '(:cols 80 :rows 36)))))

(ert-deftest eas-link-demo-two-tickers ()
  "The demo's two tickers share hover and zoom over the bus \"tickers\"."
  (eas-link-test--fresh
    (pcase-let ((`(,tsm ,demo) (eas-link-demo-open)))
      (should (equal (eas-view-id tsm) "panes:TSM"))
      (should (equal (eas-view-id demo) "panes:DEMO"))
      (eas-dispatch tsm (list :type "pointermove" :px (eas-link-test--px tsm "price" 0.5)))
      (should (eas-link-test--store demo "crosshair"))
      (should (equal (plist-get (eas-link-test--store demo "crosshair") :values)
                     (plist-get (eas-link-test--store tsm "crosshair") :values)))
      (eas-dispatch demo '(:type "key" :key "+"))
      (should (equal (eas-link-test--domain tsm "rsi") (eas-link-test--domain demo "price"))))))

(provide 'eas-link-test)
;;; eas-link-test.el ends here
