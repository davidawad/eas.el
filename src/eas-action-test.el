;;; eas-action-test.el --- tests for the action registry and legend toggle -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.5: click-to-drill per mark, org jumps and legend toggles,
;; driven headlessly through `eas-dispatch' and `eas-replay'.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-action-test--legend-spec
  '(:data (:values [(:x 1 :y 3 :c "u") (:x 2 :y 5 :c "v") (:x 3 :y 4 :c "u") (:x 4 :y 8 :c "v")])
    :width 200 :height 100
    :params [(:name "series" :select (:type "point" :fields ["c"]) :bind "legend")]
    :mark "point"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
               :color (:field "c" :type "nominal")
               :opacity (:condition (:param "series" :value 1) :value 0.2)))
  "Points coloured by c, with a legend-bound toggle.")

(defconst eas-action-test--agg-spec
  '(:data (:values [(:cat "a" :item "x1" :v 1) (:cat "b" :item "y1" :v 4) (:cat "a" :item "x2" :v 2)
                    (:cat "a" :item "x3" :v 3)])
    :width 200 :height 100
    :mark "bar"
    :encoding (:x (:field "cat" :type "nominal") :y (:aggregate "sum" :field "v" :type "quantitative")))
  "Monthly-total style bars: each bar sums several source rows.")

(defmacro eas-action-test--with-view (var source &rest body)
  "Open SOURCE (a template name or spec) as VAR (id \"t\") in a fresh registry; run BODY.
SOURCE nil leaves VAR nil for BODY to open."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-action-inhibit nil)
         (eas-action-display-function #'ignore)
         (eas-action-drill-show-function #'ignore))
     (let ((,var (cond ((null ,source) nil)
                       ((stringp ,source)
                        (eas-view-open ,source :bindings (eas-template-example ,source) :id "t"))
                       (t (eas-view-open ,source :id "t")))))
       ,@body)))

(defun eas-action-test--centre (view mark-id i)
  "Pixel centre of item I of MARK-ID in VIEW's scene."
  (let ((item (aref (plist-get (eas-scene-mark (eas-view-scene view) mark-id) :items) i)))
    (if (plist-member item :w)
        (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
      (vector (plist-get item :x) (plist-get item :y)))))

(defun eas-action-test--legend-area (view label)
  "Centre of the SVG :map area of VIEW's legend entry LABEL."
  (let* ((entries (plist-get (aref (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :legends) 0)
                             :entries))
         (i (seq-position (mapcar (lambda (e) (plist-get e :label)) entries) label))
         (area (seq-find (lambda (a) (string-suffix-p (format "|%d" i) (symbol-name (nth 1 a))))
                         (seq-filter (lambda (a) (string-prefix-p "eas-legend:" (symbol-name (nth 1 a))))
                                     (eas-svg-hot-spots (eas-view-scene view)))))
         (rect (cdr (car area))))
    (vector (/ (+ (caar rect) (cadr rect)) 2.0) (/ (+ (cdar rect) (cddr rect)) 2.0))))

(defun eas-action-test--mouse-1 (view px)
  "Press and release at PX in VIEW (what a GUI mouse-1 sends); return the inspect."
  (eas-dispatch view (list :type "pointerdown" :px px))
  (eas-dispatch view (list :type "pointerup" :px px)))

(defun eas-action-test--opacities (view)
  "Opacity of each point in VIEW."
  (seq-map (lambda (i) (plist-get i :opacity)) (plist-get (eas-scene-mark (eas-view-scene view) "main/0") :items)))

;;; Registry and binding arguments

(ert-deftest eas-action-register-and-bind-with-args ()
  (eas-action-test--with-view v "bars"
    (let ((seen nil))
      (should (equal (eas-register-action "test-args" :fn (lambda (target _view) (setq seen target) "ok")
                                            :doc "Test.")
                     "test-args"))
      (should (equal (plist-get (eas-test-should-code "INVALID_INPUT" (eas-register-action "x" :fn 3)) :action) "x"))
      (eas-action-bind v "main/0" '(:action "test-args" :depth 2))
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 1))) :click)))
        (should (equal (plist-get click :action) "test-args"))
        (should (equal (plist-get click :args) '(:depth 2)))
        (should (equal (plist-get click :result) "ok"))
        (should (equal (plist-get click :source-row) 1))
        (should (equal (plist-get seen :args) '(:depth 2))))
      (should (equal (plist-get (eas-test-should-code "NOT_FOUND" (eas-action-bind v "*" '(:action "nope"))) :action)
                     "nope"))
      (eas-test-should-code "INVALID_INPUT" (eas-action-bind v "*" '(:depth 2)))
      (dolist (name '("open-href" "echo" "copy-row" "drill" "goto-source" "open-notes"))
        (should (member name (eas-action-names)))))))

(ert-deftest eas-action-template-binds-action-objects ()
  (let ((eas--templates (copy-sequence (progn (eas-template-names) eas--templates))))
    (eas-template-register
     (eas-json-parse
      "{\"x-eas\":{\"template\":\"test-drill\",\"version\":\"1.0.0\",
         \"slots\":{\"data\":{\"shape\":\"plist\",\"required\":true}},
         \"actions\":{\"main/0\":{\"action\":\"drill\",\"match\":[\"cat\"]}}},
        \"data\":{\"name\":\"data\"},\"width\":200,\"height\":100,\"mark\":\"bar\",
        \"encoding\":{\"x\":{\"field\":\"cat\",\"type\":\"nominal\"},
                    \"y\":{\"aggregate\":\"sum\",\"field\":\"v\",\"type\":\"quantitative\"}}}"))
    (eas-action-test--with-view v nil
      (setq v (eas-view-open "test-drill" :bindings (list :data (plist-get (plist-get eas-action-test--agg-spec :data) :values))
                               :id "t"))
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 0))) :click)))
        (should (equal (plist-get click :action) "drill"))
        (should (equal (plist-get click :args) '(:match ["cat"])))
        (should (equal (plist-get click :result) "t>a"))
        (should (= (length (plist-get (eas-view-data (eas-view-get "t>a")) :rows)) 3))))))

;;; Legend toggle

(ert-deftest eas-action-legend-toggle-through-svg-map-areas ()
  (eas-action-test--with-view v eas-action-test--legend-spec
    (let* ((px (eas-action-test--legend-area v "v"))
           (click (plist-get (eas-action-test--mouse-1 v px) :click)))
      (should (equal (plist-get (plist-get (eas-view-state v) :params) :series)
                     '(:type "point" :fields ["c"] :values [["v"]])))
      (should (equal (eas-action-test--opacities v) '(0.2 1 0.2 1)))
      (should (equal (plist-get click :legend) "color"))
      (should (equal (plist-get click :value) "v"))
      (should (equal (plist-get click :param) "series"))
      (should (eq (plist-get click :selected) t))
      ;; Nothing bound: a legend click never falls back to "*" or open-href.
      (eas-action-bind v "*" "copy-row")
      (should (eq (plist-get (plist-get (eas-action-test--mouse-1 v (eas-action-test--legend-area v "u")) :click)
                             :action)
                  :null))
      (should (equal (eas-action-test--opacities v) '(1 1 1 1)))
      ;; Clicking an entry again toggles it out.
      (let ((click (plist-get (eas-action-test--mouse-1 v px) :click)))
        (should (eq (plist-get click :selected) :false)))
      (should (equal (eas-selection v "series") [(:x 1 :y 3 :c "u") (:x 3 :y 4 :c "u")]))
      ;; Replaying the log rebuilds the same selection.
      (let ((log (eas-view-log v)))
        (eas-action-test--with-view w eas-action-test--legend-spec
          (eas-replay w log)
          (should (equal (eas-action-test--opacities w) (eas-action-test--opacities v))))))))

(ert-deftest eas-action-legend-param-runs-its-bound-action ()
  (eas-action-test--with-view v eas-action-test--legend-spec
    (let ((seen nil))
      (eas-register-action "test-legend" :fn (lambda (target _view) (push (plist-get target :value) seen) "legend"))
      (eas-action-bind v "series" "test-legend")
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--legend-area v "u"))) :click)))
        (should (equal (plist-get click :action) "test-legend"))
        (should (equal (plist-get click :result) "legend")))
      (eas-action-bind v "series" nil)
      (eas-action-bind v "legend" "test-legend")
      (eas-dispatch v (list :type "click" :px (eas-action-test--legend-area v "v")))
      (should (equal seen '("v" "u")))
      ;; A click on a point is not a legend click: the legend param's action stays put.
      (eas-action-bind v "series" "test-legend")
      (should (eq (plist-get (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 0)))
                                        :click)
                             :action)
                  :null)))))

(ert-deftest eas-action-legend-toggle-with-ret-in-a-text-buffer ()
  (eas-action-test--with-view v eas-action-test--legend-spec
    (eas-view-resize v '(:cols 60 :rows 16) 'text)
    (with-temp-buffer
      (eas-view-mode)
      (setq eas-mode--view v)
      (setf (eas-view-buffer v) (current-buffer))
      (let ((inhibit-read-only t)) (insert (eas-text-render (eas-view-scene v))))
      (goto-char (point-min))
      (while (not (equal (get-text-property (point) 'eas-legend) "u")) (forward-char 1))
      (eas-mode-click-at-point)
      (should (equal (plist-get (plist-get (eas-view-state v) :params) :series)
                     '(:type "point" :fields ["c"] :values [["u"]])))
      (should (equal (plist-get (plist-get (eas-inspect v) :click) :value) "u"))
      (should (string-match-p "1 value of c" (plist-get (aref (plist-get (eas-inspect v) :params) 0) :summary))))))

;;; open-href: URLs and org links

(ert-deftest eas-action-href-opens-urls-and-org-links ()
  (let* ((urls nil) (links nil)
         (eas-action-browse-function (lambda (u) (push u urls)))
         (eas-action-org-link-function (lambda (l) (push l links))))
    (dolist (href '("https://example.com/a" "mailto:x@example.com" "file:notes.org::*Mar 2" "id:abc" "[[*Heading]]"))
      (should (equal (eas-action-open-link href) href)))
    (should (equal urls '("mailto:x@example.com" "https://example.com/a")))
    (should (equal links '("[[*Heading]]" "id:abc" "file:notes.org::*Mar 2")))))

(ert-deftest eas-action-org-link-href-jumps-to-the-heading ()
  (let* ((file (make-temp-file "eas-notes" nil ".org" "* Intro\n* Mar 03 review\nbody\n"))
         (spec `(:data (:values [(:k "a" :v 3 :link ,(format "file:%s::*Mar 03 review" file))])
                 :width 200 :height 100 :mark "bar"
                 :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                            :href (:field "link")))))
    (unwind-protect
        (eas-action-test--with-view v spec
          (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 0)))
                                  :click)))
            (should (equal (plist-get click :action) "open-href"))
            (should (eq (plist-get click :ran) t))
            (with-current-buffer (find-buffer-visiting file)
              (should (looking-at-p "\\* Mar 03 review"))
              (kill-buffer))))
      (delete-file file))))

;;; goto-source

(defconst eas-action-test--org
  "#+title: steps\n\n#+name: steps\n| day | n |\n|-----+---|\n| Mon | 3 |\n| Tue | 5 |\n|-----+---|\n| Wed | 4 |\n\n* Tue\nwalked\n"
  "An org file with a named table (hlines included) and a heading per day.")

(ert-deftest eas-action-goto-source-jumps-to-the-org-table-row ()
  (let ((file (make-temp-file "eas-src" nil ".org" eas-action-test--org)))
    (unwind-protect
        (let* ((buffer (find-file-noselect file))
               (data (eas-data-from "org-table" (list :name "steps" :buffer buffer))))
          (eas-action-test--with-view v nil
            (setq v (eas-view-open "bars" :bindings (list :data (plist-get data :rows) :category "day" :value "n")
                                     :id "t"))
            (eas-action-bind v "main/0" "goto-source")
            (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 2)))
                                    :click)))
              (should (equal (plist-get (plist-get click :error) :code) "NOT_FOUND")))
            (eas-action-set-source v (list :file file :table "steps"))
            (dolist (case '((0 . "Mon") (2 . "Wed")))
              (let ((click (plist-get (eas-dispatch v (list :type "click"
                                                              :px (eas-action-test--centre v "main/0" (car case))))
                                      :click)))
                (should (eq (plist-get click :ran) t))
                (with-current-buffer buffer
                  (should (looking-at-p (cdr case)))
                  (should (equal (plist-get click :result) (format "%s:%d" file (line-number-at-pos)))))))
            ;; Heading mode: rows name their heading.
            (eas-action-set-source v (list :buffer buffer :heading "day"))
            (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 1)))
            (with-current-buffer buffer (should (looking-at-p "\\* Tue")))
            ;; A link column wins over the view source.
            (let* ((links nil) (eas-action-org-link-function (lambda (l) (push l links))))
              (eas-action-bind v "main/0" '(:action "goto-source" :field "day"))
              (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 0)))
              (should (equal links '("Mon")))))
          (kill-buffer buffer))
      (delete-file file))))

;;; open-notes

(ert-deftest eas-action-open-notes-for-the-clicked-date ()
  (let* ((dir (make-temp-file "eas-notes" t))
         (file (expand-file-name "journal.org" dir)))
    (with-temp-file file (insert "* 2026-03-02 Mon\n* 2026-03-03 Tue\nnotes\n"))
    (unwind-protect
        (eas-action-test--with-view v "line"
          (let ((px (let ((p (aref (plist-get (aref (plist-get (eas-scene-mark (eas-view-scene v) "main/0") :items) 0)
                                              :points)
                                   1)))
                      (vector (aref p 0) (aref p 1)))))
            (eas-action-bind v "*" "open-notes")
            (let ((click (plist-get (eas-dispatch v (list :type "click" :px px)) :click)))
              (should (equal (plist-get (plist-get click :error) :code) "NOT_FOUND"))
              (should (string-match-p "notes location" (plist-get (plist-get click :error) :message))))
            (let ((eas-action-notes-directory dir))
              (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :result)
                             (format "%s:1" (expand-file-name "2026-03-03.org" dir)))))
            (let ((eas-action-notes-file file))
              (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :result)
                             (format "%s:2" file))))
            (eas-action-bind v "*" `(:action "open-notes" :file ,file :field "value"))
            (should (equal (plist-get (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :error)
                                      :code)
                           "NOT_FOUND"))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p dir (buffer-file-name b))) (kill-buffer b)))
      (delete-directory dir t))))

;;; drill

(ert-deftest eas-action-drill-opens-the-rows-behind-an-aggregate ()
  (eas-action-test--with-view v eas-action-test--agg-spec
    (let ((shown nil))
      (setq eas-action-drill-show-function (lambda (detail parent) (push (list (eas-view-id detail) (eas-view-id parent)) shown)))
      (eas-action-bind v "main/0" '(:action "drill" :spec (:width 200 :height 100 :mark "bar"
                                                            :encoding (:x (:field "item" :type "nominal")
                                                                       :y (:field "v" :type "quantitative")))))
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-test--centre v "main/0" 0))) :click)))
        (should (equal (plist-get click :result) "t>a"))
        (should (equal shown '(("t>a" "t")))))
      (let ((detail (eas-view-get "t>a")))
        (should (equal (plist-get (eas-view-data detail) :rows)
                       [(:cat "a" :item "x1" :v 1) (:cat "a" :item "x2" :v 2) (:cat "a" :item "x3" :v 3)]))
        (should (= (length (plist-get (eas-scene-mark (eas-view-scene detail) "main/0") :items)) 3))
        (should (string-match-p "x3" (eas-text-render (progn (eas-view-resize detail '(:cols 50 :rows 12) 'text)
                                                               (eas-view-scene detail)))))))))

(ert-deftest eas-action-drill-asks-a-provider-for-finer-rows ()
  (eas-action-test--with-view v "line"
    (eas-register-drill "test-intraday"
                          :fn (lambda (target _view _args)
                                (let ((day (plist-get (plist-get target :row) :date)))
                                  (list (list :date (concat day "T14:30:00Z") :value 1.0)
                                        (list :date (concat day "T15:30:00Z") :value 2.0)
                                        (list :date (concat day "T16:30:00Z") :value 1.5)))))
    (eas-action-bind v "*" '(:action "drill" :provider "test-intraday" :template "line" :bindings (:title "Intraday")))
    (let* ((p (aref (plist-get (aref (plist-get (eas-scene-mark (eas-view-scene v) "main/0") :items) 0) :points) 2))
           (click (plist-get (eas-dispatch v (list :type "click" :px (vector (aref p 0) (aref p 1)))) :click)))
      (should (equal (plist-get click :result) "t>2026-03-04"))
      (let ((detail (eas-inspect "t>2026-03-04")))
        (should (equal (plist-get detail :template) "line")))
      ;; An unknown provider is recorded, not raised.
      (eas-action-bind v "*" '(:action "drill" :provider "nope"))
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (vector (aref p 0) (aref p 1)))) :click)))
        (should (equal (plist-get (plist-get click :error) :code) "NOT_FOUND"))))))

(provide 'eas-action-test)
;;; eas-action-test.el ends here
