;;; eas-distributions-test.el --- fc-qx1.39: distributions polish -*- lexical-binding: t; -*-

;;; Commentary:

;; The distributions gallery group's second pass: oracle references of
;; their own and masks, the memoized transforms, explicit color ranges,
;; pre-binned axis ticks, the property check, the
;; customization specs and the gallery bench.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-agent)
(require 'eas-vl-gallery)
(require 'eas-vl-gallery-bench)
(require 'eas-spec-props)

(defmacro eas-distributions-test--gallery (status &rest body)
  "Run BODY over a scratch gallery whose group \"g\" has STATUS and one spec \"s\"."
  (declare (indent 1))
  `(let* ((root (make-temp-file "eas-gallery" t))
          (eas-vl-gallery-directory root))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "g/ref" root) t)
           (with-temp-file (expand-file-name "g/s.vl.json" root)
             (insert "{\"data\": {\"values\": [{\"a\": 1, \"b\": 2}]}, \"mark\": \"bar\","
                     " \"encoding\": {\"x\": {\"field\": \"a\", \"type\": \"ordinal\"},"
                     " \"y\": {\"field\": \"b\", \"type\": \"quantitative\"}}}"))
           (with-temp-file (expand-file-name "g/status.json" root) (insert (eas-json-pretty ,status)))
           ,@body)
       (delete-directory root t))))

;;; Oracle references and masks

(ert-deftest eas-distributions-ref-of-its-own ()
  "A status.json \"ref\" wins; \"interim_ref\" serves until bin/chart's ref exists."
  (eas-distributions-test--gallery '(:s (:status "pass" :ref "ref/s.vega.png"))
    (should (equal (file-name-nondirectory (eas-vl-gallery-ref-file "g" "s")) "s.vega.png"))
    (should (string-match-p "STALE_REF" (eas-vl-gallery-ref-problem "g" "s"))))
  (eas-distributions-test--gallery '(:s (:status "pass" :interim_ref "ref/s.vega.png"))
    (should (equal (file-name-nondirectory (eas-vl-gallery-ref-file "g" "s")) "s.vega.png"))
    (with-temp-file (expand-file-name "g/ref/s.png" eas-vl-gallery-directory) (insert "x"))
    (should (equal (file-name-nondirectory (eas-vl-gallery-ref-file "g" "s")) "s.png"))))

(ert-deftest eas-distributions-write-status-keeps-oracle-fields ()
  (eas-distributions-test--gallery '(:s (:status "pass" :ref "ref/s.vega.png" :ref_build "node x" :mask "emoji"
                                         :note "why" :threshold 0.01))
    (let* ((eas-chart-rsvg-program "no-such-rasterizer")
           (entry (plist-get (eas-vl-gallery-write-status "g") :s)))
      (should (equal (list (plist-get entry :ref) (plist-get entry :ref_build) (plist-get entry :mask)
                           (plist-get entry :threshold))
                     '("ref/s.vega.png" "node x" "emoji" 0.01))))))

(ert-deftest eas-distributions-custom-groups-are-groups ()
  (should (member "distributions/custom" (eas-vl-gallery-groups)))
  (should (>= (length (eas-vl-gallery-names "distributions/custom")) 8)))

(ert-deftest eas-distributions-emoji-mask ()
  (let* ((scene '(:views [(:marks [(:mark "text" :items [(:x 50 :y 40 :text "🐖" :fontSize 20
                                                             :align "center" :baseline "middle")
                                                            (:x 10 :y 10 :text "plain" :fontSize 20)])])]))
         (boxes (eas-vl-gallery-mask-boxes "emoji" scene)))
    (should (equal boxes '([37.5 27.5 25.0 25.0])))
    (should-error (eas-vl-gallery-mask-boxes "glyphs" scene) :type 'eas-error))
  (let* ((img (list :w 2 :h 2 :rgba (concat (unibyte-string 9 9 9 255) (make-string 12 0))))
         (masked (eas-vl-gallery-mask-image img '([1 0 1 2]))))
    (should (equal (plist-get masked :rgba)
                   (concat (unibyte-string 9 9 9 255 9 9 9 255) (make-string 4 0) (unibyte-string 9 9 9 255))))
    ;; The original image is untouched.
    (should (equal (substring (plist-get img :rgba) 4 8) (make-string 4 0)))))

;;; Memoized transforms

(ert-deftest eas-distributions-memo ()
  (let ((tb (eas-memo-table 2)) (calls 0))
    (dotimes (_ 3) (eas-memo tb 'a (cl-incf calls)))
    (should (= calls 1))
    (eas-memo tb 'b 0) (eas-memo tb 'c 0)
    (should (= (hash-table-count (cdr tb)) 1))
    (eas-memo-clear)
    (should (= (hash-table-count (cdr tb)) 0))))

(ert-deftest eas-distributions-memo-is-exact ()
  "Memoized bootstrap intervals, densities and time floors equal fresh ones."
  (let ((nums [3 1 4 1 5 9 2 6]))
    (eas-memo-clear)
    (let ((a (eas-agg-bootstrap-ci (append nums nil))))
      (should (equal a (eas-agg--bootstrap nums)))
      (should (eq a (eas-agg-bootstrap-ci (append nums nil))))))
  (let ((values '(1.0 2.0 2.5 4.0)))
    (should (equal (eas-density--samples values 0.7 0.0 5.0 10 nil)
                   (vconcat (cl-loop for i to 10 collect (eas-density--pdf values 0.7 (* i 0.5))))))
    (should (eq (eas-density--samples values 0.7 0.0 5.0 10 nil)
                (eas-density--samples values 0.7 0.0 5.0 10 nil))))
  (let ((eas-time-zone "America/Chicago"))
    (should (equal (eas-time-unit-floor "yearmonth" "2004-03-17")
                   (eas-time-unit--floor "yearmonth" "2004-03-17"))))
  (let ((eas-time-zone "UTC"))
    (should (equal (eas-time-unit-floor "yearmonth" "2004-03-17")
                   (eas-time-unit--floor "yearmonth" "2004-03-17")))))

;;; Colors and bins

(ert-deftest eas-distributions-explicit-color-range ()
  "A named-color range compiles and interpolates in HCL, as Vega-Lite's does."
  (should (equal (eas-color-hex "White") "#ffffff"))
  (should (equal (eas-color-hex "#AbC") "#aabbcc"))
  (should-not (eas-color-hex "no-such-color"))
  (let* ((scene (eas-compile '(:data (:values [(:a "x" :v 0) (:a "y" :v 5) (:a "z" :v 10)]) :mark "rect"
                               :encoding (:x (:field "a" :type "nominal")
                                          :color (:field "v" :type "quantitative" :scale (:range ["white" "#e41a1c"]))))))
         (items (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :items)))
    ;; d3.interpolateHcl("white", "#e41a1c"): white has no chroma, so even
    ;; its end takes red's ("rgb(255, 190, 152)", as Vega draws it).
    (should (equal (mapcar (lambda (i) (plist-get i :fill)) items) '("#ffbe98" "#ff7358" "#e41a1c")))))

(ert-deftest eas-distributions-prebinned-axis-ticks ()
  "Pre-binned fields without a step tick like any linear axis (size/40)."
  (let* ((scene (eas-compile '(:width 480 :data (:values [(:s 1 :e 2 :n 3) (:s 9 :e 10 :n 4)]) :mark "bar"
                               :encoding (:x (:field "s" :type "quantitative" :bin "binned" :scale (:zero :false))
                                          :x2 (:field "e") :y (:field "n" :type "quantitative")))))
         (axis (seq-find (lambda (a) (equal (plist-get a :channel) "x"))
                         (plist-get (aref (plist-get scene :views) 0) :axes))))
    (should (equal (mapcar (lambda (tk) (plist-get tk :label)) (plist-get axis :ticks))
                   '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")))))

;;; The property check

(defun eas-agent-test--ok-data (env)
  "ENV's data, asserting it is ok."
  (should (eq (plist-get env :ok) t))
  (plist-get env :data))

;;; Surfaces and the bench

(ert-deftest eas-distributions-agent-reads-data-next-to-the-spec ()
  "A spec file's data URL resolves against the file, wherever Emacs runs."
  (let* ((file (expand-file-name "histogram.vl.json" (eas-vl-gallery-group-directory "distributions")))
         (default-directory temporary-file-directory)
         (env (eas-agent "render" file :backend "text" :cols 40 :rows 10)))
    (should (eq (plist-get env :ok) t))))

(ert-deftest eas-distributions-gallery-bench ()
  (let ((r (eas-vl-gallery-bench-example "distributions" "bar_percent_of_total" 1)))
    (should (= (plist-get r :rows) 5))
    (should (equal (eas-plist-keys (plist-get r :ms))
                   '(:compile-cold :compile-svg :render-svg :compile-text :render-text :hover)))))

(provide 'eas-distributions-test)
;;; eas-distributions-test.el ends here
