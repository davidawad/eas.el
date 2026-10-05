;;; eas-conformance-test.el --- conformance gallery, references, supported.json, fallback -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-agent)

(defconst eas-conformance-test-unproven '("encoding/order")
  "Recognised features no gallery spec proves yet (so they fall back).")

(defun eas-conformance-test--rasterizer-p ()
  "Non-nil when native SVG can be rasterized and decoded here."
  (and (executable-find eas-chart-rsvg-program) (zlib-available-p)))

(defvar eas-conformance-test--results nil
  "Gallery results of this session, with the oracle when it can run.")

(defun eas-conformance-test--results ()
  "Run the gallery once per session (with the oracle when rsvg-convert exists)."
  (or eas-conformance-test--results
      (setq eas-conformance-test--results
            (eas-conformance-run (eas-conformance-test--rasterizer-p)))))

(defun eas-conformance-test--text (entry)
  "ENTRY's text rendering at the golden size."
  (let ((eas-spec-supported-function nil))
    (concat (substring-no-properties
             (eas-text-render (eas-compile (plist-get entry :spec) :target 'text
                                               :size eas-conformance-text-size)))
            "\n")))

(defun eas-conformance-test--write-text-goldens ()
  "Rewrite every gallery text golden (EAS_UPDATE_GOLDEN)."
  (dolist (entry (eas-conformance-gallery))
    (with-temp-file (eas-conformance-text-file entry)
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (eas-conformance-test--text entry)))))

(ert-deftest eas-conformance-text-goldens ()
  "Every gallery spec compiles natively and matches its exact text golden."
  (if (getenv "EAS_UPDATE_GOLDEN")
      (eas-conformance-test--write-text-goldens)
    (dolist (entry (eas-conformance-gallery))
      (should (equal (cons (plist-get entry :name) (eas-conformance-test--text entry))
                     (cons (plist-get entry :name)
                           (with-temp-buffer (insert-file-contents (eas-conformance-text-file entry))
                                             (buffer-string))))))))

(ert-deftest eas-conformance-supported-json-is-current ()
  "supported.json lists exactly the features the passing gallery proves.
Its oracle verdicts are checked too wherever the oracle can run."
  (when (getenv "EAS_UPDATE_GOLDEN")
    ;; supported.json counts only specs that match their text golden.
    (eas-conformance-test--write-text-goldens)
    (setq eas-conformance-test--results nil))
  (let* ((results (eas-conformance-test--results))
         (generated (eas-conformance-supported-data results))
         (pairs (lambda (data key) (cl-loop for (k v) on (plist-get data :features) by #'cddr
                                            collect (cons k (plist-get v key))))))
    (should (seq-every-p (lambda (r) (plist-get r :ok)) results))
    (if (getenv "EAS_UPDATE_GOLDEN")
        (with-temp-file eas-conformance-supported-file (insert (eas-json-pretty generated)))
      (let ((committed (eas-conformance-supported)))
        (should committed)
        (should (equal (funcall pairs generated :specs) (funcall pairs committed :specs)))
        (when (eas-conformance-test--rasterizer-p)
          (should (equal (funcall pairs generated :oracle) (funcall pairs committed :oracle))))))))

(ert-deftest eas-conformance-every-native-feature-is-proven ()
  "Each mark, channel, scale type, transform and param form has a gallery spec."
  (let ((supported (eas-conformance-supported-features))
        (vocabulary (append (mapcar (lambda (m) (concat "mark/" m)) eas-spec--marks)
                            (mapcar (lambda (c) (concat "encoding/" (eas-key-name c))) eas-spec--channels)
                            (mapcar (lambda (s) (concat "scale/" s)) eas-spec--scale-types)
                            (mapcar (lambda (tr) (concat "transform/" (eas-key-name tr)))
                                    (remq :x-eas:transform eas-spec--transforms))
                            '("composition/layer" "composition/vconcat" "composition/hconcat"
                              "param/point" "param/interval" "param/value" "bind/scales" "bind/legend"
                              "encoding/condition" "encoding/aggregate" "encoding/bin" "encoding/timeUnit"))))
    (should (listp supported))
    (should (equal (seq-remove (lambda (f) (or (member f supported) (member f eas-conformance-test-unproven)))
                               vocabulary)
                   nil))
    (dolist (f eas-conformance-test-unproven) (should-not (member f supported)))))

(ert-deftest eas-conformance-check-reads-supported-json ()
  (let ((findings (eas-spec-check '(:data (:values [(:a 1 :b 2)]) :mark "line"
                                      :encoding (:x (:field "a") :y (:field "b") :order (:field "a"))))))
    (should (equal (mapcar (lambda (f) (list (plist-get f :code) (plist-get f :path))) findings)
                   '(("UNSUPPORTED_FEATURE" "/encoding/order")))))
  (should (plist-get (plist-get (eas-describe 'supported) :supported) :features)))

;;; The oracle

(ert-deftest eas-conformance-oracle-agrees-with-bin-chart ()
  "Native SVG vs bin/chart's image within each spec's threshold and size tolerance.
The images are the committed references, or fresh builds when bin/chart
is on PATH; only rsvg-convert is needed."
  (unless (zlib-available-p) (eas-test-skip "this Emacs lacks zlib, needed to decode PNGs"))
  (unless (eas-conformance-test--rasterizer-p)
    (eas-test-skip (format "%s not on PATH; needed to rasterize native SVG for the image oracle"
                             eas-chart-rsvg-program)))
  (let ((failures (seq-remove (lambda (r) (equal (plist-get (plist-get r :oracle) :status) "pass"))
                              (eas-conformance-test--results))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :name) (plist-get (plist-get r :oracle) :detail)))
                           failures)
                   nil))))

(ert-deftest eas-conformance-references-match-the-gallery ()
  "Every gallery spec has a reference built from its current text, and no other."
  (should (equal (mapcar (lambda (e) (list (plist-get e :name) (eas-conformance-ref-problem e)))
                         (seq-filter #'eas-conformance-ref-problem (eas-conformance-gallery)))
                 nil))
  (should (equal (sort (mapcar #'eas-key-name (eas-plist-keys (plist-get (eas-conformance-manifest) :refs)))
                       #'string<)
                 (sort (mapcar (lambda (e) (plist-get e :name)) (eas-conformance-gallery)) #'string<)))
  (should (stringp (eas-conformance-ref-zone))))

(ert-deftest eas-conformance-usermeta-does-not-stale-a-reference ()
  (let* ((entry (car (eas-conformance-gallery)))
         (spec (plist-get entry :spec)))
    (should (equal (eas-conformance-spec-hash spec)
                   (eas-conformance-spec-hash (plist-put (copy-sequence spec) :usermeta '(:eas (:threshold 0.5))))))
    (should-not (equal (eas-conformance-spec-hash spec)
                       (eas-conformance-spec-hash (plist-put (copy-sequence spec) :mark "point"))))))

(defconst eas-conformance-test--theme-file
  (eas-test-file "test/conformance/bin-chart-default-theme.json")
  "bin/chart's default theme, vendored.")

(ert-deftest eas-conformance-default-theme-is-bin-charts ()
  "The native default theme is exactly the vendored `chart theme --json'."
  (let ((vendored (eas-json-read-file eas-conformance-test--theme-file)))
    (should (equal (eas-json-canonical eas-theme-default) (eas-json-canonical (plist-get vendored :config))))
    (should (equal (eas-content-hash eas-theme-default) (plist-get vendored :hash)))))

(ert-deftest eas-conformance-vendored-theme-matches-bin-chart ()
  "The vendored theme is what the installed bin/chart reports."
  (eas-test-require-chart)
  (should (equal (plist-get (eas-chart-theme) :hash)
                 (plist-get (eas-json-read-file eas-conformance-test--theme-file) :hash))))

;;; Plumbing with stand-in programs (not an oracle: they only exercise the harness)

(defun eas-conformance-test--script (body)
  "An executable shell script running BODY."
  (let ((file (make-temp-file "eas-stub" nil ".sh" (concat "#!/bin/sh\n" body "\n"))))
    (set-file-modes file #o755)
    file))

(defun eas-conformance-test--ref (entry)
  "ENTRY's committed reference PNG."
  (expand-file-name (concat (plist-get entry :name) ".png") (eas-conformance-ref-directory)))

(defmacro eas-conformance-test--with-stubs (png chart &rest body)
  "Run BODY with an rsvg-convert stub writing PNG, and a bin/chart stub
writing PNG too when CHART is non-nil (otherwise no bin/chart)."
  (declare (indent 2))
  `(let* ((eas-chart-rsvg-program (eas-conformance-test--script (format "cp '%s' \"$2\"" ,png)))
          (eas-chart-program (if ,chart (eas-conformance-test--script (format "cp '%s' \"$4\"" ,png))
                                 "no-such-chart-program")))
     (unwind-protect (progn ,@body)
       (delete-file eas-chart-rsvg-program)
       (when ,chart (delete-file eas-chart-program)))))

(ert-deftest eas-conformance-oracle-plumbing ()
  (let* ((entry (car (eas-conformance-gallery)))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"))
         (ref (eas-conformance-test--ref entry)))
    (eas-conformance-test--with-stubs ref nil
      (let ((r (eas-conformance-oracle entry native)))
        (should (equal (plist-get r :status) "pass"))
        (should (equal (plist-get r :source) "ref"))
        (should (= (plist-get r :ratio) 0))))
    (eas-conformance-test--with-stubs ref t
      (should (equal (plist-get (eas-conformance-oracle entry native) :source) "bin/chart")))
    (let ((eas-chart-rsvg-program "no-such-rsvg-convert"))
      (should (equal (plist-get (eas-conformance-oracle entry native) :status) "unverified")))))

(ert-deftest eas-conformance-oracle-fails-stale-references ()
  (let* ((entry (car (eas-conformance-gallery)))
         (stale (plist-put (copy-sequence entry) :spec
                           (plist-put (copy-sequence (plist-get entry :spec)) :description "edited")))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")))
    (should (string-prefix-p "STALE_REF" (eas-conformance-ref-problem stale)))
    (dolist (chart '(nil t))
      (eas-conformance-test--with-stubs (eas-conformance-test--ref entry) chart
        (let ((r (eas-conformance-oracle stale native)))
          (should (equal (plist-get r :status) "fail"))
          (should (string-prefix-p "STALE_REF" (plist-get r :detail))))))))

(ert-deftest eas-conformance-oracle-bounds-the-size-delta ()
  "A canvas size difference beyond the tolerance fails even when pixels agree."
  (let* ((entries (eas-conformance-gallery))
         (entry (seq-find (lambda (e) (equal (plist-get e :name) "mark-bar")) entries))
         (other (seq-find (lambda (e) (equal (plist-get e :name) "mark-tick")) entries))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")))
    (eas-conformance-test--with-stubs (eas-conformance-test--ref other) nil
      (let ((r (eas-conformance-oracle entry native)))
        (should (equal (plist-get r :status) "fail"))
        (should (string-match-p "beyond" (plist-get r :detail)))))))

(ert-deftest eas-conformance-native-svg-uses-the-reference-zone ()
  "Native SVG is compiled in the references' time zone, the text in UTC."
  (let* ((entry (seq-find (lambda (e) (equal (plist-get e :name) "encoding-timeunit")) (eas-conformance-gallery)))
         (native (eas-conformance-native entry)))
    (should (equal (eas-conformance-ref-zone) "America/Chicago"))
    ;; 2026-03-02 is UTC midnight, still 1 March in Chicago, as Vega shows it.
    (should (string-match-p ">Mar 01, 2026<" (plist-get native :svg)))
    (should (string-match-p "Mar 02, 2026" (plist-get native :text)))))

;;; Static fallback

(defconst eas-conformance-test--arc
  '(:data (:values [(:k "a" :v 1) (:k "b" :v 2)]) :mark "arc"
    :encoding (:theta (:field "v" :type "quantitative") :color (:field "k" :type "nominal")))
  "A pie chart: outside the native subset.")

(ert-deftest eas-conformance-unsupported-specs-fall-back-to-static ()
  (let ((eas-views (make-hash-table :test 'equal))
        (eas-static-fallback t)
        (eas-chart-program "no-such-chart-program"))
    (let* ((view (eas-view-open eas-conformance-test--arc :id "pie"))
           (inspect (eas-inspect view)))
      (should (eq (plist-get inspect :interactive) :false))
      (should (equal (mapcar (lambda (w) (plist-get w :path)) (plist-get inspect :warnings))
                     '("/mark" "/encoding/theta")))
      (should (equal (plist-get (plist-get (plist-get inspect :static) :error) :code) "NOT_FOUND"))
      (eas-dispatch view '(:type "key" :key "+"))
      (should (equal (plist-get (eas-inspect view) :last-event) "key +"))
      (let ((buffer (eas-show view 'text)))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "Static chart (not interactive)" (buffer-string)))
              (should (string-match-p "mark/arc at /mark" (buffer-string))))
          (kill-buffer buffer))))))

(ert-deftest eas-conformance-fallback-takes-bin-chart-image ()
  (eas-conformance-test--with-stubs (eas-conformance-test--ref (car (eas-conformance-gallery))) t
   (let ((eas-views (make-hash-table :test 'equal))
         (eas-static-fallback t))
     (let ((inspect (eas-inspect (eas-view-open eas-conformance-test--arc :id "pie"))))
       (should (equal (plist-get inspect :static) '(:source "bin/chart" :type "svg")))))))

(ert-deftest eas-conformance-static-fallback-is-opt-in ()
  "By default bin/chart never runs to display a chart, even when installed:
the view shows UNSUPPORTED_FEATURE text and svg render fails."
  (should-not (default-value 'eas-static-fallback))
  (let* ((marker (make-temp-file "eas-chart-ran"))
         (eas-chart-program (eas-conformance-test--script (format "echo ran > '%s'; exit 1" marker))))
    (delete-file marker)
    (unwind-protect
        (let ((eas-views (make-hash-table :test 'equal)))
          (let* ((view (eas-view-open eas-conformance-test--arc :id "pie"))
                 (static (plist-get (eas-inspect view) :static)))
            (should (eq (plist-get (eas-inspect view) :interactive) :false))
            (should (eq (plist-get static :source) :null))
            (should (equal (plist-get (plist-get static :error) :code) "UNSUPPORTED_FEATURE"))
            (should (string-match-p "eas-static-fallback" (plist-get (plist-get static :error) :message)))
            (let ((buffer (eas-show view 'text)))
              (unwind-protect
                  (with-current-buffer buffer
                    (should (string-match-p "No static image (UNSUPPORTED_FEATURE)" (buffer-string)))
                    (should (string-match-p "mark/arc at /mark" (buffer-string))))
                (kill-buffer buffer))))
          (let ((env (eas-agent "render" (eas-json-encode eas-conformance-test--arc) :backend "svg")))
            (should (eq (plist-get env :ok) :false))
            (should (equal (plist-get env :reason) "UNSUPPORTED_FEATURE")))
          (should-not (file-exists-p marker)))
      (delete-file eas-chart-program)
      (when (file-exists-p marker) (delete-file marker)))))

(provide 'eas-conformance-test)
;;; eas-conformance-test.el ends here
