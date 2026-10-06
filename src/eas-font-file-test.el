;;; eas-font-file-test.el --- user font files: metrics, layout, SVG -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The bundled test font is ABeeZee Regular (SIL OFL 1.1, see
;; test/eas/fonts/OFL.txt).  Its expected metrics were read with
;; fontTools: 250 mapped characters, 1000 units per em, "0" 600 units,
;; "W" 943, and "Hello, World 123" 8148 units in all.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-font-file-test--ttf (eas-test-file "test/eas/fonts/ABeeZee-Regular.ttf")
  "The bundled open font.")

(defmacro eas-font-file-test--with-registry (&rest body)
  "Run BODY with an empty font registry of its own."
  (declare (indent 0) (debug t))
  `(let ((eas-font-file--faces nil)) ,@body))

(defun eas-font-file-test--spec (&rest config)
  "A bar chart whose y labels are wider in ABeeZee than in Arial.
CONFIG is its config object."
  (append '(:data (:values [(:k "little" :v 1) (:k "tilt" :v 3)]) :mark "bar"
            :encoding (:y (:field "k" :type "nominal") :x (:field "v" :type "quantitative")))
          (and config (list :config config))))

(defun eas-font-file-test--x0 (scene)
  "The left edge of SCENE's first plot."
  (aref (plist-get (aref (plist-get scene :views) 0) :bounds) 0))

(ert-deftest eas-font-file-reads-the-bundled-font ()
  (let* ((m (eas-font-file-read eas-font-file-test--ttf)) (w (plist-get m :widths)))
    (should (equal (plist-get m :family) "ABeeZee"))
    (should (= (plist-get m :weight) 400))
    (should (equal (plist-get m :style) "normal"))
    (should (= (plist-get m :units-per-em) 1000))
    (should (= (hash-table-count w) 250))
    (should (equal (list (gethash ?0 w) (gethash ?W w) (gethash ?i w) (gethash ?\s w)) '(600 943 250 300)))
    (should (= (eas-font-file-width (list :metrics m) "Hello, World 123" 1000) 8148))))

(ert-deftest eas-font-file-rejects-what-it-cannot-read ()
  (should (eq (car (should-error (eas-font-file-read (eas-test-file "test/eas/fonts/none.ttf"))
                                 :type 'eas-not-found))
              'eas-not-found))
  (dolist (bytes '("wOFF\0\1\0\0rest-of-a-woff" "ttcf\0\1\0\0\0\0\0\0" "not a font at all"))
    (let ((err (should-error (eas-font-file-parse (encode-coding-string bytes 'binary) "x.ttf")
                             :type 'eas-invalid-input)))
      (should (equal (plist-get (cddr err) :code) "INVALID_INPUT"))
      (should (plist-get (cddr err) :next)))))

(ert-deftest eas-font-file-measures-a-registered-family ()
  (eas-font-file-test--with-registry
    (should (equal (eas-font-register eas-font-file-test--ttf) "ABeeZee"))
    ;; The first registered family of a CSS list wins, in any case.
    (dolist (family '("ABeeZee" "abeezee" "'ABeeZee', sans-serif" "Nope, \"ABeeZee\""))
      (let ((eas-font-family family))
        (should (< (abs (- (eas-font-text-width "Hello, World 123" 11) (* 8.148 11))) 1e-9))))
    ;; Bold has no face of its own: the regular face measures it.
    (let ((eas-font-family "ABeeZee"))
      (should (= (eas-font-text-width "Hi" 10 700) (eas-font-text-width "Hi" 10))))
    ;; Unregistered families keep the built-in tables.
    (let ((eas-font-family "sans-serif"))
      (should (< (abs (- (eas-font-text-width "100" 11) 18.353)) 0.001)))
    (eas-font-unregister "ABEEZEE")
    (let ((eas-font-family "ABeeZee"))
      (should (< (abs (- (eas-font-text-width "100" 11) 18.353)) 0.001)))))

(ert-deftest eas-font-file-register-picks-the-nearest-face ()
  (eas-font-file-test--with-registry
    (eas-font-register eas-font-file-test--ttf :family "Pair")
    (eas-font-register eas-font-file-test--ttf :family "Pair" :weight "bold")
    (eas-font-register eas-font-file-test--ttf :family "Pair" :weight 700)
    (should (= (length (eas-font-file-faces "Pair")) 2))
    (should (= (plist-get (eas-font-file-face "Pair" t) :weight) 700))
    (should (= (plist-get (eas-font-file-face "Pair") :weight) 400))
    (should-not (eas-font-file-face "Arial"))))

(ert-deftest eas-font-file-config-font-sizes-the-layout ()
  "config.font and axis.labelFont set the font labels are measured in."
  (eas-font-file-test--with-registry
    (let ((arial (eas-font-file-test--x0 (eas-compile (eas-font-file-test--spec :font "ABeeZee")))))
      (eas-font-register eas-font-file-test--ttf)
      (let ((wide (eas-font-file-test--x0 (eas-compile (eas-font-file-test--spec :font "ABeeZee"))))
            (labels (eas-font-file-test--x0 (eas-compile (eas-font-file-test--spec :axis '(:labelFont "ABeeZee"))))))
        (should (> wide arial))
        (should (= labels wide))))))

(ert-deftest eas-font-file-title-and-text-marks-measure-in-their-font ()
  (eas-font-file-test--with-registry
    (let* ((title (lambda (&optional font)
                    (let ((scene (eas-compile `(:width 20 :height 20 :data (:values [(:a 1)]) :mark "point"
                                                :title (:text "lttltlttltlttltlttl" :anchor "start"
                                                        ,@(and font (list :font font)))))))
                      (plist-get (plist-get scene :size) :w))))
           (text (lambda (&optional font)
                   (let* ((scene (eas-compile `(:data (:values [(:t "lttltl")]) :encoding (:text (:field "t"))
                                                :mark (:type "text" ,@(and font (list :font font))))))
                          (mark (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0))
                          (b (eas-marks-item-bounds "text" (aref (plist-get mark :items) 0)
                                                    (eas-layout-metrics 'svg))))
                     (- (aref b 2) (aref b 0)))))
           (title-arial (funcall title)) (text-arial (funcall text)))
      ;; Unregistered, the family is measured as Arial.
      (should (= (funcall title "ABeeZee") title-arial))
      (should (= (funcall text "ABeeZee") text-arial))
      (eas-font-register eas-font-file-test--ttf)
      (should (> (funcall title "ABeeZee") title-arial))
      (should (> (funcall text "ABeeZee") text-arial)))))

(ert-deftest eas-font-file-svg-carries-font-face ()
  (eas-font-file-test--with-registry
    (eas-font-register eas-font-file-test--ttf)
    (let* ((scene (eas-compile (eas-font-file-test--spec :font "ABeeZee")))
           (fonts (plist-get scene :fonts)))
      (should (equal fonts (vector (list :family "ABeeZee" :weight 400 :style "normal"
                                         :src eas-font-file-test--ttf :format "truetype"))))
      (let ((svg (let ((eas-svg-font-embed 'url)) (eas-svg-render scene))))
        (should (string-match-p "<style>@font-face{font-family:'ABeeZee';font-weight:400;font-style:normal;src:url('file:///[^']*/ABeeZee-Regular.ttf') format('truetype')}</style>" svg))
        (should (string-match-p "<svg [^>]*font-family=\"ABeeZee\"" svg)))
      (let ((svg (let ((eas-svg-font-embed 'data)) (eas-svg-render scene))))
        (should (string-match-p "src:url('data:font/ttf;base64,AAEAAA" svg))
        (should (> (length svg) 60000)))
      (let ((svg (let ((eas-svg-font-embed nil)) (eas-svg-render scene))))
        (should-not (string-search "@font-face" svg))))
    ;; A chart naming no registered family carries no fonts.
    (should-not (plist-member (eas-compile (eas-font-file-test--spec)) :fonts))))

(ert-deftest eas-font-file-spec-registers-its-fonts ()
  "x-eas.fonts registers at resolve; pure Vega-Lite keeps the names."
  (eas-font-file-test--with-registry
    (let* ((eas-font-file-directory (eas-test-file "test/eas"))
           (resolved (eas-resolve-spec
                      (append '(:x-eas (:fonts [(:src "fonts/ABeeZee-Regular.ttf" :family "Bee")]))
                              (eas-font-file-test--spec :font "Bee")))))
      (should-not (plist-member resolved :x-eas))
      (should (equal (plist-get (plist-get resolved :config) :font) "Bee"))
      (should (equal (plist-get (aref (plist-get (eas-compile resolved) :fonts) 0) :src) eas-font-file-test--ttf)))
    (dolist (bad '((:fonts (:src "a.ttf")) (:fonts [(:family "x")])))
      (should (equal (plist-get (cddr (should-error (eas-resolve-spec (append (list :x-eas bad) (eas-font-file-test--spec)))
                                                    :type 'eas-invalid-input))
                                :code)
                     "INVALID_INPUT")))))

(ert-deftest eas-font-file-text-backend-ignores-fonts ()
  "The text backend draws in the frame's faces: fonts change nothing."
  (eas-font-file-test--with-registry
    (let ((plain (eas-text-render (eas-compile (eas-font-file-test--spec) :target 'text :size '(:cols 40 :rows 10)))))
      (eas-font-register eas-font-file-test--ttf)
      (should (equal (eas-text-render (eas-compile (eas-font-file-test--spec :font "ABeeZee" :title '(:font "ABeeZee"))
                                                   :target 'text :size '(:cols 40 :rows 10)))
                     plain)))))

(provide 'eas-font-file-test)
;;; eas-font-file-test.el ends here
