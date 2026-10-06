;;; eas-render-test.el --- tests for the SVG and text renderers -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-svg)
(require 'eas-text)

(defconst eas-render-test--specs
  `(("line" . ,(lambda () (eas-resolve "line" (plist-put (eas-template-example "line") :points t))))
    ("bars" . ,(lambda () (eas-resolve "bars" (eas-template-example "bars"))))
    ("stacked" . ,(lambda ()
                    '(:data (:values [(:k "a" :c "x" :v 1) (:k "a" :c "y" :v 2) (:k "b" :c "x" :v 3) (:k "b" :c "y" :v 1)])
                      :vconcat [(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                                        :color (:field "c" :type "nominal")))
                                (:mark "point" :encoding (:x (:field "v" :type "quantitative") :y (:field "k" :type "nominal")))])))
    ("area" . ,(lambda () '(:data (:values [(:t 1 :v 3) (:t 2 :v 5) (:t 3 :v 4) (:t 4 :v 8) (:t 5 :v 6)])
                            :mark "area" :encoding (:x (:field "t" :type "quantitative") :y (:field "v" :type "quantitative")))))
    ("hbar" . ,(lambda () '(:data (:values [(:k "alpha" :v 3.3) (:k "beta" :v 7.9) (:k "gamma" :v 5.2)])
                            :mark "bar" :encoding (:y (:field "k" :type "nominal") :x (:field "v" :type "quantitative"))))))
  "Specs rendered by both backends for goldens.")

(ert-deftest eas-render-text-goldens ()
  (dolist (entry eas-render-test--specs)
    (eas-test-golden (format "text-%s.txt" (car entry))
                       (concat (substring-no-properties
                                (eas-text-render (eas-compile (funcall (cdr entry)) :target 'text
                                                                  :size '(:cols 60 :rows 16))))
                               "\n"))))

(ert-deftest eas-render-svg-goldens ()
  (dolist (entry eas-render-test--specs)
    (eas-test-golden (format "svg-%s.svg" (car entry))
                       (concat (eas-svg-render (eas-compile (funcall (cdr entry)))) "\n"))))

(ert-deftest eas-render-text-is-deterministic ()
  (let ((spec (funcall (cdr (assoc "bars" eas-render-test--specs)))))
    (should (equal (eas-text-render (eas-compile spec :target 'text :size '(:cols 40 :rows 12)))
                   (eas-text-render (eas-compile spec :target 'text :size '(:cols 40 :rows 12)))))))

(defun eas-render-test--find (text prop value)
  "Position of the first char of TEXT whose PROP equals VALUE."
  (cl-loop for i from 0 below (length text)
           when (equal (get-text-property i prop text) value) return i))

(ert-deftest eas-render-text-cells-carry-datum-and-help-echo ()
  (let* ((scene (eas-compile (funcall (cdr (assoc "bars" eas-render-test--specs)))
                               :target 'text :size '(:cols 50 :rows 14)))
         (text (eas-text-render scene))
         (pos (eas-render-test--find text 'eas-datum 4)))
    (should pos)
    (should (equal (get-text-property pos 'eas-view text) "main"))
    (should (equal (get-text-property pos 'eas-mark text) "main/0"))
    (should (equal (get-text-property pos 'help-echo text) "category: Fri\nvalue: 12050"))
    (should (equal (plist-get (get-text-property pos 'face text) :foreground)
                   (eas-theme-get eas-theme-default :mark :color)))))

(ert-deftest eas-render-text-line-cells-map-to-nearest-datum ()
  (let* ((scene (eas-compile (eas-resolve "line" (eas-template-example "line"))
                               :target 'text :size '(:cols 60 :rows 16)))
         (text (eas-text-render scene))
         (datums (delete-dups (cl-loop for i from 0 below (length text)
                                       for d = (get-text-property i 'eas-datum text)
                                       when d collect d))))
    (should (equal (sort datums #'<) '(0 1 2 3 4 5 6 7)))))

(ert-deftest eas-render-text-legend-entries-are-tagged ()
  (let* ((text (eas-text-render (eas-compile (funcall (cdr (assoc "stacked" eas-render-test--specs)))
                                                 :target 'text :size '(:cols 50 :rows 20)))))
    (should (eas-render-test--find text 'eas-legend "y"))))

(ert-deftest eas-render-svg-hot-spots-for-discrete-items-and-legends ()
  (let* ((scene (eas-compile (funcall (cdr (assoc "stacked" eas-render-test--specs)))))
         (image (eas-svg-image scene :scale 1))
         (map (plist-get (cdr image) :map)))
    (should (eq (car image) 'image))
    (should (eq (plist-get (cdr image) :type) 'svg))
    (should (= (length map) (+ 4 4 2)))
    (should (assq 'eas:vconcat_0|vconcat_0/0|0 (mapcar (lambda (a) (cons (nth 1 a) a)) map)))
    (should (seq-find (lambda (a) (string-prefix-p "eas-legend:" (symbol-name (nth 1 a)))) map))
    (let ((area (seq-find (lambda (a) (eq (car (car a)) 'rect)) map)))
      (should (plist-get (nth 2 area) 'pointer)))))

(ert-deftest eas-render-svg-series-are-single-paths ()
  (let* ((values (vconcat (mapcar (lambda (i) (list :x i :y (% (* i 7) 13))) (number-sequence 0 999))))
         (svg (eas-svg-render (eas-compile (list :data (list :values values) :mark "line"
                                                     :encoding '(:x (:field "x" :type "quantitative")
                                                                 :y (:field "y" :type "quantitative")))))))
    (should (= (cl-count-if (lambda (_) t) (split-string svg "<path" t)) 2))))

(ert-deftest eas-render-svg-theme-is-a-vega-config ()
  (let* ((scene (eas-compile (funcall (cdr (assoc "bars" eas-render-test--specs)))))
         (svg (eas-svg-render scene '(:background "#111111" :axis (:labelColor "#eeeeee")))))
    (should (string-match-p "fill=\"#111111\"" svg))
    (should (string-match-p "fill=\"#eeeeee\"" svg))
    ;; Keys the theme leaves alone keep the scene's config (the default theme).
    (should (string-match-p (format "stroke=\"%s\"" (eas-theme-get eas-theme-default :axis :domainColor)) svg))))

(ert-deftest eas-render-svg-escapes-text ()
  (let ((svg (eas-svg-render (eas-compile '(:title "a < b & c" :data (:values [(:x 1)]) :mark "point"
                                                :encoding (:x (:field "x" :type "quantitative")))))))
    (should (string-match-p "a &lt; b &amp; c" svg))))

(ert-deftest eas-renderers-read-only-the-scene ()
  "Renderers depend on no spec, template or compile layer."
  (dolist (file '("src/eas-svg.el" "src/eas-text.el"))
    (with-temp-buffer
      (insert-file-contents (eas-test-file file))
      (dolist (layer '("eas-compile" "eas-spec" "eas-template" "eas-resolve" "eas-marks"))
        (goto-char (point-min))
        (should-not (re-search-forward (format "(require '%s)" layer) nil t))))))

(provide 'eas-render-test)
;;; eas-render-test.el ends here
