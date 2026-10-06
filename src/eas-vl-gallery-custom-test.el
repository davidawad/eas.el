;;; eas-vl-gallery-custom-test.el --- customization specs of every gallery group -*- lexical-binding: t; -*-

;;; Commentary:

;; test/vl-examples/GROUP/custom/*.vl.json are eas's own specs, one per
;; chart type of a group, setting non-default properties (fc-qx1.38 to
;; .46).  Each checks clean, renders natively for both backends, lays out
;; without overlap, matches its text golden and, where bin/chart and a
;; rasterizer are present, its picture (eas-vl-gallery-custom.el).

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery-custom)

(ert-deftest eas-vl-gallery-custom-specs-hold ()
  (let ((groups (eas-vl-gallery-custom-groups)))
    (dolist (g '("area-circular" "calculations" "bar"))
      (should (member g groups)))
    ;; Every spec's problems at once, not just the first failing spec's.
    (should (equal (cl-loop for group in groups
                            append (cl-loop for name in (eas-vl-gallery-custom-names group)
                                            for problems = (eas-vl-gallery-custom-check group name)
                                            when problems collect (cons (concat group "/" name) problems)))
                   nil))))

(ert-deftest eas-vl-gallery-custom-builds-only-missing-or-stale-refs ()
  ;; With bin/chart on PATH the harness builds a reference only when it
  ;; is missing or its spec changed; an up-to-date one is never rewritten.
  (let* ((root (make-temp-file "eas-custom-gallery" t))
         (eas-vl-gallery-directory root)
         (dir (expand-file-name "g/custom" root))
         (spec-file (expand-file-name "s.vl.json" dir))
         (builds 0))
    (unwind-protect
        (cl-letf (((symbol-function 'eas-chart-available-p) (lambda () t))
                  ((symbol-function 'eas-chart-build)
                   (lambda (_spec _format) (setq builds (1+ builds)) (format "png %d" builds))))
          (make-directory dir t)
          (cl-flet ((write-spec (title)
                      (with-temp-file spec-file
                        (insert (eas-json-encode (list :mark "point" :title title :data (list :values [(:a 1)])
                                                       :encoding (list :x (list :field "a" :type "quantitative")))))))
                    (build () (eas-vl-gallery-custom-build-ref "g" "s" (eas-vl-gallery-custom-spec "g" "s")))
                    (ref () (with-temp-buffer (insert-file-contents (eas-vl-gallery-custom-ref "g" "s"))
                                              (buffer-string))))
            (write-spec "one")
            (should (eq (eas-vl-gallery-custom-ref-state "g" "s" (eas-vl-gallery-custom-spec "g" "s")) 'missing))
            (should (build))
            (should (equal (ref) "png 1"))
            (should (eq (eas-vl-gallery-custom-ref-state "g" "s" (eas-vl-gallery-custom-spec "g" "s")) 'current))
            (should-not (build))
            (should (equal (ref) "png 1"))
            ;; usermeta (thresholds and their reasons) is not what bin/chart draws.
            (let ((spec (eas-json-read-file spec-file)))
              (with-temp-file spec-file
                (insert (eas-json-encode (plist-put spec :usermeta '(:eas (:threshold 0.05)))))))
            (should-not (build))
            (write-spec "two")
            (should (eq (eas-vl-gallery-custom-ref-state "g" "s" (eas-vl-gallery-custom-spec "g" "s")) 'stale))
            (should (build))
            (should (equal (ref) "png 2"))
            (should (= builds 2))))
      (delete-directory root t))))

(provide 'eas-vl-gallery-custom-test)
;;; eas-vl-gallery-custom-test.el ends here
