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
    (dolist (group groups)
      (dolist (name (eas-vl-gallery-custom-names group))
        (should (equal (cons (concat group "/" name) (eas-vl-gallery-custom-check group name))
                       (list (concat group "/" name))))))))

(provide 'eas-vl-gallery-custom-test)
;;; eas-vl-gallery-custom-test.el ends here
