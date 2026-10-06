;;; eas-legend-merge-test.el --- tests for the merged color and size legend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-legend-merge-test--legends (encoding)
  "The legends of a point chart of ENCODING, as (TYPE TITLE ENTRIES)."
  (let* ((spec (eas-json-parse
                (json-encode `((data (values . [((a . 1) (b . 2)) ((a . 5) (b . 9)) ((a . 9) (b . 4))]))
                               (mark . "point")
                               (encoding (x (field . "a") (type . "quantitative")) ,@encoding)))))
         (scene (eas-compile spec)))
    (mapcar (lambda (l) (list (plist-get l :type) (plist-get l :title) (plist-get l :entries)))
            (plist-get (aref (plist-get scene :views) 0) :legends))))

(ert-deftest eas-legend-merge-color-and-size-of-one-field ()
  "A continuous color and a size of one field draw one symbol legend."
  (let ((legends (eas-legend-merge-test--legends
                  '((color (field . "a") (type . "quantitative"))
                    (size (field . "a") (type . "quantitative"))))))
    (should (= (length legends) 1))
    (pcase-let ((`(,type ,title ,entries) (car legends)))
      (should (equal type "symbol"))
      (should (equal title "a"))
      (let ((sizes (mapcar (lambda (e) (plist-get e :size)) entries))
            (colors (mapcar (lambda (e) (plist-get e :color)) entries)))
        ;; Each entry is sized and colored: sizes grow, colors vary.
        (should (equal sizes (sort (copy-sequence sizes) #'<)))
        (should (cl-every #'stringp colors))
        (should (> (length (delete-dups (copy-sequence colors))) 1))))))

(ert-deftest eas-legend-merge-keeps-other-fields-and-null-legends-apart ()
  "Different fields, or a null size legend, keep the color a gradient."
  (let ((legends (eas-legend-merge-test--legends
                  '((color (field . "a") (type . "quantitative"))
                    (size (field . "b") (type . "quantitative"))))))
    (should (equal (mapcar #'car legends) '("gradient" "symbol"))))
  (let ((legends (eas-legend-merge-test--legends
                  '((color (field . "a") (type . "quantitative"))
                    (size (field . "a") (type . "quantitative") (legend))))))
    (should (equal (mapcar #'car legends) '("gradient")))))

(ert-deftest eas-legend-merge-legend-values-pick-the-entries ()
  "The color legend's values pick the merged legend's entries."
  (let ((legends (eas-legend-merge-test--legends
                  '((color (field . "a") (type . "quantitative") (legend (values . [1 3 9])))
                    (size (field . "a") (type . "quantitative"))))))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) (nth 2 (car legends)))
                   '("1" "3" "9")))))

(ert-deftest eas-legend-merge-piecewise-linear-scale ()
  "A linear scale of more than two stops maps piecewise, as d3 does."
  (let ((s (list :type "linear" :domain [1.0 2.0 4.0] :range [0 10 30])))
    (should (= (eas-scale-apply s 1.5) 5.0))
    (should (= (eas-scale-apply s 3) 20.0))
    ;; The end segments extrapolate.
    (should (= (eas-scale-apply s 5) 40.0))
    (should (= (funcall (eas-scale-fn s) 3) 20.0))))

(provide 'eas-legend-merge-test)
;;; eas-legend-merge-test.el ends here
