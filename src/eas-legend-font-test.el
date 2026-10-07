;;; eas-legend-font-test.el --- tests for a legend's own fonts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'eas-test-support)
(require 'eas)

(ert-deftest eas-legend-font-of-one-legend-is-drawn ()
  "A legend's own titleFont and labelFont reach its SVG text.
config.legend sets them for every legend; the legend object for one."
  (let* ((spec (eas-json-parse
                (json-encode `((data (values . [((a . 1) (b . "x")) ((a . 5) (b . "y"))]))
                               (mark . "point")
                               (encoding (x (field . "a") (type . "quantitative"))
                                         (color (field . "b") (type . "nominal")
                                                (legend (titleFont . "Tahoma")
                                                        (labelFont . "Courier New")
                                                        (titleFontStyle . "italic"))))))))
         (svg (eas-svg-render (eas-compile spec))))
    (should (string-match-p "font-family=\"Tahoma\"[^>]*>b</text>" svg))
    (should (string-match-p "font-style=\"italic\"[^>]*>b</text>" svg))
    (should (string-match-p "font-family=\"Courier New\"[^>]*>x</text>" svg))))

(provide 'eas-legend-font-test)
;;; eas-legend-font-test.el ends here
