;;; eas-marks-image.el --- the image mark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4/L5.  Vega-Lite's image mark draws the picture its url
;; channel (or mark.url) names, mark.width by mark.height pixels
;; (default 50), centred on x/y (align "center", baseline "middle").
;; Items are rects (:x :y :w :h) plus :url, so bounds, hit-testing and
;; translation treat them as rects.  The SVG renderer embeds local
;; images as data: URIs, resolving relative urls against
;; `eas-image-base-directory'; the character grid shows one glyph.

;;; Code:

(require 'eas-core)
(require 'eas-encode)

(declare-function eas-marks--pos "eas-marks")
(declare-function eas-marks--style "eas-marks")
(declare-function eas-marks--extras "eas-marks")

(defvar eas-image-base-directory nil
  "Directory relative image urls resolve against; nil means `default-directory'.")

(defconst eas-marks-image-glyph ?▣ "The image mark on the character grid.")

(defun eas-marks-image-row (unit scales bounds)
  "Row builder (ROW I -> item) for UNIT's image mark under SCALES in BOUNDS."
  (let* ((mark (plist-get unit :mark))
         (w (or (plist-get mark :width) 50)) (h (or (plist-get mark :height) 50))
         (align (or (plist-get mark :align) "center")) (baseline (or (plist-get mark :baseline) "middle"))
         (url-def (plist-get (plist-get unit :encoding) :url)))
    (lambda (row i)
      (let ((x (eas-marks--pos unit scales :x row bounds))
            (y (eas-marks--pos unit scales :y row bounds))
            (url (or (and url-def (eas-encode-raw url-def row)) (plist-get mark :url))))
        (when (and (numberp x) (numberp y) (stringp url))
          (append (list :datum i
                        :x (pcase align ("left" x) ("right" (- x w)) (_ (- x (/ w 2.0))))
                        :y (pcase baseline ("top" y) ("bottom" (- y h)) (_ (- y (/ h 2.0))))
                        :w w :h h :url url
                        :aspect (if (eq (plist-get mark :aspect) :false) :false t))
                  (eas-marks--extras unit row)))))))

(defun eas-marks-image--mime (file)
  "MIME type of image FILE by its extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ("png" "image/png") ((or "jpg" "jpeg") "image/jpeg") ("gif" "image/gif") ("svg" "image/svg+xml")
    (_ "application/octet-stream")))

(defun eas-marks-image-href (url)
  "URL as an SVG href: a data: URI when it names a readable local file."
  (let ((file (and (not (string-match-p "\\`[a-z]+:" url))
                   (expand-file-name url (or eas-image-base-directory default-directory)))))
    (if (and file (file-readable-p file))
        (concat "data:" (eas-marks-image--mime file) ";base64,"
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (base64-encode-region (point-min) (point-max) t)
                  (buffer-string)))
      url)))

(provide 'eas-marks-image)
;;; eas-marks-image.el ends here
