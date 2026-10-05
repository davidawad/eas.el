;;; eas-vl-gallery-custom.el --- customization specs beside the official examples -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fc-qx1.42.  test/vl-examples/GROUP/custom/NAME.vl.json are specs of
;; our own, one per chart type of the group, setting non-default mark,
;; axis, legend, scale, title and config properties.  Unlike the
;; official examples they carry their verdict in usermeta.eas
;; ({"threshold": T, "note": "..."}) and are held to more:
;;
;;   - check is native with no warnings: every property they set is in
;;     the native subset and honored (eas-spec-props.el lists the ignored);
;;   - both backends render, without overlap at the gallery's three
;;     pixel and three cell sizes;
;;   - the text rendering equals the golden custom/NAME.txt
;;     (EAS_UPDATE_GOLDEN=1 or FINANCIAL_CHART_UPDATE_GOLDEN=1 rewrites it);
;;   - the native SVG is within the threshold of bin/chart's image.
;;     The harness builds custom/ref/NAME.png with bin/chart when it is
;;     on PATH; without it a committed reference is compared wherever a
;;     rasterizer exists, and with neither the image check is
;;     reported as unverified rather than passed.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-spec-props)
(require 'eas-vl-gallery)
(require 'eas-chart)
(require 'eas-png)

(defun eas-vl-gallery-custom-directory (group)
  "GROUP's directory of customization specs."
  (expand-file-name "custom" (eas-vl-gallery-group-directory group)))

(defun eas-vl-gallery-custom-groups ()
  "Groups with customization specs, sorted."
  (seq-filter (lambda (g) (eas-vl-gallery-custom-names g))
              (and (file-directory-p eas-vl-gallery-directory)
                   (directory-files eas-vl-gallery-directory nil "\\`[^.]"))))

(defun eas-vl-gallery-custom-names (group)
  "Customization spec names of GROUP, sorted."
  (let ((dir (eas-vl-gallery-custom-directory group)))
    (and (file-directory-p dir)
         (mapcar (lambda (f) (string-remove-suffix ".vl.json" f))
                 (directory-files dir nil "\\.vl\\.json\\'")))))

(defun eas-vl-gallery-custom-file (group name &optional ext)
  "File NAME.EXT (default .vl.json) of GROUP's customization specs."
  (expand-file-name (concat name (or ext ".vl.json")) (eas-vl-gallery-custom-directory group)))

(defun eas-vl-gallery-custom-ref (group name)
  "bin/chart's reference PNG for customization spec NAME of GROUP."
  (expand-file-name (concat "ref/" name ".png") (eas-vl-gallery-custom-directory group)))

(defun eas-vl-gallery-custom-spec (group name)
  "Customization spec NAME of GROUP with its data inlined."
  (let ((dir (eas-vl-gallery-custom-directory group)))
    (eas-vl-gallery-inline (eas-json-read-file (eas-vl-gallery-custom-file group name)) dir)))

(defun eas-vl-gallery-custom--findings (spec)
  "Check findings and ignored properties of SPEC, as problem strings."
  (mapcar (lambda (f) (format "%s at %s: %s" (plist-get f :code) (plist-get f :path) (plist-get f :message)))
          (append (let ((eas-spec-supported-function nil)) (eas-spec-check spec))
                  (eas-spec-props-findings spec))))

(defun eas-vl-gallery-custom-image (group name spec svg)
  "Judge native SVG of customization SPEC NAME in GROUP against bin/chart.
Return (:status pass|fail|unverified :detail D [:ratio R])."
  (let ((ref (eas-vl-gallery-custom-ref group name))
        (threshold (or (plist-get (plist-get (plist-get spec :usermeta) :eas) :threshold)
                       eas-vl-gallery-default-threshold)))
    (when (eas-chart-available-p)
      (make-directory (file-name-directory ref) t)
      (let ((process-environment (cons (concat "TZ=" eas-vl-gallery-zone) process-environment))
            (coding-system-for-write 'no-conversion))
        (with-temp-file ref
          (set-buffer-multibyte nil)
          (insert (eas-chart-build spec "png")))))
    (cond
     ((not (file-exists-p ref))
      (list :status "unverified" :detail (format "no reference: %s builds custom/ref/%s.png" eas-chart-program name)))
     ((not (eas-vl-gallery-rasterizer-p))
      (list :status "unverified" :detail (format "%s is not on PATH to rasterize native SVG" eas-chart-rsvg-program)))
     (t (let ((mine (make-temp-file "eas-custom" nil ".png")))
          (unwind-protect
              (let* ((cmp (progn (eas-chart-rasterize svg mine)
                                 (eas-png-compare (eas-png-read mine) (eas-png-read ref))))
                     (ratio (plist-get cmp :ratio)) (delta (plist-get cmp :size-delta)))
                (list :status (if (and (<= ratio threshold) (<= (max (abs (aref delta 0)) (abs (aref delta 1))) 8))
                                  "pass" "fail")
                      :ratio ratio
                      :detail (format "ratio %.4f (threshold %s), size delta %S" ratio threshold delta)))
            (delete-file mine)))))))

(defun eas-vl-gallery-custom-check (group name)
  "Problems of customization spec NAME of GROUP, as strings (nil when it
holds).  An image that cannot be verified here is no problem."
  (condition-case err
      (let* ((spec (eas-vl-gallery-custom-spec group name))
             (findings (eas-vl-gallery-custom--findings spec))
             (svg (eas-vl-gallery-svg spec))
             (text (eas-vl-gallery-text spec))
             (golden (eas-vl-gallery-custom-file group name ".txt"))
             (image (eas-vl-gallery-custom-image group name spec svg))
             problems)
        (when (or (getenv "EAS_UPDATE_GOLDEN") (getenv "FINANCIAL_CHART_UPDATE_GOLDEN"))
          (with-temp-file golden (set-buffer-file-coding-system 'utf-8-unix) (insert text)))
        (cond ((not (file-exists-p golden)) (push (format "%s: no text golden %s" name golden) problems))
              ((not (equal text (with-temp-buffer (insert-file-contents golden) (buffer-string))))
               (push (format "%s: text rendering differs from %s" name golden) problems)))
        (dolist (f findings) (push (format "%s: %s" name f) problems))
        (dolist (p (eas-vl-gallery-resize-problems spec)) (push (format "%s: %s" name p) problems))
        (when (equal (plist-get image :status) "fail")
          (push (format "%s: image %s" name (plist-get image :detail)) problems))
        (nreverse problems))
    (error (list (format "%s: %s" name (error-message-string err))))))

(provide 'eas-vl-gallery-custom)
;;; eas-vl-gallery-custom.el ends here
