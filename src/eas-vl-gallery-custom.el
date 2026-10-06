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
;;     (EAS_UPDATE_GOLDEN=1 rewrites it);
;;   - the native SVG is within the threshold of bin/chart's image.
;;     References are committed: custom/ref/NAME.png, with the hash of
;;     the spec each was built from in custom/ref/manifest.json
;;     (`eas-conformance-spec-hash', usermeta excluded, data inlined).
;;     With bin/chart on PATH the harness builds a reference only when
;;     it is missing or its spec changed, and never rewrites an
;;     up-to-date one.  Without a reference the image check is
;;     "ref-pending", with a reference of an older spec "ref-stale",
;;     with no rasterizer "unverified"; none of them is a pass.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-spec-props)
(require 'eas-vl-gallery)
(require 'eas-chart)
(require 'eas-png)
(require 'eas-conformance-oracle)

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
  "Return bin/chart's reference PNG for customization spec NAME of GROUP."
  (expand-file-name (concat "ref/" name ".png") (eas-vl-gallery-custom-directory group)))

;;; References and their manifest

(defun eas-vl-gallery-custom-manifest-file (group)
  "GROUP's manifest of customization references."
  (expand-file-name "ref/manifest.json" (eas-vl-gallery-custom-directory group)))

(defun eas-vl-gallery-custom-manifest (group)
  "GROUP's parsed reference manifest, or nil when absent."
  (let ((file (eas-vl-gallery-custom-manifest-file group)))
    (and (file-exists-p file) (eas-json-read-file file))))

(defun eas-vl-gallery-custom-ref-state (group name spec)
  "State of the reference of customization SPEC NAME in GROUP.
Return `missing' without a reference PNG, `stale' when the manifest
records another spec hash for it, else `current'.  A committed PNG the
manifest does not know is current: nothing proves it is out of date."
  (let ((entry (plist-get (plist-get (eas-vl-gallery-custom-manifest group) :refs) (eas-key name))))
    (cond ((not (file-exists-p (eas-vl-gallery-custom-ref group name))) 'missing)
          ((and entry (not (equal (plist-get entry :spec_sha256) (eas-conformance-spec-hash spec)))) 'stale)
          (t 'current))))

(defun eas-vl-gallery-custom-record-ref (group name spec)
  "Record in GROUP's manifest that NAME's reference was built from SPEC."
  (let* ((manifest (eas-vl-gallery-custom-manifest group))
         (refs (eas-plist-put (plist-get manifest :refs) (eas-key name)
                              (list :png_sha256 (eas-conformance--file-hash (eas-vl-gallery-custom-ref group name))
                                    :spec_sha256 (eas-conformance-spec-hash spec)))))
    (with-temp-file (eas-vl-gallery-custom-manifest-file group)
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (eas-json-pretty
               (list :generator "bin/chart build SPEC --out ref/NAME.png, via eas-vl-gallery-custom-build-ref"
                     :spec_hash "eas-conformance-spec-hash of the spec with its data inlined"
                     :tz eas-vl-gallery-zone
                     :refs (eas-json-canonical refs))))
      (insert "\n"))))

(defun eas-vl-gallery-custom-build-ref (group name spec)
  "Build NAME's reference in GROUP from SPEC with bin/chart if it is due.
A reference is due when it is missing or stale; an up-to-date one is
never rewritten.  Return non-nil when a reference was built."
  (when (and (eas-chart-available-p) (not (eq (eas-vl-gallery-custom-ref-state group name spec) 'current)))
    (let ((ref (eas-vl-gallery-custom-ref group name)))
      (make-directory (file-name-directory ref) t)
      (let ((process-environment (cons (concat "TZ=" eas-vl-gallery-zone) process-environment))
            (coding-system-for-write 'no-conversion)
            (png (eas-chart-build spec "png")))
        (with-temp-file ref
          (set-buffer-multibyte nil)
          (insert png)))
      (eas-vl-gallery-custom-record-ref group name spec)
      t)))

(defun eas-vl-gallery-custom-spec (group name)
  "Customization spec NAME of GROUP with its data inlined."
  (let ((dir (eas-vl-gallery-custom-directory group)))
    (eas-vl-gallery-inline (eas-json-read-file (eas-vl-gallery-custom-file group name)) dir)))

(defun eas-vl-gallery-custom--findings (spec)
  "Check findings and ignored properties of SPEC, as problem strings."
  (mapcar (lambda (f) (format "%s at %s: %s" (plist-get f :code) (plist-get f :path) (plist-get f :message)))
          (let ((eas-spec-supported-function nil)) (eas-spec-check spec))))

(defun eas-vl-gallery-custom-image (group name spec svg)
  "Judge native SVG of customization SPEC NAME in GROUP against bin/chart.
Return (:status S :detail D [:ratio R]), S one of pass, fail,
ref-pending, ref-stale and unverified."
  (let ((ref (eas-vl-gallery-custom-ref group name))
        (threshold (or (plist-get (plist-get (plist-get spec :usermeta) :eas) :threshold)
                       eas-vl-gallery-default-threshold)))
    (eas-vl-gallery-custom-build-ref group name spec)
    (pcase (eas-vl-gallery-custom-ref-state group name spec)
      ('missing
       ;; ref-pending: no bin/chart reference yet; never a failure.
       (list :status "ref-pending" :detail (format "no reference: %s builds custom/ref/%s.png" eas-chart-program name)))
      ('stale
       ;; A reference of an older spec proves nothing either way.
       (list :status "ref-stale"
             :detail (format "custom/ref/%s.png was built from an older spec: %s rebuilds it" name eas-chart-program)))
      (_ (eas-vl-gallery-custom--compare ref svg threshold)))))

(defun eas-vl-gallery-custom--compare (ref svg threshold)
  "Judge native SVG against reference PNG REF within THRESHOLD."
  (if (not (eas-vl-gallery-rasterizer-p))
      (list :status "unverified" :detail (format "%s is not on PATH to rasterize native SVG" eas-chart-rsvg-program))
    (let ((mine (make-temp-file "eas-custom" nil ".png")))
      (unwind-protect
          (let* ((cmp (progn (eas-chart-rasterize svg mine)
                             (eas-png-compare (eas-png-read mine) (eas-png-read ref))))
                 (ratio (plist-get cmp :ratio)) (delta (plist-get cmp :size-delta)))
            (list :status (if (and (<= ratio threshold) (<= (max (abs (aref delta 0)) (abs (aref delta 1))) 8))
                              "pass" "fail")
                  :ratio ratio
                  :detail (format "ratio %.4f (threshold %s), size delta %S" ratio threshold delta)))
        (delete-file mine)))))

(defun eas-vl-gallery-custom-check (group name)
  "Return the problems of customization spec NAME of GROUP, as strings.
Return nil when there are none.  An image that cannot be verified here
is no problem."

  (condition-case err
      (let* ((spec (eas-vl-gallery-custom-spec group name))
             (findings (eas-vl-gallery-custom--findings spec))
             (svg (eas-vl-gallery-svg spec))
             (text (eas-vl-gallery-text spec))
             (golden (eas-vl-gallery-custom-file group name ".txt"))
             (image (eas-vl-gallery-custom-image group name spec svg))
             problems)
        (when (getenv "EAS_UPDATE_GOLDEN")
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
