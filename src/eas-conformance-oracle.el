;;; eas-conformance-oracle.el --- reference images and the image oracle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/chart is the oracle, but it is not installed everywhere, so its
;; output is committed: test/conformance/ref/NAME.png is `bin/chart
;; build' of NAME.vl.json, and ref/manifest.json records, per spec, the
;; hash of the spec it was built from and of the PNG.  The spec hash is
;; `eas-content-hash' of the spec without "usermeta" (which Vega-Lite
;; ignores), so thresholds and their justifications can change without
;; invalidating a reference.
;;
;; The oracle rasterizes the native SVG (rsvg-convert) and compares it
;; with the reference using `eas-png-compare': canvases are aligned
;; and padded, the size delta is reported and bounded separately, and
;; the spec passes when the differing-pixel ratio is within its
;; usermeta.eas.threshold.  A reference whose manifest hash no longer
;; matches its spec is stale and fails.  When bin/chart is on PATH the
;; reference is rebuilt from the spec instead of read from ref/, and
;; `eas-conformance-update-refs' rewrites ref/ and the manifest.
;;
;; Vega draws "time" scales and timeUnits in local time, so references
;; depend on the zone they were built in.  The manifest records it
;; ("tz"); native SVG is compiled with `eas-time-zone' bound to it and
;; bin/chart rebuilds run with TZ set to it.

;;; Code:

(require 'eas-core)
(require 'eas-chart)
(require 'eas-png)
(require 'eas-time)
(require 'eas-vl-gallery-mask)

(defvar eas-conformance-directory)
(declare-function eas-conformance-gallery "eas-conformance")

(defvar eas-conformance-size-tolerance 8
  "Default largest canvas size difference, in pixels per axis, that passes.
Vega pads the canvas for marks and labels overhanging the plot (1-7px
in the gallery), which native layout reproduces only approximately.")

(defun eas-conformance-ref-directory ()
  "Directory of the committed bin/chart reference images."
  (expand-file-name "ref" eas-conformance-directory))

(defun eas-conformance-manifest-file ()
  "The reference manifest."
  (expand-file-name "manifest.json" (eas-conformance-ref-directory)))

(defun eas-conformance-manifest ()
  "Parsed ref/manifest.json, or nil when absent."
  (let ((file (eas-conformance-manifest-file)))
    (and (file-exists-p file) (eas-json-read-file file))))

(defconst eas-conformance-default-zone "UTC"
  "Zone for references when the manifest names none.")

(defun eas-conformance-ref-zone ()
  "The time zone the committed references were built in."
  (or (plist-get (eas-conformance-manifest) :tz) eas-conformance-default-zone))

(defun eas-conformance-spec-hash (spec)
  "Hex sha256 identifying what bin/chart renders for SPEC (usermeta excluded)."
  (string-remove-prefix "sha256:" (eas-content-hash (eas--plist-without spec :usermeta))))

(defun eas-conformance--eas-meta (entry key)
  "usermeta.eas KEY of gallery ENTRY's spec."
  (plist-get (plist-get (plist-get (plist-get entry :spec) :usermeta) :eas) key))

(defun eas-conformance--file-hash (file)
  "Hex sha256 of FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun eas-conformance-ref-problem (entry)
  "Why ENTRY's committed reference cannot be trusted, or nil.
Gallery-group entries (with :ref) carry their own verdict in :ref-problem."
  (if (plist-member entry :ref) (plist-get entry :ref-problem)
   (let* ((name (plist-get entry :name))
         (file (expand-file-name (concat name ".png") (eas-conformance-ref-directory)))
         (ref (plist-get (plist-get (eas-conformance-manifest) :refs) (eas-key name)))
         (fix "rebuild with bin/chart: M-x eas-conformance-update-refs"))
    (cond ((not (file-exists-p file)) (format "STALE_REF: ref/%s.png is missing; %s" name fix))
          ((null ref) (format "STALE_REF: ref/manifest.json has no entry for %s; %s" name fix))
          ((not (equal (plist-get ref :spec_sha256) (eas-conformance-spec-hash (plist-get entry :spec))))
           (format "STALE_REF: ref/%s.png was built from an older %s.vl.json; %s" name name fix))
          ((not (equal (plist-get ref :png_sha256) (eas-conformance--file-hash file)))
           (format "STALE_REF: ref/%s.png does not match its manifest hash; %s" name fix))))))

(defun eas-conformance--write-png (spec file)
  "Build SPEC with bin/chart into PNG FILE, in the references' time zone."
  (let ((process-environment (cons (concat "TZ=" (eas-conformance-ref-zone)) process-environment))
        (coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (eas-chart-build spec "png")))))

(defun eas-conformance--judge (entry mine ref source)
  "Compare native PNG MINE with reference PNG REF for ENTRY; SOURCE names REF."
  (let* ((mask (plist-get entry :mask))
         (cmp (eas-png-compare (eas-vl-gallery-mask-image (eas-png-read mine) mask)
                               (eas-vl-gallery-mask-image (eas-png-read ref) mask)))
         (threshold (plist-get entry :threshold))
         (tolerance (or (eas-conformance--eas-meta entry :size_tolerance) eas-conformance-size-tolerance))
         (delta (plist-get cmp :size-delta))
         (size-ok (and (<= (abs (aref delta 0)) tolerance) (<= (abs (aref delta 1)) tolerance)))
         (ratio-ok (<= (plist-get cmp :ratio) threshold)))
    (append
     (list :status (if (and size-ok ratio-ok) "pass" "fail")
           :detail (format "ratio %.4f %s %s; size %+d,%+d px (native %dx%d, %s %dx%d) %s %d; offset %d,%d"
                           (plist-get cmp :ratio) (if ratio-ok "<=" ">") threshold
                           (aref delta 0) (aref delta 1)
                           (aref (plist-get cmp :native) 0) (aref (plist-get cmp :native) 1)
                           source (aref (plist-get cmp :reference) 0) (aref (plist-get cmp :reference) 1)
                           (if size-ok "within" "beyond") tolerance
                           (aref (plist-get cmp :offset) 0) (aref (plist-get cmp :offset) 1))
           :threshold threshold :source source)
     cmp)))

(defun eas-conformance-oracle (entry native)
  "Compare NATIVE's SVG for gallery ENTRY with bin/chart's image.
Return (:status :detail ...): STATUS is \"pass\", \"fail\" or
\"unverified\" (when the native SVG cannot be rasterized here)."
  (cond
   ((not (zlib-available-p)) (list :status "unverified" :detail "this Emacs lacks zlib, needed to decode PNGs"))
   ((not (executable-find eas-chart-rsvg-program))
    (list :status "unverified"
          :detail (format "%s is not on PATH; needed to rasterize native SVG" eas-chart-rsvg-program)))
   ((not (plist-get native :svg)) (list :status "fail" :detail "native rendering failed"))
   (t
    (let ((stale (if (plist-member entry :ref) (plist-get entry :ref-problem)
                   (eas-conformance-ref-problem entry)))
          (mine (make-temp-file "eas-native" nil ".png"))
          (fresh (and (eas-chart-available-p) (not (plist-get entry :ref-pinned))
                      (make-temp-file "eas-ref" nil ".png"))))
      (unwind-protect
          (condition-case err
              (progn
                (eas-chart-rasterize (plist-get native :svg) mine)
                (when fresh (eas-conformance--write-png (plist-get entry :spec) fresh))
                (cond
                 (fresh (let ((r (eas-conformance--judge entry mine fresh "bin/chart")))
                          (if stale (append (list :status "fail" :detail (concat stale "; " (plist-get r :detail))) r)
                            r)))
                 ((and stale (string-match-p "missing\\|no entry" stale)) (list :status "fail" :detail stale))
                 (t (let ((r (eas-conformance--judge
                              entry mine (or (plist-get entry :ref)
                                             (expand-file-name (concat (plist-get entry :name) ".png")
                                                               (eas-conformance-ref-directory)))
                              "ref")))
                      (if stale (append (list :status "fail" :detail (concat stale "; " (plist-get r :detail))) r)
                        r)))))
            (eas-error (list :status "fail" :detail (plist-get (eas-error-plist err) :message))))
        (delete-file mine)
        (when fresh (delete-file fresh)))))))

(defun eas-conformance-update-refs (&optional entries)
  "Rebuild ref/NAME.png with bin/chart for ENTRIES (default: the gallery).
Rewrite ref/manifest.json.  Signals NOT_FOUND without bin/chart."
  (interactive)
  (let ((entries (or entries (seq-remove (lambda (e) (plist-member e :ref)) (eas-conformance-gallery))))
        (zone (eas-conformance-ref-zone))
        (refs (plist-get (eas-conformance-manifest) :refs)))
    (dolist (entry entries)
      (let ((file (expand-file-name (concat (plist-get entry :name) ".png") (eas-conformance-ref-directory))))
        (eas-conformance--write-png (plist-get entry :spec) file)
        (setq refs (eas-plist-put refs (eas-key (plist-get entry :name))
                                    (list :png_sha256 (eas-conformance--file-hash file)
                                          :spec_sha256 (eas-conformance-spec-hash (plist-get entry :spec)))))))
    (with-temp-file (eas-conformance-manifest-file)
      (insert (eas-json-pretty
               (list :generator "bin/chart build SPEC --out ref/NAME.png (vl-convert, default theme), via eas-conformance-update-refs"
                     :spec_hash "eas-conformance-spec-hash: sha256 of the spec's canonical JSON without usermeta"
                     :tz zone
                     :refs (eas-json-canonical refs))))
      (insert "\n"))))


(provide 'eas-conformance-oracle)
;;; eas-conformance-oracle.el ends here
