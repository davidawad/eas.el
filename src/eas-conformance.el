;;; eas-conformance.el --- the gallery, supported.json and the oracle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A feature is supported only when a conformance spec proves it.  The
;; gallery is test/conformance/NAME.vl.json, pure Vega-Lite, each with
;; usermeta.eas.threshold.  For every spec:
;;
;;   native  compile for both targets, render SVG and text; the text must
;;           equal test/conformance/NAME.txt exactly
;;   oracle  native SVG -> PNG (rsvg-convert) vs bin/chart's PNG (the
;;           committed ref/NAME.png, or a fresh build when bin/chart is
;;           installed), aligned and compared at the spec's threshold
;;           (see eas-conformance-oracle.el); "unverified" when
;;           rsvg-convert is missing
;;
;; `eas-conformance-generate' writes supported.json next to this file
;; from the passing gallery; `eas-spec-check', compile and describe
;; read it through `eas-spec-supported-function'.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-template)
(require 'eas-compile)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-chart)
(require 'eas-conformance-oracle)

(defvar eas-conformance-directory
  (expand-file-name "test/conformance" eas-template--root)
  "Directory holding the conformance gallery.")

(defconst eas-conformance-supported-file
  (expand-file-name "supported.json"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "The generated list of supported features, shipped with the engine.")

(defconst eas-conformance-text-size '(:cols 60 :rows 16)
  "Size of the exact text goldens.")

(defconst eas-conformance-extensions
  '(:transform/x-eas "Domain transforms are materialized by resolve (ERT: eas-resolve-materializes-domain-transforms)."
    :mark/slot "Template slot placeholders are substituted by resolve (ERT: eas-template-enum-and-array-items)."
    :transform/x-eas/countpattern "Vega's countpattern: regexp matches counted into text/count rows (ERT: eas-vega-wordcloud-countpattern-counts-words)."
    :transform/x-eas/wordcloud "Vega's wordcloud: seeded spiral placement of measured, rotated words (ERT: eas-vega-wordcloud-places-without-overlap)."
    :transform/x-eas:hierarchy "stratify, nest, tree (tidy, cluster), treemap, partition, pack, treelinks, treepath and formula match Vega's layouts (ERT: eas-vega-hierarchy-*)."
    :transform/x-eas:linkpath "linkpath draws Vega's line, arc, curve, diagonal and orthogonal links, radial too (ERT: eas-vega-hierarchy-linkpath-*)."
    :x-eas/timer "x-eas.timer ticks a shown view's timer handlers every interval while its buffer is visible (ERT: eas-vega-interaction-timer-drives-params)."
    :x-eas/on "x-eas.on handlers turn timer ticks and keys into param events (ERT: eas-vega-interaction-keys-drive-params)."
    :transform/x-eas/dotbin "Vega's dotbin (Wilkinson dot plot bins) as a domain transform (ERT: eas-vega-distributions-dotbin-matches-vega)."
    :transform/x-eas:force "Vega's force transform, d3-force's simulation and forces (ERT: eas-vega-network-force-matches-d3)."
    :transform/x-eas:graph "Nodes joined to links: order, degree, link and pair rows (ERT: eas-vega-network-graph-joins-nodes-and-links)."
    :transform/x-eas:voronoi "Vega's voronoi transform: cell paths and vertices (ERT: eas-vega-network-voronoi-cells)."
    :transform/x-eas:arc-label "Pie label layout, materialized by resolve (ERT: eas-vega-std-basic-arc-label-separates-each-side)."
    :transform/x-eas:label "Point label layout, materialized by resolve (ERT: eas-vega-std-basic-label-keeps-labels-apart)."
    :transform/x-eas/parallel-coordinates "Fields folded and scaled on their own niced domains, with axis tick rows (ERT: eas-vega-other-custom-parallel-coordinates-scales-each-field)."
    :transform/x-eas/serpentine "A domain laid along a serpentine path, in pixels (ERT: eas-vega-other-custom-serpentine-lays-out-the-path)."
    ;; Vega's contour, density and raster transforms (eas-7r1.7, eas-contour.el)
    :transform/x-eas:geopath "Vega's geopath: GeoJSON to SVG path data in pixels, identity or naturalEarth1/equalEarth/mercator/equirectangular (ERT: eas-vega-contour-geopath-writes-svg-path-data)."
    :transform/x-eas:geopoints "GeoJSON vertices as line rows, cut to a box (ERT: eas-vega-contour-geopoints-cut-lines-to-the-box)."
    :transform/x-eas:heatmap "Vega's heatmap: a value grid as a PNG data: URL for the image mark (ERT: eas-vega-contour-heatmap-colors-and-opacity)."
    :transform/x-eas:isocontour "Vega's isocontour: d3-contour marching squares, smoothing, thresholds and levels (ERT: eas-vega-contour-marching-squares-match-d3)."
    :transform/x-eas:kde2d "Vega's kde2d: 2D kernel density grids on the chart's nice zero domains (ERT: eas-vega-contour-kde2d-matches-vega).")
  "Features outside Vega-Lite proper, proven by ERT rather than the gallery.")

(defvar eas-conformance-gallery-functions nil
  "Functions of no arguments returning more gallery entries.
An entry is a plist like `eas-conformance-gallery' returns, plus :ref
\(its reference PNG), :ref-problem (why that reference cannot be
trusted, or nil) and :text-file (its text golden).  The official
Vega-Lite examples that pass natively join this way (eas-vl-gallery.el).")

(defun eas-conformance-gallery ()
  "The gallery as plists (:name :file :spec :threshold), sorted by name.
Entries from `eas-conformance-gallery-functions' follow."
  (append
   (mapcar (lambda (file)
             (let ((spec (eas-json-read-file file)))
               (list :name (string-remove-suffix ".vl.json" (file-name-nondirectory file))
                     :file file :spec spec
                     :threshold (or (plist-get (plist-get (plist-get spec :usermeta) :eas) :threshold) 0.05))))
           (directory-files eas-conformance-directory t "\\.vl\\.json\\'"))
   (apply #'append (mapcar #'funcall eas-conformance-gallery-functions))))

(defun eas-conformance-text-file (entry)
  "The text golden of gallery ENTRY."
  (or (plist-get entry :text-file)
      (expand-file-name (concat (plist-get entry :name) ".txt") eas-conformance-directory)))

(defun eas-conformance-native (entry)
  "Compile and render gallery ENTRY natively, unrestricted by supported.json.
The SVG is compiled in the references' time zone, the text golden in UTC.
Return (:name :ok :features :svg :text :error)."
  (let ((eas-spec-supported-function nil)
        (spec (plist-get entry :spec)))
    (condition-case err
        (let* ((svg (let ((eas-time-zone (eas-conformance-ref-zone)))
                      ;; :oracle-scene drops what a known-defective reference lacks.
                      (eas-svg-render (funcall (or (plist-get entry :oracle-scene) #'identity) (eas-compile spec)))))
               (text (concat (substring-no-properties
                              (eas-text-render (eas-compile spec :target 'text :size eas-conformance-text-size)))
                             "\n"))
               (golden (eas-conformance-text-file entry))
               (matches (and (file-exists-p golden)
                             (equal text (with-temp-buffer (insert-file-contents golden) (buffer-string))))))
          (list :name (plist-get entry :name) :ok matches
                :features (delete-dups (mapcar (lambda (f) (plist-get f :feature))
                                               (seq-remove (lambda (f) (plist-get f :invalid))
                                                           (eas-spec-features (eas-spec-parse spec)))))
                :svg svg :text text
                :error (unless matches (list :code "GOLDEN_MISMATCH" :message "text differs from its golden"))))
      (eas-error (list :name (plist-get entry :name) :ok nil :features nil :error (eas-error-plist err))))))

(defun eas-conformance-run (&optional oracle)
  "Run the gallery natively (and against bin/chart when ORACLE is non-nil)."
  (mapcar (lambda (entry)
            (let ((native (eas-conformance-native entry)))
              (append native
                      (list :oracle (if oracle (eas-conformance-oracle entry native)
                                      (list :status "unverified" :detail "oracle not run"))))))
          (eas-conformance-gallery)))

(defun eas-conformance-supported-data (results)
  "The supported.json content for gallery RESULTS."
  (let ((features (make-hash-table :test 'equal)))
    (dolist (r results)
      (when (and (plist-get r :ok) (not (equal (plist-get (plist-get r :oracle) :status) "fail")))
        (dolist (f (plist-get r :features))
          (puthash f (cons r (gethash f features)) features))))
    (list :contract "eas-supported/v1"
          :vega-lite eas-spec-vega-lite-version
          :gallery "test/conformance"
          :features
          (let (out)
            (dolist (f (sort (hash-table-keys features) #'string<))
              (let ((rs (reverse (gethash f features))))
                (setq out (append out (list (eas-key f)
                                            (list :specs (vconcat (mapcar (lambda (r) (plist-get r :name)) rs))
                                                  :oracle (if (seq-some (lambda (r) (equal (plist-get (plist-get r :oracle) :status) "pass")) rs)
                                                              "verified" "unverified")))))))
            out)
          :extensions eas-conformance-extensions)))

(defun eas-conformance-generate (&optional oracle file)
  "Run the gallery (with ORACLE when non-nil) and write supported.json to FILE."
  (let ((data (eas-conformance-supported-data (eas-conformance-run oracle))))
    (with-temp-file (or file eas-conformance-supported-file)
      (insert (eas-json-pretty data)))
    data))

;;; Reading supported.json

(defvar eas-conformance--cache nil "(MTIME . FEATURES) of the last read.")

(defun eas-conformance-supported ()
  "Parsed supported.json, or nil when it does not exist."
  (when (file-exists-p eas-conformance-supported-file)
    (eas-json-read-file eas-conformance-supported-file)))

(defun eas-conformance-supported-features ()
  "Feature ids supported.json proves, plus extensions; t when the file is absent."
  (let ((mtime (file-attribute-modification-time (file-attributes eas-conformance-supported-file))))
    (if (null mtime) t
      (unless (equal (car eas-conformance--cache) mtime)
        (let ((data (eas-conformance-supported)))
          (setq eas-conformance--cache
                (cons mtime (mapcar #'eas-key-name
                                    (append (eas-plist-keys (plist-get data :features))
                                            (eas-plist-keys (plist-get data :extensions))))))))
      (cdr eas-conformance--cache))))

(setq eas-spec-supported-function #'eas-conformance-supported-features)

(defun eas-conformance-describe ()
  "The describe section for supported features."
  (let ((data (eas-conformance-supported)))
    (list :supported (if data
                         (list :file eas-conformance-supported-file
                               :features (vconcat (mapcar #'eas-key-name (eas-plist-keys (plist-get data :features))))
                               :verified (vconcat (cl-loop for (k v) on (plist-get data :features) by #'cddr
                                                           when (equal (plist-get v :oracle) "verified")
                                                           collect (eas-key-name k)))
                               :extensions (vconcat (mapcar #'eas-key-name (eas-plist-keys (plist-get data :extensions))))
                               :oracle (cond ((eas-chart-available-p) "bin/chart available")
                                             ((executable-find eas-chart-rsvg-program)
                                              "committed bin/chart references (test/conformance/ref)")
                                             (t (format "unverified here: %s is not on PATH"
                                                        eas-chart-rsvg-program))))
                       :null))))

(add-hook 'eas-describe-functions #'eas-conformance-describe)

(require 'eas-vl-gallery)
(require 'eas-vl-gallery-custom)

(provide 'eas-conformance)
;;; eas-conformance.el ends here
