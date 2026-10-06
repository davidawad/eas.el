;;; eas-compile.el --- compile: resolved spec + rows -> scene/v1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L4.  `eas-compile' turns a resolved chart/v1 spec (pure Vega-Lite
;; with inline data), optional replacement rows, a size and the
;; runtime's view state into scene/v1: views with bounds, invertible
;; scales, placed axes and legends, and marks whose items carry datum
;; back-references, resolved style, tooltip and href, plus a hit-test
;; index per mark.  It is pure; renderers and hit-testing read only the
;; scene.  A unit or layer is one scene view; each concat cell is its
;; own view.  Source rows are tagged with :_eas_row so a datum can be
;; traced to its input row through filters and calculations.

;;; Code:

(require 'eas-core)
(require 'eas-compile-memo)
(require 'eas-spec)
(require 'eas-data)
(require 'eas-transform)
(require 'eas-encode)
(require 'eas-scale)
(require 'eas-layout)
(require 'eas-marks)
(require 'eas-hit)
(require 'eas-compile-scales)
(require 'eas-compile-place)
(require 'eas-marks-bounds)
(require 'eas-theme)
(require 'eas-font-file)
(require 'eas-legend)
(require 'eas-link-scale)
(require 'eas-overlay)
(require 'eas-polar)
(require 'eas-bins)
(require 'eas-composite)
(require 'eas-facet)
(require 'eas-nested)
(require 'eas-layer)
(require 'eas-independent)
(require 'eas-compile-shared-pos)
(require 'eas-projection)
(require 'eas-spec-props)
(require 'eas-title)
(require 'eas-axis)
(require 'eas-compile-aux)
(require 'eas-compile-channels)
(require 'eas-params-init)
(require 'eas-text-snap)
(require 'eas-text-labels)

(defvar eas-compile-gc-threshold (* 64 1024 1024)
  "GC threshold compile runs under; compile allocates many small plists.")

(defconst eas-compile-row-key :_eas_row
  "Key tagging each source row with its index in the input data.")

(defun eas-compile--tag (rows)
  "Return ROWS (a vector or list of plists) tagged with their indices.
A primitive value becomes the row {\"data\": VALUE}, as in Vega-Lite."
  (let ((i -1))
    (vconcat (seq-map (lambda (row) (setq i (1+ i))
                        (append (if (or (null row) (eas-object-p row)) row (list :data row))
                                (list eas-compile-row-key i)))
                      rows))))

(defun eas-compile--view-id (node path)
  "Return a stable id for view NODE at PATH: its name, else derived from PATH."
  (or (plist-get node :name)
      (if (string-empty-p path) "main"
        (replace-regexp-in-string "\\`_" "" (replace-regexp-in-string "/" "_" path)))))

(defun eas-compile--node-data (node ctx override)
  "Return the rows for NODE given CTX.
OVERRIDE, when non-nil, stands in for the root data."
  (let ((data (plist-get node :data)))
    (cond
     ((and override (string-empty-p (plist-get ctx :path))) (eas-compile--tag override))
     ((null data) (plist-get ctx :rows))
     ((vectorp (plist-get data :values))
      ;; Vega wraps primitive values as {"data": value}.
      (eas-compile--tag (seq-map (lambda (v) (if (and (consp v) (keywordp (car v))) v (list :data v)))
                                 (plist-get data :values))))
     ((plist-get data :name)
      (eas-signal "INVALID_INPUT"
                    (format "Data %s is a template slot; resolve the template first" (plist-get data :name))
                    :path (concat (plist-get ctx :path) "/data")))
     (t (eas-signal "UNSUPPORTED_FEATURE" "Only inline data.values compiles natively"
                      :path (concat (plist-get ctx :path) "/data") :feature "data/url")))))

(defun eas-compile--valid-rows (enc rows type)
  "ROWS without those whose continuous x or y under ENC is null.
Vega-Lite's mark.invalid \"filter\" drops them from marks and scales;
lines and areas keep them (their default breaks the path instead)."
  (let ((keys (cl-loop for ch in '(:x :y :x2 :y2)
                       for d = (plist-get enc ch)
                       when (and (eas-object-p d) (plist-get d :field)
                                 (member (plist-get d :type) '("quantitative" "temporal")))
                       collect (eas-encode-field d))))
    (if (or (null keys) (member type '("line" "area"))) rows
      (vconcat (seq-remove (lambda (row) (seq-some (lambda (k) (memq (plist-get row k) '(nil :null))) keys)) rows)))))

(defun eas-compile--unit (node ctx env)
  "Compile unit NODE under CTX into a unit plist (rows, encoding, mark).
ENV gives the param values expressions see."
  (let* ((path (plist-get ctx :path))
         (encoding (eas-layer-drop-empty (plist-get ctx :encoding)))
         (rows (eas-layer-coerce
                (eas-nested-flatten (eas-compile-memo-transform-run (vconcat (plist-get ctx :transforms)) (plist-get ctx :rows) env
                                                                    (concat path "/transform"))
                                    encoding)
                encoding))
         (enc (eas-encode-normalize encoding rows))
         (derived (eas-encode-derive enc rows env))
         (mark (plist-get node :mark))
         (derived (cons (car derived) (eas-compile--valid-rows (car derived) (cdr derived) (plist-get mark :type))))
         (unit (list :path path :name (plist-get node :name)
                     :mark (eas-marks-resolve-mark
                            mark (or (plist-get ctx :config)
                                     (eas-theme-merge eas-theme-vega-lite eas-theme-default)))
                     :encoding (car derived) :rows (vconcat (cdr derived)) :env env
                     :aggregated (seq-some (lambda (d) (and (eas-object-p d) (plist-get d :aggregate)))
                                           (cl-loop for (_ d) on (plist-get ctx :encoding) by #'cddr collect d))
                     :params (plist-get node :params))))
    (eas-polar-stack (eas-marks-stack unit))))

(defun eas-compile--merge-encoding (parent encoding)
  "ENCODING over a layer's PARENT encoding, as Vega-Lite's mergeEncoding:
a child field or datum def inherits the parent def's other properties.
A child channel set to null removes the channel it would inherit."
  (let ((enc parent))
    (cl-loop for (ch d) on encoding by #'cddr
             for p = (plist-get parent ch)
             do (setq enc (if (eq d :null) (eas--plist-without enc ch)
                            (eas-plist-put
                           enc ch (if (and (consp d) (keywordp (car d)) (consp p) (keywordp (car p))
                                           (or (plist-get d :field) (plist-member d :datum)))
                                      (let ((m p)) (cl-loop for (k v) on d by #'cddr do (setq m (eas-plist-put m k v))) m)
                                    d)))))
    enc))

(defun eas-compile--point-overlay (node)
  "The point layer Vega-Lite overlays on NODE's line or area (mark.point), or nil."
  (let* ((mark (plist-get node :mark)) (point (plist-get mark :point)))
    (when (and (member (plist-get mark :type) '("line" "area"))
               (or (eq point t) (and (eas-object-p point) point)))
      (plist-put (copy-sequence node) :mark
                 (append (list :type "point" :opacity 1 :filled t)
                         (and (plist-get mark :color) (list :color (plist-get mark :color)))
                         (and (eas-object-p point) (eas--plist-without point :type)))))))

(defun eas-compile--child-ctx (node ctx key i rows)
  "Context for child I of NODE's KEY array, inheriting from CTX with ROWS."
  (list :rows rows
        :transforms (append (plist-get ctx :transforms) (append (plist-get node :transform) nil))
        :encoding (if (eq key :layer)
                      (eas-compile--merge-encoding (plist-get ctx :encoding) (plist-get node :encoding))
                    nil)
        :path (format "%s/%s/%d" (plist-get ctx :path) (eas-key-name key) i)
        :config (plist-get ctx :config)
        :width (or (plist-get node :width) (plist-get ctx :width))
        :height (or (plist-get node :height) (plist-get ctx :height))))

(defun eas-compile--collect (node ctx env override group)
  "Walk NODE under CTX; return a layout tree.  GROUP collects layer units.
ENV gives the param values expressions see; OVERRIDE, when non-nil,
stands in for the root data."
  (let* ((rows (eas-compile--node-data node ctx override))
         (ctx (plist-put (copy-sequence ctx) :rows rows))
         (own (eas-compile--merge-encoding (plist-get ctx :encoding) (plist-get node :encoding))))
    (cond
     ((or (plist-get node :vconcat) (plist-get node :hconcat))
      (let* ((key (if (plist-get node :vconcat) :vconcat :hconcat))
             ;; A facet's rows are transformed and partitioned once for all its cells.
             (pre (eas-facet-layout-partition node ctx rows env))
             (node (if pre (car pre) node))
             (rows (if pre (cdr pre) rows)))
        (list :concat (if (eq key :vconcat) "v" "h")
              :spacing (let ((s (plist-get node :spacing))) (and (numberp s) s))
              :grid (plist-get (plist-get node :x-eas) :grid)
              :x-eas (plist-get node :x-eas) :bounds (plist-get node :bounds) ; facet, flush layouts
              :children (let ((eas-link--scope (eas-link-scope node)))
                          (seq-map-indexed
                           (lambda (child i)
                             (eas-compile--collect child (eas-compile--child-ctx node ctx key i rows)
                                                     env nil nil))
                           (plist-get node key))))))
     (t
      (let* ((g (or group (list :id (eas-compile--view-id node (plist-get ctx :path))
                                :path (plist-get ctx :path) :units nil
                                :spec-w (or (plist-get node :width) (plist-get ctx :width))
                                :spec-h (or (plist-get node :height) (plist-get ctx :height))
                                :resolve (plist-get node :resolve)
                                ;; A concat cell's own title (the root's is the chart title).
                                :title-node (and (not (string-empty-p (plist-get ctx :path))) (eas-title-lines node) node)
                                :header (plist-get (plist-get node :x-eas) :header)
                                :params nil))))
        (when-let* ((inherited (and (not group) (eas-link-scoped-params node))))
          (plist-put g :params (mapcar (lambda (p) (append p (list :view (plist-get g :id)))) inherited)))
        (when (plist-get node :params)
          (plist-put g :params (append (plist-get g :params)
                                       (mapcar (lambda (p) (append p (list :view (plist-get g :id))))
                                               (plist-get node :params)))))
        (if (plist-get node :layer)
            (seq-do-indexed
             (lambda (child i)
               (eas-compile--collect child (eas-compile--child-ctx node ctx :layer i rows) env nil g))
             (plist-get node :layer))
          (let ((uctx (plist-put (copy-sequence ctx) :encoding own)))
            (plist-put uctx :transforms (append (plist-get ctx :transforms)
                                                (append (plist-get node :transform) nil)))
            (dolist (n (delq nil (list node (eas-compile--point-overlay node))))
              (plist-put g :units (append (plist-get g :units)
                                          (list (append (eas-compile--unit n uctx env)
                                                        (list :node n :ctx uctx))))))))
        (unless group (list :group g)))))))

(defun eas-compile--scales (group state metrics)
  "Build GROUP's scales, axis defs and legend specs; STATE gives zooms.
METRICS supplies the config."
  (let* ((units (plist-get group :units))
         (config (plist-get metrics :config))
         (zoom (plist-get (plist-get state :domains) (eas-key (plist-get group :id))))
         (color (eas-compile-color-scale units config))
         ;; Other color channels: one of the same field shares color's
         ;; scale and legend, one of another field gets its own.
         (extra-colors (eas-compile--extra-colors units config color))
         (size (eas-compile-aux-scale units :size [4 361]))
         (opacity (eas-compile-aux-scale units :opacity [0.3 0.8]))
         (shape-scale (eas-bins-shape-scale units))
         (dash (eas-compile-dash-scale units config))
         (channels (eas-compile-channels-scales units))
         (first-def (lambda (ch) (cdar (eas-compile--defs units ch))))
         ;; Legend symbols copy the mark that encodes the legend's channel.
         (unit (or (caar (eas-compile--defs units (if color (nth 0 color) :size))) (car units)))
         (shape (let ((type (plist-get (plist-get unit :mark) :type)))
                  (cond ((member type '("bar" "rect" "area" "square")) "square")
                        ((member type '("line" "rule" "trail")) "stroke") (t "circle"))))
         (spec (lambda (channel def scale)
                 ;; Symbols copy the look of the layer that encodes the channel.
                 (let ((owner (or (caar (eas-compile--defs units channel)) unit)))
                   (list :channel channel :def def :scale scale :shape shape
                         :style (eas-marks-legend-style owner metrics))))))
    (plist-put group :scales
               (append (cl-loop for ch in '(:x :y)
                                for s = (eas-compile-position-scale
                                         units ch (or (plist-get zoom ch) (eas-link-param-domain units ch state)))
                                when s append (list ch s))
                       (eas-polar-scales units)
                       (when color (list (nth 0 color) (nth 2 color)))
                       (cl-loop for (ch _ scale) in extra-colors append (list ch scale))
                       (when size (list :size size))
                       (when opacity (list :opacity opacity))
                       (when shape-scale (list :shape shape-scale))
                       (when dash (list :strokeDash dash))
                       channels))
    (plist-put group :axis-defs (list :x (eas-bins-axis-def units :x) :y (eas-bins-axis-def units :y)))
    (eas-independent-scales group zoom)
    (plist-put group :legend-specs
               (delq nil (list (when color (append (funcall spec (nth 0 color) (nth 1 color) (nth 2 color))
                                                   (when (eas-compile-channels-legend-shape units)
                                                     (list :shape-scale shape-scale))
                                                   ;; Vega-Lite merges a discretized size legend of the same field.
                                                   (when (eas-scale-discretize-merge-p (nth 1 color) (funcall first-def :size) size)
                                                     (list :size-scale size))))
                               (when (and size (not (and color (eas-scale-discretize-merge-p
                                                                 (nth 1 color) (funcall first-def :size) size))))
                                 (funcall spec :size (funcall first-def :size) size))
                               (when opacity (funcall spec :opacity (funcall first-def :opacity) opacity))

                               (when dash (funcall spec :strokeDash (funcall first-def :strokeDash) dash)))))
    (plist-put group :legend-specs
               (append (plist-get group :legend-specs)
                       (cl-loop for (ch def scale own) in extra-colors
                                when own
                                collect (let ((owner (caar (eas-compile--defs units ch))))
                                          (append (list :shape (eas-compile--legend-shape owner) :extra t)
                                                  (funcall spec ch def scale))))))))

(defun eas-compile--legend-shape (unit)
  "The legend symbol shape of UNIT's mark: square, stroke or circle."
  (let ((type (plist-get (plist-get unit :mark) :type)))
    (cond ((member type '("bar" "rect" "area" "square")) "square")
          ((member type '("line" "rule" "trail")) "stroke") (t "circle"))))

(defun eas-compile--extra-colors (units config color)
  "(CHANNEL DEF SCALE OWN) for UNITS' color channels besides COLOR's.
COLOR is `eas-compile-color-scale's answer.  A channel mapping COLOR's
field shares its scale (OWN nil); another field gets its own scale and
legend (OWN t).  CONFIG gives the ranges."
  (when color
    (delq nil (mapcar (lambda (ch)
                        (unless (eq ch (nth 0 color))
                          (when-let* ((c (eas-compile-color-scale units config (list ch))))
                            (if (equal (plist-get (nth 1 c) :field) (plist-get (nth 1 color) :field))
                                (list ch (nth 1 c) (nth 2 color) nil)
                              (append c (list t))))))
                      '(:color :fill :stroke)))))

(defun eas-compile--ranges (group)
  "Map GROUP's positional scales onto its placed plot."
  (let ((x0 (plist-get group :x0)) (y0 (plist-get group :y0))
        (w (plist-get group :w)) (h (plist-get group :h)) (scales (plist-get group :scales)))
    (when (plist-get scales :x)
      (setq scales (plist-put scales :x (eas-compile-set-range (plist-get scales :x) (vector x0 (+ x0 w))))))
    (when-let* ((y (plist-get scales :y)))
      (setq scales (plist-put scales :y (eas-compile-set-range
                                         y (if (member (plist-get y :type) '("band" "point"))
                                               (vector y0 (+ y0 h)) (vector (+ y0 h) y0))))))
    (plist-put group :scales (eas-offset-set-ranges scales))
    (eas-polar-ranges group)
    (eas-independent-ranges group)
    (eas-projection-ranges group)))

(defun eas-compile--brush-span (scale range lo len)
  "Pixel span (START . SIZE) of RANGE on SCALE; the whole LO..LO+LEN without RANGE."
  (if (not (vectorp range)) (cons lo len)
    (let ((a (and scale (eas-scale-apply scale (aref range 0))))
          (b (and scale (eas-scale-apply scale (aref range 1)))))
      (and a b (cons (max lo (min a b)) (- (min (+ lo len) (max a b)) (max lo (min a b))))))))

(defun eas-compile--brushes (group state)
  "Return a brush mark for each GROUP interval param with a value in STATE."
  (let ((x0 (plist-get group :x0)) (y0 (plist-get group :y0)) out)
    (dolist (p (plist-get group :params))
      (let* ((select (plist-get p :select))
             (value (plist-get (plist-get state :params) (eas-key (plist-get p :name))))
             (scales (plist-get group :scales)))
        (when (and (or (equal select "interval") (equal (plist-get select :type) "interval"))
                   (not (equal (plist-get p :bind) "scales"))
                   (or (vectorp (plist-get value :x)) (vectorp (plist-get value :y))))
          (let ((xs (eas-compile--brush-span (plist-get scales :x) (plist-get value :x) x0 (plist-get group :w)))
                (ys (eas-compile--brush-span (plist-get scales :y) (plist-get value :y) y0 (plist-get group :h))))
            (when (and xs ys)
              (push (list :id (format "%s/brush:%s" (plist-get group :id) (plist-get p :name))
                          :mark "brush" :param (plist-get p :name) :interactive-off t :rows []
                          :items (vector (list :x (car xs) :y (car ys) :w (cdr xs) :h (cdr ys)
                                               :fill "#333333" :opacity 0.125 :stroke "#ffffff")))
                    out))))))
    (nreverse out)))

(defun eas-compile--view (group metrics state)
  "Assemble GROUP into a scene view under METRICS and STATE."
  (let* ((bounds (vector (plist-get group :x0) (plist-get group :y0) (plist-get group :w) (plist-get group :h)))
         (scales (plist-get group :scales))
         (marks (seq-map-indexed
                 (lambda (unit k)
                   (unless (plist-get unit :items)
                     (setq unit (plist-put unit :items (eas-marks-items unit (eas-independent-unit-scales group unit)
                                                                          bounds metrics)))
                     (setq unit (plist-put unit :index nil)))
                   (let ((mark (list :id (or (plist-get unit :name) (format "%s/%d" (plist-get group :id) k))
                                     :mark (plist-get (plist-get unit :mark) :type)
                                     :path (plist-get unit :path)
                                     :rows (plist-get unit :rows)
                                     :items (plist-get unit :items))))
                     (unless (plist-get unit :index)
                       (plist-put unit :index (eas-hit-index mark)))
                     (append mark (list :index (plist-get unit :index)))))
                 (plist-get group :units)))
         (legend-y (plist-get group :y0)))
    (append
     ;; A projected view is no cell: no config.view fill or frame.
     (when (eas-projection-view-p group) (list :cell :false))
     (list :id (plist-get group :id) :path (plist-get group :path) :bounds bounds
          :header (when-let* ((h (plist-get group :header)))
                    (eas-facet-header-place h bounds (or (plist-get group :header-inset) 0) metrics))
          :clip (if (eas-compile--clipped-p group state) t :false)
          ;; Vega-Lite frames each plot with config.view.stroke; terminals don't.
          :frame (let ((stroke (plist-get (eas-theme-get (plist-get metrics :config) :view) :stroke)))
                   (unless (or (eas-layout-text-p metrics) (eq stroke :null) (eas-projection-view-p group))
                     (append (list :stroke (if (stringp stroke) stroke "#ddd"))
                             ;; config.view's other stroke properties (fc-qx1.40).
                             (cl-loop with view = (eas-theme-get (plist-get metrics :config) :view)
                                      for k in '(:strokeWidth :strokeDash :strokeOpacity)
                                      for v = (plist-get view k)
                                      when (or (numberp v) (vectorp v)) append (list k v)))))
          :scales scales
          :axes (vconcat (mapcar (lambda (axis)
                                   (eas-layout-axis-place
                                    axis (plist-get scales (eas-key (plist-get axis :channel))) bounds metrics))
                                 (plist-get group :axes-model)))
          :legends (vconcat
                    (if (and (eas-layout-text-p metrics) (null (plist-get group :legend-offsets)))
                        (mapcar (lambda (legend)
                                  (prog1 (eas-legend-place
                                          legend (+ (aref bounds 0) (aref bounds 2) (plist-get metrics :legend-offset))
                                          legend-y metrics)
                                    (setq legend-y (+ legend-y (cdr (eas-legend-size legend metrics))))))
                                (plist-get group :legends-model))
                      (cl-mapcar (lambda (legend at)
                                   (eas-legend-place legend (+ (aref bounds 0) (car at)) (+ (aref bounds 1) (cdr at))
                                                       metrics))
                                 (plist-get group :legends-model) (plist-get group :legend-offsets)))
                    (mapcar (lambda (l) (eas-legend-place (nth 0 l) (nth 1 l) (nth 2 l) metrics))
                            (plist-get group :shared-legends)))
          :marks (vconcat (append (eas-compile--brushes group state) marks
                                  (delq nil (list (eas-title-view-mark group metrics)))
                                  (eas-facet-layout-view-marks group)))
          :params (vconcat (mapcar (lambda (p) (plist-get p :name)) (plist-get group :params)))))))

(defun eas-compile--title-frame (spec)
  "SPEC's title.frame: \"bounds\" or \"group\" (the default)."
  (let ((title (plist-get spec :title)))
    (or (and (eas-object-p title) (stringp (plist-get title :frame)) (plist-get title :frame)) "group")))

(defun eas-compile--title-start (groups metrics spec)
  "Left edge the title anchors to: the plots, or with frame bounds the chart's.
GROUPS give the plots, METRICS the padding and SPEC the title frame."
  (if (equal (eas-compile--title-frame spec) "bounds")
      (apply #'min (mapcar (lambda (g) (if (plist-get g :content-x1) (+ (plist-get g :x0) (plist-get g :content-x1))
                                         (plist-get metrics :pad)))
                           groups))
    (apply #'min (mapcar (lambda (g) (plist-get g :x0)) groups))))

(defun eas-compile--title-width (total groups metrics spec title)
  "TOTAL (W . H) widened so a start-anchored TITLE fits, as Vega's autosize pads.
GROUPS, METRICS and SPEC place the title as in `eas-compile--title-start'."
  (if (or (null title) (eas-layout-text-p metrics)
          (not (equal (eas-title-anchor spec metrics) "start")))
      total
    ;; The x the title is drawn at (see the scene's :title): the plots'
    ;; edge with an explicit frame, else the chart's padding.
    (let ((need (+ (if (or (equal (eas-compile--title-frame spec) "bounds")
                           (let ((tt (plist-get spec :title))) (and (eas-object-p tt) (equal (plist-get tt :frame) "group"))))
                       (eas-compile--title-start groups metrics spec)
                     (plist-get metrics :pad))
                   (let ((eas-font-family (plist-get (eas-title-style spec metrics) :font)))
                     (apply #'max (mapcar (lambda (line)
                                            (eas-layout-text-width metrics line (plist-get metrics :chart-title-size)
                                                                   (plist-get metrics :chart-title-weight)))
                                          (eas-title-lines spec))))
                   (plist-get metrics :pad))))
      (if (> need (car total)) (cons (ceiling need) (cdr total)) total))))

(defun eas-compile--title (spec)
  "The chart title text of SPEC (its lines joined by spaces), or nil."
  (eas-title-text spec))

(defun eas-compile--env (spec state)
  "Return the param values expressions see: SPEC :value defaults, then STATE."
  (let ((env nil))
    (cl-labels ((walk (node)
                  (seq-doseq (p (plist-get node :params))
                    (when (plist-member p :value)
                      (setq env (eas-plist-put env (eas-key (plist-get p :name)) (plist-get p :value)))))
                  (dolist (key '(:layer :vconcat :hconcat))
                    (seq-doseq (child (plist-get node key)) (walk child)))))
      (walk spec))
    (cl-loop for (k v) on (plist-get state :params) by #'cddr
             do (setq env (eas-plist-put env k v)))
    env))

(defvar eas-encode-count-title)

(cl-defun eas-compile-plan (spec &key rows size target cell state)
  "Everything `eas-compile' derives before items: units, scales, layout.
SPEC, ROWS, SIZE, TARGET, CELL and STATE are as in `eas-compile'.  The
runtime keeps the plan so that a selection change can patch it
\(`eas-compile-patch') instead of compiling again."

  (let* ((gc-cons-threshold (max gc-cons-threshold eas-compile-gc-threshold))
         (spec (eas-projection-expand (eas-composite-expand (eas-facet-expand (eas-overlay-expand (eas-spec-validate spec))))))
         (unsupported (car (eas-spec-unsupported spec)))
         ;; Axis and legend titles are made here, some without a config at hand.
         (eas-encode-count-title (let ((c (plist-get (plist-get spec :config) :countTitle)))
                                   (and (stringp c) c))))
    (when unsupported
      (eas-signal "UNSUPPORTED_FEATURE" (plist-get unsupported :message)
                    :path (plist-get unsupported :path) :feature (plist-get unsupported :feature)))
    (let* ((target (or target 'svg))
           (eas-compile-scales-text (eq target 'text))
           (config (eas-theme-merge eas-theme-vega-lite eas-theme-default
                                      (let ((c (plist-get spec :config))) (and (eas-object-p c) c))
                                      ;; A top-level padding overrides config.padding, as in Vega-Lite.
                                      (let ((p (plist-get spec :padding))) (and (numberp p) (list :padding p)))))
           (metrics (eas-layout-metrics target cell config))
           (cellv (plist-get metrics :cell))
           (size (cond ((and (consp size) (plist-get size :cols))
                        (cons (* (plist-get size :cols) (aref cellv 0)) (* (plist-get size :rows) (aref cellv 1))))
                       (t size)))
           (rows (if (eas-data-p rows) (plist-get rows :rows) rows))
           (env (eas-compile--env spec state))
           (tree (eas-compile-memo
                  (eas-compile--collect spec (list :path "" :rows [] :transforms nil :encoding nil :config config)
                                        env rows nil)))
           (groups (eas-place--groups tree))
           (title (eas-compile--title spec))
           (title-h (eas-title-height spec metrics)))
      (dolist (g groups) (eas-compile--scales g state metrics))
      (eas-facet-layout-share tree state metrics)
      (eas-concat-flush-prepare tree metrics)
      (eas-shared-prepare tree groups spec)
      (eas-shared-pos-prepare tree groups spec state)
      (let ((total (eas-place-layout tree metrics title-h size)))
        (dolist (g groups) (eas-compile--ranges g))
        ;; Text: a y band takes whole rows (eas-text-snap.el).
        (when (eas-layout-text-p metrics)
          (dolist (g groups) (eas-text-snap-bands g metrics)))
        ;; Vega's canvas also holds whatever the marks overhang: measure
        ;; them, lay out again around them and move the items along.
        ;; Charts fitted to a size (Emacs windows) skip this; their
        ;; overhang mostly falls in the padding, and measuring would
        ;; compute every item twice.
        (when (and (null size) (not (eas-layout-text-p metrics))
                   (eas-compile--measure-marks groups metrics state))
          (let ((origins (mapcar (lambda (g) (cons (plist-get g :x0) (plist-get g :y0))) groups)))
            (setq total (eas-place-layout tree metrics title-h nil t))
            (cl-loop for g in groups for o in origins
                     do (eas-compile--ranges g)
                     (let ((dx (- (plist-get g :x0) (car o))) (dy (- (plist-get g :y0) (cdr o))))
                       (dolist (u (plist-get g :units))
                         (plist-put u :items (eas-marks-translate (plist-get u :items) dx dy)))))))
        (unless size (setq total (eas-compile--title-width total groups metrics spec title)))
        (list :spec spec :metrics metrics :groups groups :total total :title title :env env
              :cut (plist-get tree :cut))))))

(defun eas-compile--clipped-p (group state)
  "Return non-nil when GROUP is clipped to its plot.
Vega-Lite clips zoomable views (a param bound to scales or a scale
domain from a selection), marks with clip: true, and eas clips views
zoomed in STATE."
  (or (plist-get (plist-get state :domains) (eas-key (plist-get group :id)))
      (seq-some (lambda (p) (equal (plist-get p :bind) "scales")) (plist-get group :params))
      (eas-link-domain-params (plist-get group :units))
      (seq-some (lambda (u) (eq (plist-get (plist-get u :mark) :clip) t)) (plist-get group :units))
      ;; A domain bound to a selection zooms the view, which Vega-Lite clips.
      (seq-some (lambda (u) (seq-some (lambda (ch) (plist-get (plist-get (plist-get (plist-get (plist-get u :encoding) ch) :scale) :domain) :param))
                                      '(:x :y)))
                (plist-get group :units))))

(defun eas-compile--measure-marks (groups metrics state)
  "Compute GROUPS' items and record how far they overhang each plot.
Sets :mark-over [LEFT TOP RIGHT BOTTOM] and :scope-over (series marks'
overhang on the right) in pixels; clipped views overhang nothing, as in
Vega.  Return non-nil when anything overhangs, so chrome may grow.
METRICS and STATE are as for `eas-compile--view'."

  (let (grew)
    (dolist (g groups)
      (let* ((x0 (plist-get g :x0)) (y0 (plist-get g :y0)) (w (plist-get g :w)) (h (plist-get g :h))
             (bounds (vector x0 y0 w h))
             (over (lambda (b) (if b (vector (max 0 (- x0 (aref b 0))) (max 0 (- y0 (aref b 1)))
                                             (max 0 (- (aref b 2) x0 w)) (max 0 (- (aref b 3) y0 h)))
                                 (vector 0 0 0 0))))
             mbox sbox)
        (dolist (u (plist-get g :units))
          (unless (plist-get u :items)
            (plist-put u :items (eas-marks-items u (eas-independent-unit-scales g u) bounds metrics)))
          (unless (eas-compile--clipped-p g state)
            (let ((b (eas-marks-bounds u metrics)))
              (setq mbox (eas-layout-union mbox b))
              (when (eas-marks-scope-p u) (setq sbox (eas-layout-union sbox b))))))
        (let ((m (funcall over mbox)))
          (when (seq-some #'cl-plusp m) (setq grew t))
          (plist-put g :mark-over m)
          (plist-put g :scope-over (aref (funcall over sbox) 2)))))
    grew))

(defun eas-compile-scene (plan state)
  "Assemble scene/v1 from PLAN under view STATE, reusing cached items."
  (let* ((gc-cons-threshold (max gc-cons-threshold eas-compile-gc-threshold))
         (eas-encode-count-title (let ((c (plist-get (plist-get (plist-get plan :spec) :config) :countTitle)))
                                   (and (stringp c) c)))
         (metrics (plist-get plan :metrics)) (total (plist-get plan :total))
         (spec (plist-get plan :spec)) (groups (plist-get plan :groups)) (title (plist-get plan :title)))
    (append
     (list :contract "scene/v1" :target (plist-get metrics :target)
           :size (append (list :w (car total) :h (cdr total) :cell (plist-get metrics :cell))
                         ;; A text canvas cut at its window: the width it needed.
                         (when-let* ((cut (plist-get plan :cut))) (list :cut cut)))
           :background (let ((bg (plist-get spec :background)))
                         (if (stringp bg) bg (or (eas-theme-get (plist-get metrics :config) :background) "white")))
           :config (plist-get metrics :config))
     ;; Registered font files the chart names, for the SVG's @font-face rules.
     (when-let* ((fonts (eas-font-file-scene-fonts spec (plist-get metrics :config))))
       (list :fonts (vconcat fonts)))
     (when title
       (let* ((x1 (eas-compile--title-start groups metrics spec))
              (x2 (apply #'max (mapcar (lambda (g) (+ (plist-get g :x0) (plist-get g :w))) groups)))
              (anchor (if (eas-layout-text-p metrics) "middle" (eas-title--get spec metrics :anchor :chart-title-anchor))))
         (list :title (eas-title-extra-apply (append (list :text title
                            ;; Vega-Lite's title frame "bounds": start and end are the chart's edges.
                            :x (pcase anchor ("start" (if (or (eas-layout-text-p metrics) (equal (eas-compile--title-frame spec) "bounds")
                                            ;; An explicit frame "group" anchors to the plots.
                                            (let ((tt (plist-get spec :title)))
                                              (and (eas-object-p tt) (equal (plist-get tt :frame) "group"))))
                                          x1 (plist-get metrics :pad)))
                                 ("end" (if (eas-layout-text-p metrics) x2 (- (car total) (plist-get metrics :pad))))
                                 (_ (if (eas-layout-text-p metrics) (/ (car total) 2.0) (/ (+ x1 x2) 2.0))))
                            :y (+ (plist-get metrics :pad)
                                  (if (eas-layout-text-p metrics) 0
                                    (- (eas-layout--round (* 0.8 (eas-title--get spec metrics :fontSize :chart-title-size)))
                                       (eas-layout--round (* 0.79 (eas-title--get spec metrics :fontSize :chart-title-size))))))
                            :align (pcase anchor ("start" "left") ("end" "right") (_ "center")) :baseline "top"
                            :fontSize (if (eas-layout-text-p metrics) (plist-get metrics :chart-title-size)
                                        (eas-title--get spec metrics :fontSize :chart-title-size))
                            :fontWeight (eas-title--get spec metrics :fontWeight :chart-title-weight))
                      (let ((lines (eas-title-lines spec)))
                        (when (cdr lines)
                          (list :lines (vconcat lines)
                                :lineHeight (if (eas-layout-text-p metrics) (plist-get metrics :chart-title-size)
                                              (+ (eas-title--get spec metrics :fontSize :chart-title-size) 2)))))
                      (let ((color (plist-get (eas-title--object spec) :color)))
                        (when (stringp color) (list :color color)))) spec metrics))))
     (list :views (vconcat (mapcar (lambda (v) (eas-text-labels-thin
                                                (eas-axis-fit-text (eas-legend-fit-width v (car total) metrics)
                                                                   (car total) metrics)
                                                metrics))
                                   (eas-facet-title-add (mapcar (lambda (g) (eas-compile--view g metrics state)) groups)
                                                        groups metrics)))
           :params (vconcat (apply #'append (mapcar (lambda (g) (plist-get g :params)) groups)))))))

(cl-defun eas-compile (spec &key rows size target cell state)
  "Compile resolved chart/v1 SPEC to scene/v1.
ROWS (a vector of row plists, or data/v1) replaces the root data.
TARGET is `svg' (default) or `text'; CELL is the [W H] pixel size of a
text cell (default [7 14]).  SIZE is (W . H) in pixels to fit, or for
the text target (:cols C :rows R); nil keeps the spec's own sizes.
STATE is view state: (:domains (VIEW-KEY (:x [LO HI]) ...) :params ...).
Signals UNSUPPORTED_FEATURE (with the JSON path) for anything outside
the native subset."
  (let ((scene (eas-time-with-local-zone
                (eas-compile-scene (eas-compile-plan spec :rows rows :size size :target target :cell cell :state state)
                                   state))))
    ;; Selections with an initial value start non-empty, as Vega draws them.
    (if-let* (((null state)) (init (eas-params-initial-state scene)))
        (eas-params-with-state init
          (eas-compile spec :rows rows :size size :target target :cell cell :state init))
      scene)))

(provide 'eas-compile)
;;; eas-compile.el ends here
