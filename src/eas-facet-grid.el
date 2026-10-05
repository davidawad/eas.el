;;; eas-facet-grid.el --- every Vega-Lite facet as a grid of cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2.  Vega-Lite facets a view three ways: a row and/or column
;; field in the encoding, a top-level {"facet": {"row", "column"},
;; "spec"}, and a wrapped facet ({"facet": FIELD-DEF, "columns": N} or
;; encoding.facet with "columns").  `eas-facet-grid-lower' rewrites
;; each into the same shape, a vconcat of hconcat rows with one cell per
;; level (per combination of levels in a grid), so compile, hit testing
;; and both renderers draw it as concatenated views:
;;
;; - each cell filters the data to its level(s), then runs the inner
;;   spec's own transforms;
;; - bins are computed over all the data (a bin extent is written into
;;   every cell), as Vega-Lite bins before it partitions;
;; - only the outer cells keep their axes: the y axis on the first
;;   column, the x axis under the last cell of each column; the others
;;   keep their grid lines (Vega-Lite's row headers and column footers);
;; - the composition carries x-eas.facet, which eas-facet-layout.el
;;   reads to share the cells' scales, lay them out as Vega's trellis
;;   (labels, field titles, spacing between plots) and draw the headers.
;;
;; Header properties the native layout does not draw are recorded with
;; their JSON paths, and `check' reports them as UNSUPPORTED_FEATURE.

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-expr)
(require 'eas-encode)
(require 'eas-format)
(require 'eas-compile-sort)

(declare-function eas-compile--less "eas-compile-scales")
(declare-function eas-vl-lower--view "eas-vl-lower")

(defconst eas-facet-grid--outer-keys
  '(:$schema :data :transform :config :title :description :background :padding
    :usermeta :autosize :datasets :name)
  "Keys a facet keeps on the composition rather than on each cell.")

(defconst eas-facet-grid--drop-keys
  '(:facet :spec :columns :spacing :resolve :align :bounds :center)
  "Facet keys the lowered composition does not carry.")

(defconst eas-facet-grid-header-keys
  '(:title :labelAngle :labelAlign :labelPadding :labelFontSize :labelFontWeight
    :labelColor :labelFont :labelExpr :labels :format
    :titleFontSize :titleFontWeight :titleColor :titleFont :titlePadding)
  "Header properties the native facet layout honors.")

;;; Parsing

(defun eas-facet-grid--def-p (def)
  "Non-nil when DEF is a facet field definition."
  (and (eas-object-p def) def (stringp (plist-get def :field))))

(defun eas-facet-grid--parse (spec)
  "SPEC's facet definitions as a plist, or nil when SPEC is no facet.
Keys: :row :column :facet (field defs), :columns and :path (the JSON
pointer of the definitions)."
  (cond
   ;; {"facet": {...}, "spec": {...}}
   ((and (eas-object-p (plist-get spec :facet)) (plist-get spec :facet)
         (eas-object-p (plist-get spec :spec)) (plist-get spec :spec))
    (let ((f (plist-get spec :facet)))
      (if (eas-facet-grid--def-p f)
          (list :facet f :columns (plist-get spec :columns) :path "/facet")
        (when (or (eas-facet-grid--def-p (plist-get f :row)) (eas-facet-grid--def-p (plist-get f :column)))
          (append (list :path "/facet")
                  (when (eas-facet-grid--def-p (plist-get f :row)) (list :row (plist-get f :row)))
                  (when (eas-facet-grid--def-p (plist-get f :column)) (list :column (plist-get f :column))))))))
   ;; a unit (or layer) whose encoding holds row, column or facet
   ((and (or (plist-get spec :mark) (plist-get spec :layer)) (eas-object-p (plist-get spec :encoding)))
    (let* ((enc (plist-get spec :encoding))
           (defs (cl-loop for ch in '(:row :column :facet)
                          when (eas-facet-grid--def-p (plist-get enc ch))
                          append (list ch (plist-get enc ch)))))
      (when defs
        (append defs (list :path "/encoding")
                (when (plist-get defs :facet) (list :columns (plist-get (plist-get defs :facet) :columns)))))))))

(defun eas-facet-grid--facet (spec)
  "SPEC's facet (see `eas-facet-grid--parse') with :inner (the cell
spec), :outer (the composition's keys), :spacing and :resolve; or nil."
  (when-let* ((f (eas-facet-grid--parse spec)))
    (let* ((outer (cl-loop for k in eas-facet-grid--outer-keys
                           when (plist-member spec k) append (list k (plist-get spec k))))
           (inner (if (plist-get spec :spec) (plist-get spec :spec)
                    (let ((u spec) (enc (plist-get spec :encoding)))
                      (dolist (k (append eas-facet-grid--outer-keys eas-facet-grid--drop-keys))
                        (setq u (eas--plist-without u k)))
                      (dolist (k '(:row :column :facet)) (setq enc (eas--plist-without enc k)))
                      (eas-plist-put u :encoding enc)))))
      (append f (list :inner inner :outer outer :spacing (plist-get spec :spacing)
                      :resolve (plist-get spec :resolve)
                      :layout (cl-loop for k in '(:align :bounds :center)
                                       when (plist-member spec k) append (list k (plist-get spec k))))))))

;;; Levels and labels

(defun eas-facet-grid--field (def)
  "The field DEF's levels are read from (its timeUnit's derived field)."
  (if (stringp (plist-get def :timeUnit))
      (format "%s_%s" (plist-get def :timeUnit) (plist-get def :field))
    (plist-get def :field)))

(defun eas-facet-grid--inner-fields (inner)
  "Fields INNER's encodings read, and whether one aggregates: (FIELDS . AGG)."
  (let (fields agg)
    (cl-labels ((walk (node)
                  (cl-loop for (_ d) on (plist-get node :encoding) by #'cddr
                           when (and (eas-object-p d) d)
                           do (when (stringp (plist-get d :field)) (push (plist-get d :field) fields))
                           (when (plist-get d :aggregate) (setq agg t)))
                  (seq-doseq (c (plist-get node :layer)) (walk c))))
      (walk inner))
    (cons fields agg)))

(defun eas-facet-grid--sort-lost-p (sort inner)
  "Non-nil when Vega-Lite cannot sort by SORT's field: INNER aggregates and
the field is none of its encodings' fields.  Vega-Lite computes the cells'
aggregate before the facet's domain then, which drops the field, and the
levels keep the data's order (the official trellis_area_seattle does)."
  (let ((f (eas-facet-grid--inner-fields inner)))
    (and (cdr f) (not (member (plist-get sort :field) (car f))))))

(defun eas-facet-grid--valid-rows (rows inner)
  "ROWS a non-path INNER unit keeps: Vega-Lite filters out rows whose
continuous x or y field is invalid before the facet, so a level with no
valid row gets no cell."
  (let* ((mark (plist-get inner :mark)) (type (if (stringp mark) mark (plist-get mark :type)))
         (enc (plist-get inner :encoding))
         (keys (cl-loop for ch in '(:x :y)
                        for d = (plist-get enc ch)
                        when (and (eas-object-p d) (stringp (plist-get d :field)) (not (plist-get d :aggregate))
                                  (not (plist-get d :bin)) (not (plist-get d :timeUnit))
                                  (member (plist-get d :type) '("quantitative" "temporal")))
                        collect (eas-key (plist-get d :field)))))
    (if (or (null keys) (null type) (member type '("line" "area" "trail"))) rows
      (seq-remove (lambda (r) (seq-some (lambda (k) (memq (plist-get r k) '(nil :null))) keys)) rows))))

(defun eas-facet-grid--levels (def rows &optional inner)
  "DEF's levels over ROWS, ordered by DEF's sort (Vega's ascending order,
null first, by default).  INNER is the cell spec."
  (let* ((key (eas-key (eas-facet-grid--field def)))
         (seen (make-hash-table :test 'equal)) values)
    (seq-doseq (r (eas-facet-grid--valid-rows rows inner))
      (let ((v (plist-get r key)))
        (when (null v) (setq v :null))
        (unless (gethash v seen) (puthash v t seen) (push v values))))
    (setq values (nreverse values))
    (let ((sort (plist-get def :sort))
          (asc (lambda (vs) (sort (copy-sequence vs) #'eas-compile--less))))
      (cond
       ((eq sort :null) values)
       ((equal sort "descending") (nreverse (funcall asc values)))
       ((vectorp sort) (append (seq-filter (lambda (v) (member v values)) (append sort nil))
                               (seq-remove (lambda (v) (seq-contains-p sort v)) (funcall asc values))))
       ((and (eas-object-p sort) sort (plist-get sort :field) (eas-facet-grid--sort-lost-p sort inner))
        values)
       ((and (eas-object-p sort) sort (plist-get sort :field))
        (eas-compile-sort-by-field sort (list :field (eas-facet-grid--field def)) rows (funcall asc values)))
       (t (funcall asc values))))))

(defun eas-facet-grid--header (def channel config)
  "DEF's header over CONFIG's header and headerRow/headerColumn/headerFacet."
  (let ((out nil))
    (dolist (h (list (plist-get config :header)
                     (plist-get config (pcase channel (:row :headerRow) (:column :headerColumn) (_ :headerFacet)))
                     (plist-get def :header)))
      (when (and (eas-object-p h) h)
        (cl-loop for (k v) on h by #'cddr do (setq out (eas-plist-put out k v)))))
    out))

(defun eas-facet-grid--label (def header value)
  "Header label of level VALUE of facet DEF under HEADER."
  (let* ((fmt (plist-get header :format))
         (label (cond ((memq value '(nil :null)) "null")
                      ((plist-get def :timeUnit)
                       (if (stringp fmt) (eas-expr--time-format value fmt eas-time-zone)
                         (eas-encode-format-value '(:type "temporal") value)))
                      ((and (stringp fmt) (numberp value)) (eas-format-number fmt value))
                      (t (eas-expr--string value))))
         (expr (plist-get header :labelExpr)))
    (if (stringp expr)
        (eas-expr--string (eas-expr-evaluate expr (list :value value :label label)))
      label)))

(defun eas-facet-grid--title (def header)
  "Field title of facet DEF under HEADER (nil when switched off)."
  (let ((title (cond ((plist-member def :title) (plist-get def :title))
                     ((plist-member header :title) (plist-get header :title))
                     ((plist-get def :timeUnit)
                      (eas-encode-title (list :field (eas-facet-grid--field def) :derived "timeUnit"
                                              :source (plist-get def :field))))
                     (t (plist-get def :field)))))
    (cond ((stringp title) (unless (string-empty-p title) title))
          ((vectorp title) (mapconcat #'eas-expr--string title " "))
          (t nil))))

(defconst eas-facet-grid-def-keys
  '(:field :type :title :header :sort :timeUnit :columns :spacing)
  "Facet field definition properties the native facet honors.")

(defun eas-facet-grid--unsupported (def path)
  "Findings for the properties of facet DEF (and its header) the native
layout ignores.  PATH is the definition's JSON pointer."
  (append
   (cl-loop for (k _) on def by #'cddr
            unless (memq k eas-facet-grid-def-keys)
            collect (list :feature (concat "facet/" (eas-key-name k))
                          :path (format "%s/%s" path (eas-key-name k)) :unknown t))
   (cl-loop for (k _) on (let ((h (plist-get def :header))) (and (eas-object-p h) h)) by #'cddr
            unless (memq k eas-facet-grid-header-keys)
            collect (list :feature (concat "header/" (eas-key-name k))
                          :path (format "%s/header/%s" path (eas-key-name k)) :unknown t))))

;;; Cells

(defun eas-facet-grid--bare (def)
  "Positional DEF whose axis keeps its grid only (an inner facet cell)."
  (let ((axis (plist-get def :axis)))
    (if (eq axis :null) def
      (let ((a (if (eas-object-p axis) axis nil)))
        (dolist (kv '((:domain . :false) (:ticks . :false) (:labels . :false) (:title . :null)))
          (setq a (eas-plist-put a (car kv) (cdr kv))))
        (eas-plist-put def :axis a)))))

(defun eas-facet-grid--axes (node show-x show-y)
  "NODE with the x (unless SHOW-X) and y (unless SHOW-Y) axes bared, deeply."
  (let ((enc (plist-get node :encoding)))
    (dolist (pair (list (cons :x show-x) (cons :y show-y)))
      (let ((d (plist-get enc (car pair))))
        (when (and (not (cdr pair)) (eas-object-p d) d (not (plist-member d :value)))
          (setq enc (eas-plist-put enc (car pair) (eas-facet-grid--bare d))))))
    (when enc (setq node (eas-plist-put node :encoding enc)))
    (when (vectorp (plist-get node :layer))
      (setq node (eas-plist-put node :layer (vconcat (mapcar (lambda (c) (eas-facet-grid--axes c show-x show-y))
                                                             (plist-get node :layer))))))
    node))

(defun eas-facet-grid--extent (field rows)
  "[MIN MAX] of numeric FIELD over ROWS, or nil."
  (let ((key (eas-key field)) lo hi)
    (seq-doseq (r rows)
      (let ((v (plist-get r key)))
        (when (numberp v)
          (setq lo (if lo (min lo v) v) hi (if hi (max hi v) v)))))
    (and lo (vector lo hi))))

(defun eas-facet-grid--bins (node rows)
  "NODE with every binned field's extent taken over all ROWS, deeply."
  (let ((enc (plist-get node :encoding)))
    (cl-loop for (ch d) on (plist-get node :encoding) by #'cddr
             do (let ((bin (and (eas-object-p d) d (plist-get d :bin))))
                  (when (and bin (not (memq bin '(:false :null))) (not (equal bin "binned"))
                             (stringp (plist-get d :field))
                             (not (and (eas-object-p bin) (plist-get bin :extent))))
                    (when-let* ((ext (eas-facet-grid--extent (plist-get d :field) rows)))
                      (setq enc (eas-plist-put enc ch (eas-plist-put d :bin (eas-plist-put (if (eas-object-p bin) bin nil)
                                                                                           :extent ext))))))))
    (when enc (setq node (eas-plist-put node :encoding enc)))
    (when (vectorp (plist-get node :layer))
      (setq node (eas-plist-put node :layer (vconcat (mapcar (lambda (c) (eas-facet-grid--bins c rows))
                                                             (plist-get node :layer))))))
    node))

(defun eas-facet-grid--filter (def value)
  "A filter transform keeping DEF's level VALUE."
  (list :filter (if (eq value :null)
                    (list :field (eas-facet-grid--field def) :valid :false)
                  (list :field (eas-facet-grid--field def) :equal value))))

;;; Lowering

(defun eas-facet-grid--param-names (node)
  "Names of the params declared anywhere in NODE."
  (let (names)
    (cl-labels ((walk (v)
                  (cond ((vectorp v) (seq-do #'walk v))
                        ((and (consp v) (keywordp (car v)))
                         (seq-doseq (p (plist-get v :params))
                           (when (stringp (plist-get p :name)) (push (plist-get p :name) names)))
                         (cl-loop for (_ x) on v by #'cddr do (walk x))))))
      (walk node))
    names))

(defun eas-facet-grid--static-p (transforms inner)
  "Non-nil when TRANSFORMS read no param that INNER (or they) declare, so
compile may run them once for every cell (eas-facet-layout.el)."
  (let ((text (format "%S" transforms)))
    (and (not (string-match-p ":param" text))
         (seq-every-p (lambda (n) (not (string-match-p (concat "\\_<" (regexp-quote n) "\\_>") text)))
                      (eas-facet-grid--param-names (list :t transforms :i inner))))))

(defun eas-facet-grid--rows (spec inherited)
  "SPEC's rows after its own transforms (from INHERITED rows without
inline data), or nil."
  (let* ((values (plist-get (plist-get spec :data) :values))
         (rows (if (vectorp values)
                   (seq-map (lambda (v) (if (and (consp v) (keywordp (car v))) v (list :data v))) values)
                 inherited)))
    (when rows
      (condition-case nil
          (eas-transform-run (or (plist-get spec :transform) []) rows)
        (error nil)))))

(defun eas-facet-grid--meta (f rlevels clevels wlevels headers label)
  "x-eas.facet of facet F: grid size, spacing, titles and labels.
RLEVELS, CLEVELS and WLEVELS are the row, column and wrapped levels,
HEADERS an alist (CHANNEL . HEADER), LABEL a function (CHANNEL DEF VALUE)."
  (let* ((rdef (plist-get f :row)) (cdef (plist-get f :column)) (wrap (plist-get f :facet))
         (config (plist-get (plist-get f :outer) :config))
         (n (length wlevels))
         (ncols (cond (wrap (let ((c (plist-get f :columns))) (if (and (numberp c) (> c 0)) (min c n) n)))
                      (t (length clevels))))
         ;; A facet field def may carry the spacing itself (encoding.facet).
         (spacing (or (plist-get f :spacing) (plist-get wrap :spacing)))
         (dspacing (or (and (eas-object-p config) (plist-get (plist-get config :facet) :spacing)) 20))
         (path (plist-get f :path)))
    (list :rows (if wrap (ceiling n (float (max 1 ncols))) (length rlevels)) :columns ncols
          :wrap (if wrap t :false) :count (if wrap n (* (length rlevels) ncols))
          :spacing-row (cond ((numberp spacing) spacing) ((numberp (plist-get spacing :row)) (plist-get spacing :row))
                             (t dspacing))
          :spacing-column (cond ((numberp spacing) spacing) ((numberp (plist-get spacing :column)) (plist-get spacing :column))
                                (t dspacing))
          :independent (cl-loop for (k v) on (plist-get (plist-get f :resolve) :scale) by #'cddr
                                when (equal v "independent") collect k)
          :row-title (and rdef (eas-facet-grid--title rdef (cdr (assq :row headers))))
          :column-title (or (and cdef (eas-facet-grid--title cdef (cdr (assq :column headers))))
                            (and wrap (eas-facet-grid--title wrap (cdr (assq :facet headers)))))
          :row-header (cdr (assq :row headers))
          :column-header (or (cdr (assq :column headers)) (cdr (assq :facet headers)))
          :static (if (eas-facet-grid--static-p (plist-get (plist-get f :outer) :transform) (plist-get f :inner)) t :false)
          :row-labels (and rdef (vconcat (mapcar (lambda (v) (funcall label :row rdef v)) rlevels)))
          :column-labels (cond (cdef (vconcat (mapcar (lambda (v) (funcall label :column cdef v)) clevels)))
                               (wrap (vconcat (mapcar (lambda (v) (funcall label :facet wrap v)) wlevels))))
          ;; What check and supported.json see of the facet it was.
          :features (append (if (equal path "/facet")
                                (list (list :feature "composition/facet" :path "/facet"))
                              (cl-loop for (ch d) in (list (list :row rdef) (list :column cdef) (list :facet wrap))
                                       when d collect (list :feature (concat "encoding/" (eas-key-name ch))
                                                            :path (concat "/encoding/" (eas-key-name ch))))))
          :unsupported (append (and rdef (eas-facet-grid--unsupported rdef (concat path "/row")))
                               (and cdef (eas-facet-grid--unsupported cdef (concat path "/column")))
                               (and wrap (eas-facet-grid--unsupported wrap (if (equal path "/facet") path (concat path "/facet"))))
                               ;; The trellis always aligns all cells on their full bounds.
                               (cl-loop for k in '(:align :bounds :center)
                                        when (plist-member (plist-get f :layout) k)
                                        collect (list :feature (concat "facet/" (eas-key-name k))
                                                      :path (concat "/" (eas-key-name k)) :unknown t))))))

(defun eas-facet-grid--lower (spec f rows)
  "Facet SPEC described by F lowered over ROWS (its data after its transforms)."
  (let* ((outer (plist-get f :outer))
         (config (let ((c (plist-get spec :config))) (and (eas-object-p c) c)))
         (time-tx (cl-loop for ch in '(:row :column :facet)
                           for d = (plist-get f ch)
                           when (and d (stringp (plist-get d :timeUnit)))
                           collect (list :timeUnit (plist-get d :timeUnit) :field (plist-get d :field)
                                         :as (eas-facet-grid--field d))))
         (rows (if time-tx (eas-transform-run (vconcat time-tx) rows) rows))
         (outer (if time-tx (eas-plist-put outer :transform (vconcat (plist-get outer :transform) time-tx)) outer))
         (wrap (plist-get f :facet)) (rdef (plist-get f :row)) (cdef (plist-get f :column))
         (rlevels (if rdef (eas-facet-grid--levels rdef rows (plist-get f :inner)) '(nil)))
         (clevels (if cdef (eas-facet-grid--levels cdef rows (plist-get f :inner)) '(nil)))
         (wlevels (and wrap (eas-facet-grid--levels wrap rows (plist-get f :inner))))
         (headers (cl-loop for (ch d) in (list (list :row rdef) (list :column cdef) (list :facet wrap))
                           when d collect (cons ch (eas-facet-grid--header d ch config))))
         (label (lambda (ch d v) (eas-facet-grid--label d (cdr (assq ch headers)) v)))
         (meta (eas-facet-grid--meta f rlevels clevels wlevels headers label))
         (n (length wlevels)) (ncols (plist-get meta :columns)) (nrows (plist-get meta :rows))
         (inner (eas-facet-grid--bins (let ((in (plist-get f :inner)))
                                        (if (fboundp 'eas-vl-lower--view) (eas-vl-lower--view in) in))
                                      rows))
         (cell (lambda (i j k filters labels)
                 ;; An independent scale keeps its axis in every cell.
                 (let* ((indep (plist-get meta :independent))
                        (show-x (or (memq :x indep) (if wrap (>= (+ k ncols) n) (= i (1- nrows)))))
                        (c (eas-facet-grid--axes inner show-x (or (memq :y indep) (= j 0))))
                        (text (string-join (delq nil labels) " · ")))
                   (setq c (eas-plist-put c :transform (vconcat filters (plist-get inner :transform))))
                   (eas-plist-put c :x-eas
                                  (let ((x (eas-plist-put (plist-get inner :x-eas) :facet-cell
                                                          ;; The levels its leading filters keep (eas-facet-layout.el
                                                          ;; partitions the rows by them instead).
                                                          (list :row i :column j :index k :filters (length filters)
                                                                :keys (vconcat (mapcar #'cadr filters))))))
                                    (if (string-empty-p text) x
                                      (eas-plist-put x :header (list :text text :orient "top"
                                                                     :labels (vector text)))))))))
         (grid
          (if wrap
              (cl-loop for i from 0 below nrows
                       collect (list :hconcat
                                     (vconcat (cl-loop for j from 0 below ncols
                                                       for k = (+ (* i ncols) j)
                                                       while (< k n)
                                                       collect (let ((v (nth k wlevels)))
                                                                 (funcall cell i j k (list (eas-facet-grid--filter wrap v))
                                                                          (list (funcall label :facet wrap v))))))))
            (cl-loop for rv in rlevels for i from 0
                     collect (list :hconcat
                                   (vconcat (cl-loop for cv in clevels for j from 0
                                                     collect (funcall cell i j (+ (* i ncols) j)
                                                                      (delq nil (list (and rdef (eas-facet-grid--filter rdef rv))
                                                                                      (and cdef (eas-facet-grid--filter cdef cv))))
                                                                      (list (and rdef (funcall label :row rdef rv))
                                                                            (and cdef (funcall label :column cdef cv)))))))))))
    (append outer (list :vconcat (vconcat grid)
                        :x-eas (eas-plist-put (plist-get outer :x-eas) :facet meta)))))

(defun eas-facet-grid--has-facet-p (spec)
  "Non-nil when SPEC or a view concatenated in it is a facet."
  (and spec (eas-object-p spec)
       (or (eas-facet-grid--parse spec)
           (seq-some #'eas-facet-grid--has-facet-p (plist-get spec :vconcat))
           (seq-some #'eas-facet-grid--has-facet-p (plist-get spec :hconcat)))))

(defun eas-facet-grid--lower-tree (spec inherited)
  "SPEC with its facets lowered; INHERITED is a function giving the rows a
nested view gets from its parent (computed only when a facet needs them)."
  (cond
   ((not (eas-facet-grid--has-facet-p spec)) spec)
   ((eas-facet-grid--facet spec)
    (if-let* ((rows (eas-facet-grid--rows spec (funcall inherited))))
        (eas-facet-grid--lower spec (eas-facet-grid--facet spec) rows)
      spec))
   (t (let* ((cache 'unset)
             (rows (lambda () (if (eq cache 'unset)
                                  (setq cache (eas-facet-grid--rows spec (funcall inherited)))
                                cache)))
             (out spec))
        (dolist (key '(:vconcat :hconcat))
          (when (vectorp (plist-get out key))
            (setq out (eas-plist-put out key (vconcat (mapcar (lambda (c) (eas-facet-grid--lower-tree c rows))
                                                              (plist-get out key)))))))
        out))))

(defvar eas-facet-grid--memo nil
  "The last (KEY . OUTPUT) of `eas-facet-grid-lower'.  Parse, check and
compile lower the same spec in turn; KEY is its data rows (by identity),
the rest of it and the time zone.")

(defun eas-facet-grid--memo-key (spec)
  "SPEC's memo key: its inline rows (compared by identity) and the rest."
  (list (plist-get (plist-get spec :data) :values) (eas--plist-without spec :data) eas-time-zone))

(defun eas-facet-grid-lower (spec)
  "SPEC with every facet over inline data lowered to a grid of cells.
A facet whose data is not inline is left alone (and reported unsupported)."
  (let ((key (eas-facet-grid--memo-key spec)) (memo (car eas-facet-grid--memo)))
    (if (and memo (eq (car memo) (car key)) (equal (cdr memo) (cdr key)))
        (cdr eas-facet-grid--memo)
      (let ((out (eas-facet-grid--lower-tree spec (lambda () nil))))
        (setq eas-facet-grid--memo (cons key out))
        out))))

;;; Findings for check

(defun eas-facet-grid-features (spec)
  "Features of the facets lowered in SPEC, and the header properties they
ignore (UNSUPPORTED_FEATURE findings), under their original paths."
  (let (out)
    (cl-labels ((walk (node)
                  (when (and node (eas-object-p node))
                    (let ((meta (plist-get (plist-get node :x-eas) :facet)))
                      (dolist (u (append (plist-get meta :features) (plist-get meta :unsupported)))
                        (push u out)))
                    (dolist (key '(:vconcat :hconcat :layer))
                      (seq-doseq (c (plist-get node key)) (walk c))))))
      (walk spec))
    (nreverse out)))

(provide 'eas-facet-grid)
;;; eas-facet-grid.el ends here
