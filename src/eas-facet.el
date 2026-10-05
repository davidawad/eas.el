;;; eas-facet.el --- row and column facets as concatenated cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A unit with a `row' (or `column') field is a facet: one
;; cell per value, stacked down (or across), sharing scales, each
;; labelled by a header.  `eas-facet-expand' rewrites it before compile
;; into a vconcat (hconcat) of cells, each filtered to its value, so
;; layout, hit testing and both renderers need nothing new:
;;
;; - shared scales: discrete x, y, color, fill, stroke, shape, size and
;;   opacity domains are computed over all the data and written into
;;   every cell; continuous x and y domains become the data's extent,
;;   niced (Vega-Lite's shared resolution)
;; - legends are drawn once, by the first cell
;; - each cell carries x-eas.header (:text :orient), Vega-Lite's header
;;   label: 10px, black, 10px off the plot, rotated -90 for rows.  The
;;   header's own title (the field name) is not drawn.

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-layout)

(declare-function eas-compile--discrete-domain "eas-compile-scales")

;;; Top-level facet: {"facet": {"row": F}, "spec": S}

(defun eas-facet--sorted (values)
  "VALUES sorted ascending (numbers before strings), duplicates removed."
  (sort (delete-dups (copy-sequence values))
        (lambda (a b) (if (and (numberp a) (numberp b)) (< a b) (string< (format "%s" a) (format "%s" b))))))

(defun eas-facet-lower (spec)
  "SPEC with a top-level facet over inline data lowered to a concat of cells.
{\"facet\": {\"row\": F}, \"spec\": S} with every scale resolved independent
is the same chart as a vconcat (row) or hconcat (column) of S once per value
of F, each cell filtered to its value and headed by it.  Any other facet is
left alone and reported unsupported."
  (let* ((facet (plist-get spec :facet)) (inner (plist-get spec :spec))
         (channel (and (eas-object-p facet) (cond ((plist-get facet :row) :row) ((plist-get facet :column) :column))))
         (def (and channel (plist-get facet channel)))
         (field (and (eas-object-p def) (plist-get def :field)))
         (values (plist-get (plist-get spec :data) :values))
         (scale (plist-get (plist-get spec :resolve) :scale)))
    (if (not (and field (eas-object-p inner) (vectorp values)
                  scale (cl-loop for (_ v) on scale by #'cddr always (equal v "independent"))))
        spec
      (let* ((key (eas-key field))
             (levels (let ((vs (seq-remove (lambda (v) (memq v '(nil :null)))
                                           (seq-map (lambda (r) (plist-get r key)) values))))
                       (if (vectorp (plist-get def :sort)) (append (plist-get def :sort) nil)
                         (let ((s (eas-facet--sorted vs))) (if (equal (plist-get def :sort) "descending") (nreverse s) s)))))
             (header (let ((h (plist-get def :header))) (and (eas-object-p h) h)))
             (labels (vconcat (mapcar (lambda (v) (format "%s" v)) levels)))
             (spacing (let ((s (plist-get spec :spacing))) (cond ((numberp s) s) ((eas-object-p s) (plist-get s channel)))))
             (cells (mapcar
                     (lambda (v)
                       (let ((cell (eas-plist-put (copy-sequence inner) :transform
                                                  (vconcat (list (list :filter (list :field field :equal v)))
                                                           (plist-get inner :transform)))))
                         (eas-plist-put cell :x-eas
                                        (eas-plist-put (plist-get inner :x-eas) :header
                                                       (append (list :text (format "%s" v) :labels labels
                                                                     :orient (if (eq channel :row) "left" "top")
                                                                     :fontSize (or (plist-get header :labelFontSize) 10))
                                                               (when (numberp (plist-get header :labelAngle))
                                                                 (list :angle (plist-get header :labelAngle))))))))
                     levels))
             (out (eas--plist-without (eas--plist-without (eas--plist-without spec :facet) :spec) :resolve)))
        (setq out (eas-plist-put out (if (eq channel :row) :vconcat :hconcat) (vconcat cells)))
        (if spacing (eas-plist-put out :spacing spacing) (eas--plist-without out :spacing))))))

(defvar eas-spec-rewrite-functions)
(add-hook 'eas-spec-rewrite-functions #'eas-facet-lower t)

(defconst eas-facet-header-size 10 "Vega-Lite's header labelFontSize.")
(defconst eas-facet-header-padding 10 "Vega-Lite's header labelPadding.")

(defconst eas-facet--channels '(:x :y :color :fill :stroke :shape :size :opacity)
  "Channels whose scales facets share.")

(defun eas-facet--domain (def rows)
  "The shared domain of channel DEF over ROWS, or nil to leave it alone."
  (let ((field (plist-get def :field)) (scale (plist-get def :scale)))
    (when (and (stringp field) (not (plist-get def :aggregate)) (not (eas-true-p (plist-get def :bin)))
               (not (plist-get def :timeUnit))
               (not (and (eas-object-p scale) (plist-get scale :domain))))
      (let* ((key (eas-key field))
             (values (delq nil (seq-map (lambda (r) (let ((v (plist-get r key))) (unless (eq v :null) v))) rows))))
        (if (member (plist-get def :type) '("nominal" "ordinal"))
            (eas-compile--discrete-domain (list (cons (list :rows rows :encoding nil) def)) values)
          (let ((nums (seq-filter #'numberp values)))
            (when (and nums (memq (plist-get def :channel) '(:x :y)))
              (vector (apply #'min nums) (apply #'max nums)))))))))

(defun eas-facet--cell (spec key value i domains)
  "Cell I of facet SPEC (facet channel KEY) showing VALUE, with shared DOMAINS."
  (let* ((enc (eas--plist-without (plist-get spec :encoding) key))
         (fdef (plist-get (plist-get spec :encoding) key)))
    (cl-loop for (ch d) on enc by #'cddr
             do (let ((dom (cdr (assq ch domains))))
                  (when (or dom (and (> i 0) (memq ch '(:color :fill :stroke :shape :size :opacity))))
                    (let* ((scale (if (eas-object-p (plist-get d :scale)) (plist-get d :scale) nil))
                           (d (if dom (plist-put (copy-sequence d) :scale
                                                 (append (list :domain dom)
                                                         (when (and (not (member (plist-get d :type) '("nominal" "ordinal")))
                                                                    (not (plist-member scale :nice)))
                                                           (list :nice t))
                                                         scale))
                                d))
                           (d (if (and (> i 0) (not (memq ch '(:x :y)))) (plist-put (copy-sequence d) :legend :null) d)))
                      (setq enc (eas-plist-put enc ch d))))))
    (append (list :mark (plist-get spec :mark) :encoding enc
                  :transform (vector (list :filter (list :field (plist-get fdef :field) :equal value)))
                  :x-eas (list :header (list :text (eas-expr--string value)
                                             :orient (if (eq key :row) "left" "top"))))
            (cl-loop for k in '(:width :height :params :name)
                     when (plist-member spec k) append (list k (plist-get spec k))))))

(defun eas-facet-expand (spec)
  "SPEC with a row or column facet rewritten as concatenated cells."
  (let* ((enc (plist-get spec :encoding))
         (key (seq-find (lambda (k) (let ((d (plist-get enc k))) (and (eas-object-p d) d (plist-get d :field))))
                        '(:row :column))))
    (if (not (and key (plist-get spec :mark)))
        spec
      (let* ((data (plist-get spec :data))
             (rows (eas-transform-run (or (plist-get spec :transform) [])
                                      (if (vectorp (plist-get data :values))
                                          (seq-map (lambda (v) (if (and (consp v) (keywordp (car v))) v (list :data v)))
                                                   (plist-get data :values))
                                        [])))
             (fdef (plist-get enc key))
             (fkey (eas-key (plist-get fdef :field)))
             (values (let ((vs (delete-dups (delq nil (seq-map (lambda (r) (plist-get r fkey)) rows)))))
                       (cond ((eq (plist-get fdef :sort) :null) vs)
                             ((equal (plist-get fdef :sort) "descending")
                              (reverse (sort vs (lambda (a b) (string< (format "%s" a) (format "%s" b))))))
                             (t (sort vs (lambda (a b) (if (and (numberp a) (numberp b)) (< a b)
                                                         (string< (format "%s" a) (format "%s" b)))))))))
             (domains (cl-loop for ch in eas-facet--channels
                               for d = (plist-get enc ch)
                               for dom = (and (eas-object-p d) d
                                              (eas-facet--domain (append (list :channel ch) d) rows))
                               when dom collect (cons ch dom))))
        (append (cl-loop for k in '(:$schema :data :transform :config :title :description :background
                                    :padding :usermeta :autosize)
                         when (plist-member spec k) append (list k (plist-get spec k)))
                (list (if (eq key :row) :vconcat :hconcat)
                      (vconcat (seq-map-indexed (lambda (v i) (eas-facet--cell spec key v i domains)) values))))))))

;;; Headers

(defun eas-facet-header-extent (header metrics)
  "Space HEADER needs beside its plot: (SIDE . PIXELS)."
  (cond
   ((eas-layout-text-p metrics)
    (cons :top (plist-get metrics :label-size)))
   ;; A horizontal row label takes its widest text.
   ((and (equal (plist-get header :orient) "left") (eql (plist-get header :angle) 0))
    (cons :left (+ eas-facet-header-padding
                   (apply #'max 0 (mapcar (lambda (l) (eas-layout-text-width metrics l (plist-get header :fontSize)))
                                          (append (plist-get header :labels) nil))))))
   (t (cons (if (equal (plist-get header :orient) "top") :top :left)
            (+ eas-facet-header-padding eas-facet-header-size 1)))))

(defun eas-facet-header-place (header bounds inset metrics)
  "HEADER placed beside plot BOUNDS [X Y W H], outside INSET pixels of axes.
Return (:text :x :y :angle :align :baseline :fontSize)."
  (let* ((top (equal (plist-get header :orient) "top"))
         (x0 (- (aref bounds 0) (if top 0 inset))) (y0 (- (aref bounds 1) (if top inset 0)))
         (w (aref bounds 2)) (h (aref bounds 3))
         (text (plist-get header :text)))
    (cond
     ((eas-layout-text-p metrics)
      (list :text text :x x0 :y (- y0 (plist-get metrics :label-size)) :angle 0 :align "left" :baseline "top"
            :fontSize (plist-get metrics :label-size)))
     ((equal (plist-get header :orient) "top")
      (list :text text :x (+ x0 (/ w 2.0)) :y (- y0 eas-facet-header-padding) :angle 0
            :align "center" :baseline "bottom" :fontSize eas-facet-header-size))
     ((eql (plist-get header :angle) 0)
      (list :text text :x (- x0 eas-facet-header-padding) :y (+ y0 (/ h 2.0)) :angle 0
            :align "right" :baseline "middle" :fontSize eas-facet-header-size))
     (t (list :text text :x (- x0 eas-facet-header-padding) :y (+ y0 (/ h 2.0)) :angle -90
              :align "center" :baseline "bottom" :fontSize eas-facet-header-size)))))

(provide 'eas-facet)
;;; eas-facet.el ends here
