;;; eas-facet-title.el --- a facet's field title over its headers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, with eas-facet.el.  Vega-Lite titles a row or column
;; facet with its field's title ("Site") beyond the header labels: a
;; guide-title (11px bold) whose bottom sits titlePadding (10) beyond
;; the trellis columnTitle offset (10) from the labels, centred over the
;; cells' plots (Vega's title band 0.5 over flush bounds); a row
;; facet's title is turned -90 left of its labels.  header.title
;; replaces the channel's title and null hides it.
;;
;; `eas-facet-title-header' adds the title to the cells' header plists
;; (:title on the first cell, :title-band on all so their plots stay
;; level); `eas-facet-title-band' is the room it takes beyond the
;; labels; `eas-facet-title-add' draws it once the cells are placed, as
;; a non-interactive text mark of the first cell.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-encode)
(require 'eas-theme)
(require 'eas-scale)
(require 'eas-layout-axis-style)

(defconst eas-facet-title-offset 10 "Vega-Lite's trellis columnTitle/rowTitle offset.")

(defun eas-facet-title--style (def config)
  "Title look of facet channel DEF under CONFIG: (:size :weight :color :pad)."
  (let ((get (lambda (key default)
               (let ((h (and (eas-object-p (plist-get def :header)) (plist-get (plist-get def :header) key))))
                 (cond ((and h (not (eq h :null))) h)
                       ((eas-theme-get config :header key))
                       (t default))))))
    (list :size (funcall get :titleFontSize 11) :weight (funcall get :titleFontWeight "bold")
          :color (funcall get :titleColor "black") :pad (funcall get :titlePadding 10))))

(defun eas-facet-title-text (def)
  "The facet field title for channel DEF, or nil when hidden."
  (let ((header (plist-get def :header)))
    (if (and (eas-object-p header) (plist-member header :title))
        (let ((tt (plist-get header :title))) (and (stringp tt) (not (string-empty-p tt)) tt))
      (eas-encode-title def))))

(defun eas-facet-title--label (header def config)
  "HEADER with facet DEF's label properties (header, else CONFIG's header).
labelColor labelFont labelFontStyle labelFontWeight labelFontSize
labelPadding style the label; format, labelExpr and labelLimit rewrite
it; labels false hides it."
  (let* ((own (and (eas-object-p (plist-get def :header)) (plist-get def :header)))
         (get (lambda (key) (let ((v (if (plist-member own key) (plist-get own key) (eas-theme-get config :header key))))
                              (unless (eq v :null) v))))
         (text (plist-get header :text))
         (value (plist-get header :value))
         (fmt (funcall get :format))
         (text (if (and (stringp fmt) (numberp value))
                   (funcall (eas-scale-tick-format (list :type "linear" :domain [0 1]) 5 fmt) value)
                 text))
         (text (eas-layout-axis-style-label (list :labelExpr (funcall get :labelExpr)) value text))
         (limit (funcall get :labelLimit)))
    (append (list :text (cond ((eq (funcall get :labels) :false) "")
                              ((and (numberp limit) (> limit 0))
                               (eas-layout-truncate (eas-layout-metrics 'svg nil config) text
                                                    (or (funcall get :labelFontSize) 10) limit))
                              (t text)))
            (cl-loop for (key . prop) in '((:labelColor . :color) (:labelFont . :font) (:labelFontStyle . :fontStyle)
                                           (:labelFontWeight . :fontWeight) (:labelFontSize . :fontSize)
                                           (:labelPadding . :padding))
                     for v = (funcall get key)
                     when (and v (not (plist-member header prop))) append (list prop v))
            (eas--plist-without header :text))))

(defun eas-facet-title-header (header def i id config)
  "Cell I's HEADER plist with the title of facet DEF (facet ID) under CONFIG.
HEADER's :value is the cell's facet value."
  (let ((header (eas-facet-title--label header def config)))
    (if-let* ((text (eas-facet-title-text def)))
        (append header (list :facet id :title-band (eas-facet-title--style def config))
                (when (= i 0) (list :title text)))
      header)))

(defun eas-facet-title-band (header metrics)
  "Room HEADER's facet title takes beyond its labels under METRICS (0 if none)."
  (let ((style (plist-get header :title-band)))
    (cond ((null style) 0)
          ((eas-layout-text-p metrics) (plist-get metrics :label-size))
          ;; The labels' own extent already rounds a pixel up.
          (t (+ eas-facet-title-offset (plist-get style :pad) (plist-get style :size) -1)))))

(defun eas-facet-title--mark (view header cells metrics)
  "Title mark of facet HEADER drawn by VIEW, spanning CELLS (placed views)."
  (let* ((text (eas-layout-text-p metrics))
         (style (plist-get header :title-band))
         (top (equal (plist-get header :orient) "top"))
         (boxes (mapcar (lambda (v) (plist-get v :bounds)) cells))
         (lo (apply #'min (mapcar (lambda (b) (aref b (if top 0 1))) boxes)))
         (hi (apply #'max (mapcar (lambda (b) (+ (aref b (if top 0 1)) (aref b (if top 2 3)))) boxes)))
         (labels (plist-get view :header))
         (lsize (or (plist-get labels :fontSize) 10))
         (gap (+ eas-facet-title-offset (plist-get style :pad))))
    (list :id (format "%s/facet-title" (plist-get view :id)) :mark "text" :interactive-off t :rows []
          :items (vector
                  (append
                   (list :text (plist-get header :title) :fontSize (if text lsize (plist-get style :size))
                         :fontWeight (plist-get style :weight) :fill (plist-get style :color) :opacity 1)
                   (cond
                    (text (list :x (/ (+ lo hi) 2.0) :y (- (plist-get labels :y) (plist-get metrics :label-size))
                                :align "center" :baseline "top"))
                    (top (list :x (/ (+ lo hi) 2.0) :y (- (plist-get labels :y) lsize gap)
                               :align "center" :baseline "bottom"))
                    ;; A row title, turned, beyond the widest (or turned) label.
                    (t (let ((lx (- (plist-get labels :x)
                                    (if (eql (plist-get labels :angle) 0)
                                        (apply #'max 0 (mapcar (lambda (v) (eas-layout-text-width
                                                                            metrics (plist-get (plist-get v :header) :text) lsize))
                                                               cells))
                                      lsize))))
                         (list :x (- lx gap) :y (/ (+ lo hi) 2.0) :angle -90 :align "center" :baseline "bottom")))))))))

(defun eas-facet-title-add (views groups metrics)
  "VIEWS (of GROUPS, in order) with each facet's title drawn by its first cell."
  (if (not (seq-some (lambda (g) (plist-get (plist-get g :header) :title)) groups)) views
    (let ((pairs (cl-mapcar #'cons views groups)))
      (mapcar (lambda (p)
                (let ((h (plist-get (cdr p) :header)))
                  (if (not (plist-get h :title)) (car p)
                    (let ((cells (mapcar #'car (seq-filter (lambda (o) (equal (plist-get (plist-get (cdr o) :header) :facet)
                                                                              (plist-get h :facet)))
                                                           pairs))))
                      (plist-put (copy-sequence (car p)) :marks
                                 (vconcat (plist-get (car p) :marks)
                                          (list (eas-facet-title--mark (car p) h cells metrics))))))))
              pairs))))

(provide 'eas-facet-title)
;;; eas-facet-title.el ends here
