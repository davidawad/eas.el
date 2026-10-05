;;; eas-strip.el --- passive values strip: what the chart reads at the cursor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.34).  A chart shows its values without being
;; touched: under the plot, a one-line strip reads every series at the
;; pointer's column, or at its latest datum while the pointer is
;; elsewhere.  Nothing has to be hovered; hover (eas-intersect) is for
;; marks the pointer actually touches.
;;
;;   latest  date=Mar 11, 2026  value=109
;;   cursor  date=Mar 05, 2026  AAPL=106.3  MSFT=98.2
;;
;; The column is the pointer's x (y for horizontal charts, whose y is
;; discrete), from the view state's :pointer, which the reducer records
;; on every pointermove.  It applies to every view whose plot spans
;; that column, so a price panel and the volume panel under it read the
;; same date.  Per mark, each series (a line or area item) gives its
;; datum nearest the column; discrete marks give the items nearest it.
;; A field is labelled by the series (color, fill, stroke or detail
;; values), else by the value channel's title.  Pure: scene, plan and
;; state in, data out.

;;; Code:

(require 'eas-core)
(require 'eas-encode)
(require 'eas-hit)
(require 'eas-intersect)
(require 'eas-tip)

(defconst eas-strip-marks '("line" "area" "trail" "point" "circle" "square" "bar" "rect" "tick")
  "Marks whose data the strip reads; rules and text annotate.")

(defconst eas-strip-series-marks '("line" "area" "trail") "Marks drawn as one path per series.")

(defun eas-strip--key (encoding)
  "The channel the strip reads across: :y when only y is discrete, else :x."
  (let ((x (plist-get encoding :x)) (y (plist-get encoding :y)))
    (if (and (eas-object-p y) (eas-encode-discrete-p y)
             (not (and (eas-object-p x) (eas-encode-discrete-p x))))
        :y :x)))

(defun eas-strip--column (view key pointer)
  "Pixel along KEY where POINTER ([X Y]) crosses VIEW's plot, or nil."
  (when (vectorp pointer)
    (let* ((b (plist-get view :bounds)) (i (if (eq key :x) 0 1))
           (lo (aref b i)) (v (aref pointer i)))
      (and (<= lo v (+ lo (aref b (+ i 2)))) v))))

(defun eas-strip--label (encoding row value-def)
  "ROW's series under ENCODING, else VALUE-DEF's title."
  (let ((parts (delq nil (mapcar (lambda (ch)
                                   (let ((d (plist-get encoding ch)))
                                     (when (and (eas-object-p d) (eas-encode-discrete-p d))
                                       (let ((v (eas-encode-raw d row)))
                                         (unless (memq v '(nil :null)) (eas-encode-format-value d v))))))
                                 '(:color :fill :stroke :detail)))))
    (if parts (string-join (delete-dups parts) "/")
      (or (eas-encode-title value-def) (plist-get value-def :field) "value"))))

(defun eas-strip--pick (mark key col)
  "Datums of MARK the strip reads at pixel COL along KEY (nil: the latest).
A series gives its datum nearest COL; discrete marks give every item
whose position along KEY is nearest COL (all series at one x)."
  (let ((items (plist-get mark :items)))
    (if (member (plist-get mark :mark) eas-strip-series-marks)
        (cl-loop for item across items
                 for anchors = (eas-hit--anchors item)
                 for k = (and (> (length anchors) 0)
                              (if col (eas-intersect-nearest-x anchors col) (1- (length anchors))))
                 when k collect (eas-hit--datum item k))
      (let ((i (if (eq key :x) #'car #'cdr)) best picked)
        (cl-loop for item across items
                 unless (eas-intersect--hidden-p item)
                 do (let* ((p (funcall i (eas-hit--item-point item nil)))
                           (score (if col (- (abs (- p col))) p)))
                      (cond ((or (null best) (> score (+ best 0.5))) (setq best score picked (list item)))
                            ((>= score (- best 0.5)) (push item picked)))))
        (mapcar (lambda (item) (plist-get item :datum)) (nreverse picked))))))

(defun eas-strip--mark (plan mark col-fn)
  "Strip fields of MARK (compiled from PLAN): key field, then each series.
COL-FN maps the key channel to the column pixel or nil."
  (when-let* (((member (plist-get mark :mark) eas-strip-marks))
              ((not (plist-get mark :interactive-off)))
              ((> (length (plist-get mark :items)) 0))
              (unit (eas-tip--unit plan (plist-get mark :id)))
              (encoding (plist-get unit :encoding))
              (key (eas-strip--key encoding))
              (key-def (plist-get encoding key))
              (value-def (plist-get encoding (if (eq key :x) :y :x)))
              ((eas-object-p value-def)))
    (let* ((col (funcall col-fn key))
           (rows (mapcar (lambda (d) (aref (plist-get mark :rows) d))
                         (eas-strip--pick mark key col))))
      (when rows
        (cons col
              (append
               (when (eas-object-p key-def)
                 (list (list :title (or (eas-encode-title key-def) (plist-get key-def :field))
                             :value (eas-encode-format-value key-def (eas-encode-raw key-def (car rows))))))
               (mapcar (lambda (row)
                         (list :title (eas-strip--label encoding row value-def)
                               :value (eas-encode-format-value value-def (eas-encode-raw value-def row))))
                       rows)))))))

(defun eas-strip (scene plan state)
  "The values strip of SCENE (compiled from PLAN) under view STATE, or nil.
Returns (:at AT :fields [(:title T :value V) ...]); AT is \"cursor\"
when the fields are read at the pointer's column, else \"latest\"."
  (let ((pointer (plist-get state :pointer)) fields cursor)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (mark (plist-get view :marks))
        (when-let* ((got (eas-strip--mark plan mark (lambda (key) (eas-strip--column view key pointer)))))
          (when (car got) (setq cursor t))
          (dolist (f (cdr got)) (unless (member f fields) (push f fields))))))
    (when fields
      (list :at (if cursor "cursor" "latest") :fields (vconcat (nreverse fields))))))

(defun eas-strip-format (strip)
  "STRIP on one line: where it reads, then \"title=value\" fields, or \"\"."
  (if (null strip) ""
    (concat (plist-get strip :at) "  "
            (mapconcat (lambda (f) (format "%s=%s" (plist-get f :title) (plist-get f :value)))
                       (plist-get strip :fields) "  "))))

(provide 'eas-strip)
;;; eas-strip.el ends here
