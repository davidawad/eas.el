;;; eas-marks-series.el --- per-unit lookups the series and stack builders share -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Line and area marks split rows into series, and stacked
;; marks order rows by their stack-by fields' domains.  Both used to
;; look the defs and domain positions up again for every row (and, in
;; the stack sort, for every comparison); these helpers do it once per
;; unit (fc-qx1.42, measured in test/vl-examples/area-circular/bench.json).

;;; Code:

(require 'eas-core)
(require 'eas-encode)

(defconst eas-marks--series-channels
  '(:color :fill :stroke :strokeDash :detail :opacity :fillOpacity :strokeOpacity :strokeWidth)
  "Channels whose field splits a line, area or trail into series.")

(defun eas-marks--series-defs (unit)
  "Return the field defs splitting a line/area/trail UNIT into series.
There is one per channel (nil where the channel does not split).  Vega-Lite's
pathGroupingFields: any unaggregated field on these channels splits paths,
whatever its type; size does too, except for trails (fc-qx1.41)."
  (let ((enc (plist-get unit :encoding)))
    (mapcar (lambda (ch) (let ((d (eas-encode-data-def (plist-get enc ch))))
                           (and d (eas-object-p d)
                                (or (eas-encode-discrete-p d)
                                    (not (or (plist-get d :aggregate) (equal (plist-get d :derived) "aggregate"))))
                                d)))
            (if (equal (plist-get (plist-get unit :mark) :type) "trail") eas-marks--series-channels
              (cons :size eas-marks--series-channels)))))

(defun eas-marks--series-key (unit row &optional defs)
  "The series a ROW of a line/area UNIT belongs to.
DEFS is UNIT's `eas-marks--series-defs', when already computed."
  (cl-loop for d in (or defs (eas-marks--series-defs unit)) collect (and d (eas-encode-raw d row))))

(defun eas-marks--stack-ranks (rows by domains)
  "Vector of each of ROWS' positions in DOMAINS (lists), one per def in BY."
  (let ((index (mapcar (lambda (dom)
                         (let ((h (make-hash-table :test 'equal)) (k 0))
                           (dolist (v dom) (unless (gethash v h) (puthash v k h)) (setq k (1+ k)))
                           h))
                       domains)))
    (vconcat (seq-map (lambda (row) (cl-loop for d in by for h in index collect (gethash (eas-encode-raw d row) h 0)))
                      rows))))

(provide 'eas-marks-series)
;;; eas-marks-series.el ends here
