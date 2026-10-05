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

(defun eas-marks--series-defs (unit)
  "The discrete field defs splitting a line/area UNIT into series, one per
channel (nil where the channel does not split)."
  (let ((enc (plist-get unit :encoding)))
    (mapcar (lambda (ch) (let ((d (eas-encode-data-def (plist-get enc ch))))
                           (and d (eas-object-p d) (eas-encode-discrete-p d) d)))
            '(:color :fill :stroke :strokeDash :detail))))

(defun eas-marks--series-key (unit row &optional defs)
  "The series a ROW of a line/area UNIT belongs to.
DEFS is UNIT's `eas-marks--series-defs', when already computed."
  (mapcar (lambda (d) (and d (eas-encode-raw d row))) (or defs (eas-marks--series-defs unit))))

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
