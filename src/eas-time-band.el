;;; eas-time-band.el --- bars that span their time unit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A bar (or rect) whose x or y is a timeUnit of a temporal
;; field on a time scale spans the whole unit in Vega-Lite: from the
;; unit's start to the next unit's start, inset half a pixel on each side
;; (binSpacing), and the scale's domain covers those ends.  The
;; channel's bandPosition (default 0.5) slides the span: 0 centres it on
;; the unit's start, as start + (b - 0.5) * (start - previous) to
;; end + (b - 0.5) * (end - start).

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-transform)

(defun eas-time-band-unit (def)
  "The timeUnit behind derived DEF (field UNIT_SOURCE), or nil."
  (and (eas-object-p def) (equal (plist-get def :derived) "timeUnit")
       (car (split-string (plist-get def :field) "_"))))

(defun eas-time-band-p (unit def)
  "Non-nil when DEF in UNIT draws bars spanning their time unit."
  (and (member (plist-get (plist-get unit :mark) :type) '("bar" "rect"))
       (equal (plist-get def :type) "temporal")
       (not (member (plist-get (plist-get def :scale) :type) '("band" "point")))
       (eas-time-band-unit def)))

(defun eas-time-band-step (unit ms n)
  "Return MS moved by N times the step of time UNIT's finest component."
  (let* ((parts (eas-time-unit-components unit))
         (eas-time-zone (unless (string-prefix-p "utc" unit) eas-time-zone))
         (f (eas-time-fields ms))
         (finest (car (last (seq-filter (lambda (p) (member p parts)) eas-time-unit-parts))))
         (add (lambda (part k) (if (equal finest part) (* n k) 0))))
    (eas-time-ms (+ (plist-get f :year) (funcall add "year" 1))
                 (+ (plist-get f :month) (funcall add "month" 1) (funcall add "quarter" 3))
                 (+ (plist-get f :day) (funcall add "date" 1) (funcall add "day" 1))
                 (+ (plist-get f :hours) (funcall add "hours" 1))
                 (+ (plist-get f :minutes) (funcall add "minutes" 1))
                 (+ (plist-get f :seconds) (funcall add "seconds" 1))
                 (+ (plist-get f :milliseconds) (funcall add "milliseconds" 1)))))

(defun eas-time-band-span (def value)
  "(START . END) in epoch ms of the span derived DEF's bar covers at VALUE."
  (when-let* ((ms (eas-time-parse value)))
    (let* ((unit (eas-time-band-unit def))
           (end (eas-time-band-step unit ms 1))
           (b (- (or (plist-get def :bandPosition) 0.5) 0.5)))
      (if (zerop b) (cons ms end)
        (cons (+ ms (* b (- ms (eas-time-band-step unit ms -1)))) (+ end (* b (- end ms))))))))

(defun eas-time-band-values (unit def rows)
  "Domain values DEF's spanning bars in UNIT add over ROWS."
  (when (eas-time-band-p unit def)
    (let ((key (eas-key (plist-get def :field))) out)
      (seq-doseq (row rows)
        (when-let* ((span (eas-time-band-span def (plist-get row key))))
          (push (car span) out) (push (cdr span) out)))
      out)))

(provide 'eas-time-band)
;;; eas-time-band.el ends here
