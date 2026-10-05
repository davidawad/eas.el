;;; eas-time.el --- dates as UTC epoch milliseconds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Temporal values are epoch milliseconds.  Calendar fields, formats
;; and zone-less date-time strings follow `eas-time-zone', which is
;; nil (UTC) by default so output is identical on every machine.  Bound
;; to a zone it gives Vega's local-time semantics in that zone: a
;; date-time without an offset is local, a date-only string is UTC
;; midnight (as JavaScript parses it), and fields, timeUnits, "time"
;; scale ticks and labels are local.  The conformance oracle binds it to
;; the zone bin/chart's references were built in.

;;; Code:

(require 'eas-core)
(require 'eas-time-offset)

(defvar eas-time-zone nil
  "Zone for local time, or nil for UTC.
A `decode-time' ZONE such as \"America/Chicago\".")

(defconst eas-time--iso-regexp
  (concat "\\`\\([0-9]\\{4\\}\\)\\(?:[-/]\\([0-9]\\{1,2\\}\\)"
          "\\(?:[-/]\\([0-9]\\{1,2\\}\\)"
          "\\(?:[T ]\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)"
          "\\(?::\\([0-9]\\{2\\}\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
          "\\(Z\\|[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)?\\)?\\)?\\)?\\'")
  "ISO-8601-ish date or date-time: YYYY[-MM[-DD[THH:MM[:SS[.fff]][zone]]]].")

(defun eas-time-days-from-civil (year month day)
  "Days since 1970-01-01 for the proleptic Gregorian YEAR MONTH DAY."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (mp (mod (+ month 9) 12))
         (doy (+ (/ (+ (* 153 mp) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun eas-time-civil-from-days (days)
  "Return (YEAR MONTH DAY) for DAYS since 1970-01-01."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- doe (/ doe 1460) (- (/ doe 36524)) (/ doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (/ (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9)))
         (year (+ yoe (* era 400) (if (<= month 2) 1 0))))
    (list year month day)))

(defconst eas-time--js-regexp
  (concat "\\`\\([A-Za-z]\\{3\\}\\)[a-z]*\\.? +\\([0-9]\\{1,2\\}\\),? +\\([0-9]\\{4\\}\\)"
          "\\(?: +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?::\\([0-9]\\{2\\}\\)\\)?\\)?\\'")
  "A date as JavaScript's Date.parse reads it outside ISO: \"Jan 1 2000\".")

(defconst eas-time--months
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
  "Month abbreviations of `eas-time--js-regexp' dates.")

(defun eas-time-string-p (value)
  "Non-nil when VALUE is a string `eas-time-parse' reads as a date."
  (and (stringp value)
       (or (and (string-match-p eas-time--iso-regexp value) (> (length value) 4))
           (save-match-data
             (and (string-match eas-time--js-regexp value)
                  (member (downcase (match-string 1 value)) eas-time--months)
                  t)))))

(defvar eas-time--parse-cache (make-hash-table :test 'equal)
  "Date string -> epoch ms (or :none).  A crosshair tests every row's
date against the selection on each move (fc-qx1.2), so parse once.")

(defconst eas-time--parse-cache-limit 200000
  "Entries kept before `eas-time--parse-cache' is emptied.")

(defun eas-time-parse (value)
  "Return VALUE as UTC epoch milliseconds, or nil when it is not a date.
Numbers are already epoch milliseconds."
  (cond
   ((numberp value) value)
   ((and (consp value) (keywordp (car value))) (eas-time-datetime value))
   ((stringp value)
    (let* ((key (if eas-time-zone (cons eas-time-zone value) value))
           (hit (gethash key eas-time--parse-cache)))
      (if hit (and (not (eq hit :none)) hit)
        (when (>= (hash-table-count eas-time--parse-cache) eas-time--parse-cache-limit)
          (clrhash eas-time--parse-cache))
        (let ((ms (eas-time--parse-string value)))
          (puthash key (or ms :none) eas-time--parse-cache)
          ms))))))

(defun eas-time-datetime (dt)
  "Epoch ms of Vega-Lite DateTime object DT, a plist (:year :month :date ...).
Absent parts take Vega-Lite's defaults: year 2012, January, date 1.
Months may be numbers (1-12) or names; utc true reads it in UTC."
  (let* ((month (plist-get dt :month))
         (month (cond ((numberp month) month)
                      ((stringp month)
                       (1+ (or (seq-position '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
                                             (downcase (substring month 0 (min 3 (length month)))))
                               0)))
                      ((plist-get dt :quarter) (1+ (* 3 (1- (plist-get dt :quarter)))))
                      (t 1)))
         (eas-time-zone (unless (eq (plist-get dt :utc) t) eas-time-zone)))
    (eas-time-ms (or (plist-get dt :year) 2012) month (or (plist-get dt :date) 1)
                 (or (plist-get dt :hours) 0) (or (plist-get dt :minutes) 0)
                 (or (plist-get dt :seconds) 0) (or (plist-get dt :milliseconds) 0))))

(defun eas-time--parse-string (value)
  "Parse string VALUE as `eas-time-parse' does, uncached."
  (cond
   ;; Non-ISO dates are local time in JavaScript.
   ((and (string-match eas-time--js-regexp value)
         (member (downcase (match-string 1 value)) eas-time--months))
    (let ((num (lambda (n) (if (match-string n value) (string-to-number (match-string n value)) 0))))
      (eas-time-ms (funcall num 3) (1+ (seq-position eas-time--months (downcase (match-string 1 value))))
                   (funcall num 2) (funcall num 4) (funcall num 5) (funcall num 6))))
   ((string-match eas-time--iso-regexp value)
    (let* ((num (lambda (n default)
                  (if (match-string n value) (string-to-number (match-string n value)) default)))
           (year (funcall num 1 1970)) (month (funcall num 2 1)) (day (funcall num 3 1))
           (hour (funcall num 4 0)) (minute (funcall num 5 0)) (sec (funcall num 6 0))
           (frac (match-string 7 value))
           (zone (match-string 8 value))
           (ms (if frac (round (* 1000 (string-to-number (concat "0." frac)))) 0))
           (local (and eas-time-zone (match-string 4 value) (null zone)))
           (offset (if (and zone (not (equal zone "Z")))
                       (let ((sign (if (eq (aref zone 0) ?-) -1 1))
                             (digits (replace-regexp-in-string ":" "" (substring zone 1))))
                         (* sign (+ (* 60 (string-to-number (substring digits 0 2)))
                                    (string-to-number (substring digits 2)))))
                     0)))
      (if local (eas-time-ms year month day hour minute sec ms)
        (+ (* 1000 (+ (* 86400 (eas-time-days-from-civil year month day))
                      (* 3600 hour) (* 60 (- minute offset)) sec))
           ms))))))

;; Converting in a named zone sets TZ for each call, which dominates
;; time units over thousands of rows.  A zone's UTC offset is cached per
;; 15-minute bucket when it is the same at both ends of the bucket (every
;; transition then lies outside it); local fields then follow from UTC
;; arithmetic.  Buckets holding a transition convert exactly.

(defvar eas-time--offsets (make-hash-table :test 'equal)
  "(ZONE . BUCKET) -> the zone's UTC offset in seconds throughout the
15-minute BUCKET, or `mixed' when it changes inside it.")

(defvar eas-time--encoded (make-hash-table :test 'equal)
  "(ZONE YEAR MONTH DAY HOURS MINUTES SECONDS) -> epoch seconds.")

(defun eas-time--cache-put (table key value)
  "Store VALUE under KEY in TABLE, emptied first when large; return VALUE."
  (when (> (hash-table-count table) 100000) (clrhash table))
  (puthash key value table))

(defun eas-time--offset (s)
  "`eas-time-zone''s UTC offset in seconds at epoch S, or nil when its
15-minute bucket holds a transition."
  (let* ((bucket (floor s 900)) (key (cons eas-time-zone bucket))
         (hit (gethash key eas-time--offsets)))
    (unless hit
      (let ((a (decoded-time-zone (decode-time (* bucket 900) eas-time-zone)))
            (b (decoded-time-zone (decode-time (+ (* bucket 900) 899) eas-time-zone))))
        (setq hit (eas-time--cache-put eas-time--offsets key (if (and (integerp a) (eql a b)) a 'mixed)))))
    (and (integerp hit) hit)))

(defun eas-time-fields (ms)
  "Return the calendar fields of epoch MS in `eas-time-zone' as a plist.
Keys are :year :month (1-12) :day :hours :minutes :seconds
:milliseconds and :weekday (0 is Sunday)."
  (let ((off (and (stringp eas-time-zone) (eas-time-offset-week ms eas-time-zone))))
    (cond
     ;; Outside transition weeks, UTC arithmetic at the zone's offset.
     (off (eas-time--utc-fields (+ (floor ms) off)))
     (eas-time-zone
      (let* ((ms (floor ms)) (s (floor ms 1000)) (d (decode-time s eas-time-zone)))
        (list :year (decoded-time-year d) :month (decoded-time-month d) :day (decoded-time-day d)
              :hours (decoded-time-hour d) :minutes (decoded-time-minute d) :seconds (decoded-time-second d)
              :milliseconds (- ms (* 1000 s)) :weekday (decoded-time-weekday d))))
     (t (eas-time--utc-fields ms)))))

(defun eas-time--utc-fields (ms)
  "The UTC calendar fields of epoch MS (see `eas-time-fields')."
  (let* ((ms (floor ms))
         (days (floor ms 86400000))
         (rem (- ms (* days 86400000)))
         (civil (eas-time-civil-from-days days)))
    (list :year (nth 0 civil) :month (nth 1 civil) :day (nth 2 civil)
          :hours (/ rem 3600000) :minutes (% (/ rem 60000) 60)
          :seconds (% (/ rem 1000) 60) :milliseconds (% rem 1000)
          :weekday (mod (+ 4 days) 7))))

(defun eas-time-ms (year &optional month day hours minutes seconds milliseconds)
  "Return epoch milliseconds for calendar fields in `eas-time-zone'.
MONTH (1-12), DAY and the clock fields may overflow; they are normalized."
  (let* ((local (and eas-time-zone (eas-time--utc-ms year month day hours minutes seconds milliseconds)))
         (guess (and (stringp eas-time-zone) (eas-time-offset-week local eas-time-zone)))
         (off (and guess (eas-time-offset-week (- local guess) eas-time-zone))))
    (cond
     ;; Outside transition weeks the local time names one instant.
     ((and off (eql off guess)) (- local off))
     (eas-time-zone
      (+ (* 1000 (time-convert (encode-time (list (or seconds 0) (or minutes 0) (or hours 0) (or day 1)
                                                  (or month 1) year nil -1 eas-time-zone))
                               'integer))
         (or milliseconds 0)))
     (t (eas-time--utc-ms year month day hours minutes seconds milliseconds)))))

(defun eas-time--utc-ms (year &optional month day hours minutes seconds milliseconds)
  "UTC epoch milliseconds for calendar fields YEAR MONTH DAY HOURS
MINUTES SECONDS MILLISECONDS (see `eas-time-ms')."
  (let* ((month (or month 1))
         (y (+ year (floor (1- month) 12)))
         (m (1+ (mod (1- month) 12))))
    (+ (* 86400000 (+ (eas-time-days-from-civil y m 1) (1- (or day 1))))
       (* 3600000 (or hours 0)) (* 60000 (or minutes 0))
       (* 1000 (or seconds 0)) (or milliseconds 0))))

(defun eas-time-format (ms format-string)
  "Format epoch MS in `eas-time-zone' with `format-time-string' FORMAT-STRING."
  (format-time-string format-string (seconds-to-time (/ ms 1000.0)) (or eas-time-zone t)))

(defun eas-time-iso (ms)
  "Return MS as an ISO date (YYYY-MM-DD) or date-time string."
  (if (zerop (mod (floor ms) 86400000))
      (eas-time-format ms "%Y-%m-%d")
    (eas-time-format ms "%Y-%m-%dT%H:%M:%SZ")))

(provide 'eas-time)
;;; eas-time.el ends here
