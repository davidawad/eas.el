;;; eas-scale-time.el --- UTC time ticks and Vega's multi-format labels -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A port of d3-time's tick interval choice and vega-format's
;; timeMultiFormat, so time axes tick where Vega's do: in UTC by
;; default, in `eas-time-zone' when it is bound (Vega's local time).
;; Weekday and month names are English (C locale), as in Vega.

;;; Code:

(require 'eas-time)

(defconst eas-scale-time--intervals
  `((second 1 1000) (second 5 5000) (second 15 15000) (second 30 30000)
    (minute 1 60000) (minute 5 300000) (minute 15 900000) (minute 30 1800000)
    (hour 1 3600000) (hour 3 10800000) (hour 6 21600000) (hour 12 43200000)
    (day 1 86400000) (day 2 172800000)
    (week 1 604800000)
    (month 1 2592000000) (month 3 7776000000)
    (year 1 31536000000))
  "The d3-time tickIntervals: (UNIT STEP APPROX-DURATION-MS).")

(defun eas-scale-time--floor (unit ms)
  "Floor epoch MS to the start of UNIT (in `eas-time-zone')."
  (let* ((f (eas-time-fields ms))
         (day (lambda (d) (eas-time-ms (plist-get f :year) (plist-get f :month) d))))
    (pcase unit
      ('millisecond ms)
      ('second (* 1000 (floor ms 1000)))
      ('minute (* 60000 (floor ms 60000)))
      ('hour (eas-time-ms (plist-get f :year) (plist-get f :month) (plist-get f :day) (plist-get f :hours)))
      ('day (funcall day (plist-get f :day)))
      ('week (funcall day (- (plist-get f :day) (plist-get f :weekday))))
      ('month (eas-time-ms (plist-get f :year) (plist-get f :month) 1))
      ('year (eas-time-ms (plist-get f :year) 1 1)))))

(defun eas-scale-time--offset (unit ms)
  "Advance MS by one UNIT (calendar units in `eas-time-zone')."
  (let* ((f (eas-time-fields ms))
         (at (lambda (&rest delta)
               (eas-time-ms (+ (plist-get f :year) (or (plist-get delta :year) 0))
                              (+ (plist-get f :month) (or (plist-get delta :month) 0))
                              (+ (plist-get f :day) (or (plist-get delta :day) 0))
                              (plist-get f :hours) (plist-get f :minutes) (plist-get f :seconds)
                              (plist-get f :milliseconds)))))
    (pcase unit
      ('millisecond (1+ ms)) ('second (+ ms 1000)) ('minute (+ ms 60000))
      ('hour (+ ms 3600000)) ('day (funcall at :day 1)) ('week (funcall at :day 7))
      ('month (funcall at :month 1))
      ('year (funcall at :year 1)))))

(defun eas-scale-time--field (unit ms)
  "The field d3's interval.every(step) filters on for UNIT at MS."
  (let ((f (eas-time-fields ms)))
    (pcase unit
      ('millisecond ms) ('second (plist-get f :seconds)) ('minute (plist-get f :minutes))
      ('hour (plist-get f :hours)) ('day (1- (plist-get f :day))) ('week 0)
      ('month (1- (plist-get f :month))) ('year (plist-get f :year)))))

(defun eas-scale-time--interval (start stop count)
  "Return (UNIT . STEP), d3's tickInterval for START..STOP and COUNT."
  (let* ((target (/ (abs (- stop start)) (float count)))
         (i (or (seq-position eas-scale-time--intervals target
                              (lambda (iv tg) (> (nth 2 iv) tg)))
                (length eas-scale-time--intervals))))
    (cond
     ((= i (length eas-scale-time--intervals))
      (cons 'year (max 1 (round (eas-scale-tick-increment-years start stop count)))))
     ((= i 0) (cons 'millisecond (max 1 (round (/ (- stop start) (float count))))))
     (t (let* ((prev (nth (1- i) eas-scale-time--intervals))
               (next (nth i eas-scale-time--intervals))
               (pick (if (< (/ target (nth 2 prev)) (/ (nth 2 next) target)) prev next)))
          (cons (nth 0 pick) (nth 1 pick)))))))

(declare-function eas-scale-tick-increment "eas-scale" (start stop count))

(defun eas-scale-tick-increment-years (start stop count)
  "Return d3.tickStep for COUNT ticks over START..STOP measured in years."
  (let ((inc (eas-scale-tick-increment (/ start 31536000000.0) (/ stop 31536000000.0) count)))
    (if (< inc 0) (/ 1.0 (- inc)) inc)))

(defvar eas-scale-time--memo (make-hash-table :test 'equal)
  "Memo of the pure tick functions below: (FN ARGS ZONE) -> result.
Layout asks for a time axis's ticks and labels again for every tick
count it tries.")

(defun eas-scale-time--memo (fn args)
  "FN applied to ARGS, remembered per `eas-time-zone'."
  (let* ((key (list fn args eas-time-zone)) (hit (gethash key eas-scale-time--memo 'none)))
    (if (not (eq hit 'none)) (copy-sequence hit)
      (when (>= (hash-table-count eas-scale-time--memo) 4096) (clrhash eas-scale-time--memo))
      (let ((v (apply fn args))) (puthash key (copy-sequence v) eas-scale-time--memo) v))))

(defun eas-scale-time-ticks (start stop count)
  "Return tick times (epoch ms) between START and STOP, about COUNT."
  (eas-scale-time--memo #'eas-scale-time--ticks (list start stop count)))

(defun eas-scale-time--ticks (start stop count)
  "`eas-scale-time-ticks' of START STOP COUNT, uncached."
  (let* ((reverse (< stop start))
         (lo (min start stop)) (hi (max start stop))
         (interval (eas-scale-time--interval lo hi (max 1 count)))
         (unit (car interval)) (step (cdr interval))
         (tick (eas-scale-time--floor unit lo))
         ticks)
    (when (< tick lo) (setq tick (eas-scale-time--offset unit tick)))
    (while (<= tick hi)
      (when (zerop (mod (eas-scale-time--field unit tick) step))
        (push tick ticks))
      (setq tick (eas-scale-time--offset unit tick)))
    (if reverse ticks (nreverse ticks))))

(defun eas-scale-time-multi-format (ms)
  "Format epoch MS the way Vega's default time axis does."
  (eas-scale-time--memo #'eas-scale-time--multi-format (list ms)))

(defun eas-scale-time--multi-format (ms)
  "`eas-scale-time-multi-format' of MS, uncached."
  (let* ((f (eas-time-fields ms))
         (system-time-locale "C")
         (fmt (cond
               ((> (plist-get f :milliseconds) 0) ".%3N")
               ((> (plist-get f :seconds) 0) ":%S")
               ((> (plist-get f :minutes) 0) "%I:%M")
               ((> (plist-get f :hours) 0) "%I %p")
               ((> (plist-get f :day) 1) (if (zerop (plist-get f :weekday)) "%b %d" "%a %d"))
               ((> (plist-get f :month) 1) "%B")
               (t "%Y"))))
    (eas-time-format ms fmt)))

(provide 'eas-scale-time)
;;; eas-scale-time.el ends here
