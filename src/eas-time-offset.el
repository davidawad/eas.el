;;; eas-time-offset.el --- a zone's UTC offset, cached per week -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of eas-time.el (fc-qx1.43).  `decode-time' and `encode-time'
;; with a named zone look the zone up on every call, which made a
;; timeUnit over a few thousand rows cost a second.  Within a week that
;; holds no transition the zone's offset is one number, so calendar
;; arithmetic in UTC plus that offset is exact there.
;; `eas-time-offset-week' gives the offset of the week (epoch-aligned
;; 7-day span) holding an instant, from its first and last second, or
;; nil when they differ (a DST or other transition week: callers then
;; ask Emacs directly).  Offsets are cached per zone and week.

;;; Code:

(defconst eas-time-offset--week (* 7 86400000)
  "Milliseconds in one cache bucket.")

(defvar eas-time-offset--cache (make-hash-table :test 'equal)
  "(ZONE . WEEK) -> offset in ms, or nil across a transition.")

(defun eas-time-offset-week (ms zone)
  "UTC offset in milliseconds of ZONE (a string) around epoch MS, or nil
when the week holding MS has a transition."
  (let* ((week (floor ms eas-time-offset--week))
         (key (cons zone week))
         (hit (gethash key eas-time-offset--cache 'none)))
    (if (not (eq hit 'none)) hit
      (puthash key
               (let* ((start (floor (* week eas-time-offset--week) 1000))
                      (a (decoded-time-zone (decode-time start zone)))
                      (b (decoded-time-zone (decode-time (+ start (/ eas-time-offset--week 1000) -1) zone))))
                 (and (integerp a) (eql a b) (* 1000 a)))
               eas-time-offset--cache))))

(provide 'eas-time-offset)
;;; eas-time-offset.el ends here
