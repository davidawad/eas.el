;;; hidden-views.el --- cost of live views nobody sees -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; dot-77x0xh.  N hidden views (play clocks and ladder streams) run for
;; SECONDS of real time while pushes arrive, window hooks storm and plays
;; are restarted.  Prints one RESULT line: ticks, compiles, timers, cpu.
;;
;;   emacs -Q --batch -L src -L test/eas -l bench/hidden-views.el
;;   (EAS_BENCH_N=9 EAS_BENCH_SECONDS=60; use another checkout's -L src
;;   for a baseline; neomacs works the same.)

;;; Code:

(require 'cl-lib)
(require 'eas-live)
(require 'eas-stream)
(require 'eas-play)
(require 'eas-view)
(require 'eas-template)
(require 'eas-live-test)

(let* ((n (string-to-number (or (getenv "EAS_BENCH_N") "9")))
       (secs (string-to-number (or (getenv "EAS_BENCH_SECONDS") "60")))
       (eas-live-visible-function (lambda (_) nil))
       (eas-play-visible-function (lambda (_) nil))
       (compiles 0) (max-timers 0) (pushes 0)
       (count (lambda (&rest _) (cl-incf compiles)))
       (views nil) (plays nil) (streams nil))
  (advice-add 'eas-view--compile :before count)
  (dotimes (i n)
    (if (cl-evenp i)
        (let ((v (eas-view-open "clock" :bindings (eas-template-example "clock")
                                :id (format "p%d" i))))
          (eas-play-attach v) (push v plays) (eas-play-start v))
      (let ((v (eas-view-open (eas-json-encode eas-live-test--ladder)
                              :id (format "s%d" i) :size '(400 . 300)
                              :rows (eas-live-test--book 1 25))))
        (push (eas-stream-attach v '(:max-fps 4 :window 50)) streams)
        (push v views))))
  (let ((cpu0 (get-internal-run-time)) (t0 (float-time)) (step 0))
    (while (< (- (float-time) t0) secs)
      (accept-process-output nil 0.05)
      (cl-incf step)
      (when (zerop (% step 10))         ; every ~0.5s
        (dolist (v views) (cl-incf pushes) (eas-push v (eas-live-test--book step 25))))
      (when (zerop (% step 20))         ; every ~1s: window hook storm
        (dotimes (_ 20) (eas-live--wake-soon)))
      (when (zerop (% step 100))        ; every ~5s: restart plays
        (dolist (v plays) (eas-play-start v)))
      (setq max-timers (max max-timers (+ (length timer-list) (length timer-idle-list)))))
    (let ((cpu (float-time (time-subtract (get-internal-run-time) cpu0))))
      (message "RESULT n=%d secs=%d pushes=%d ticks=%d skipped=%d frames=%d compiles=%d max-timers=%d end-timers=%d cpu=%.3f"
               n secs pushes
               (apply #'+ (mapcar (lambda (v) (eas-play-ticks (eas-play-get v))) plays))
               (apply #'+ (mapcar (lambda (v) (eas-play-skipped (eas-play-get v))) plays))
               (apply #'+ (mapcar #'eas-stream-frames streams))
               compiles max-timers
               (+ (length timer-list) (length timer-idle-list)) cpu))))

;;; hidden-views.el ends here
