;;; eas-vl-gallery-bench.el --- the bench verb over a gallery group -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fc-qx1.42.  `eas-vl-gallery-bench' runs `bench SOURCE' (resolve,
;; compile and render for both backends, a pointermove sweep) on every
;; example of a group, with its data inlined and in the references'
;; zone, and `eas-vl-gallery-bench-write' records the means in
;; test/vl-examples/GROUP/bench.json:
;;
;;   {"emacs": V, "compiled": true, "zone": Z, "reps": N, "calibration_ms": C,
;;    "examples": {NAME: {"rows": R, "items": I, "ms": {STAGE: MEAN},
;;                        "baseline_ms": {STAGE: MEAN}}}}
;;
;; calibration_ms is `eas-bench-calibrate' on the measuring machine, so
;; numbers from two machines compare by their ratio.  baseline_ms, when
;; given, are the same stages measured before a change (the box's run
;; of the parent commit).  scripts/eas-gallery-bench.sh measures
;; byte-compiled, as an installed package runs.

;;; Code:

(require 'eas-core)
(require 'eas-vl-gallery)
(require 'eas-agent-health)
(require 'eas-bench)
(require 'eas-memo)

(defun eas-vl-gallery-bench-example (group name n)
  "Return bench data for example NAME of GROUP over N repetitions.
Stage :compile-cold is an SVG compile with every memo table emptied
first (`eas-memo-clear'), as the first compile of a session runs."
  (let* ((spec (eas-vl-gallery-spec group name))
         (cold (let ((eas-time-zone eas-vl-gallery-zone) (eas-spec-supported-function nil))
                 (eas-agent--time n (lambda () (eas-memo-clear) (eas-compile spec)))))
         (env (let ((eas-time-zone eas-vl-gallery-zone) (eas-spec-supported-function nil))
                (eas-agent-bench (list spec) (list :n n))))
         (data (plist-get env :data)) (ms (plist-get data :ms)))
    (list :rows (plist-get data :rows) :items (plist-get data :items)
          :ms (cons :compile-cold
                    (cons (plist-get cold :mean)
                          (cl-loop for (stage v) on ms by #'cddr
                                   unless (eq v :null) append (list stage (plist-get v :mean))))))))

(defun eas-vl-gallery-bench (group &optional n)
  "Bench every example of GROUP (N repetitions, default 10).
Return the bench.json object without baselines."
  (let ((n (or n 10)))
    (list :emacs emacs-version :compiled (if (eas-bench-compiled-p) t :false)
          :zone eas-vl-gallery-zone :reps n :calibration_ms (eas-bench-calibrate)
          :examples (cl-loop for name in (eas-vl-gallery-names group)
                             append (list (eas-key name) (eas-vl-gallery-bench-example group name n))))))

(defun eas-vl-gallery-bench-file (group)
  "GROUP's bench.json."
  (expand-file-name "bench.json" (eas-vl-gallery-group-directory group)))

(defun eas-vl-gallery-bench-write (group &optional n baseline)
  "Bench GROUP over N repetitions and write its bench.json.
BASELINE is an earlier result (a bench object, or a file holding one)
whose stage means become each example's baseline_ms.  Return the
object written."
  (let* ((result (eas-vl-gallery-bench group n))
         (base (if (stringp baseline) (eas-json-read-file baseline) baseline))
         (examples (plist-get result :examples)))
    (when base
      (setq examples
            (cl-loop for (k v) on examples by #'cddr
                     for b = (plist-get (plist-get (plist-get base :examples) k) :ms)
                     append (list k (if b (append v (list :baseline_ms b)) v))))
      (setq result (append (eas-plist-put result :examples examples)
                           (list :baseline_calibration_ms (plist-get base :calibration_ms)))))
    (with-temp-file (eas-vl-gallery-bench-file group)
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (eas-json-pretty result) "\n"))
    result))

(provide 'eas-vl-gallery-bench)
;;; eas-vl-gallery-bench.el ends here
