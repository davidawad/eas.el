;;; geo-first-render.el --- a map template's first render in a fresh Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-gzi.  Run by geo-first-render.sh in a fresh `emacs -Q --batch'
;; on a compiled copy of src/.  Opens TEMPLATE (EAS_FR_TEMPLATE) as an
;; SVG view and prints its SVG once, at the default `gc-cons-threshold',
;; then prints one line: total ms (registry and bindings included),
;; render ms (open and print), the GC count and ms inside the render,
;; the trailing collection ms (`garbage-collect' right after: what a
;; session collects once idle), the backend and the SVG's md5.
;; EAS_FR_PROFILE=FILE writes a flat CPU profile of the render: per
;; function, its share of the samples inclusive and as the top frame.

;;; Code:

(require 'cl-lib)

(defun geo-first-render--profile (file)
  "Stop the CPU profiler and write its flat profile to FILE."
  (let ((log (profiler-cpu-log)) (self (make-hash-table :test 'equal))
        (incl (make-hash-table :test 'equal)) (all 0) rows)
    (profiler-stop)
    (maphash (lambda (bt n)
               (when (vectorp bt)
                 (setq all (+ all n))
                 (when (aref bt 0) (cl-incf (gethash (aref bt 0) self 0) n))
                 (dolist (f (delete-dups (delq nil (append bt nil))))
                   (cl-incf (gethash f incl 0) n))))
             log)
    (maphash (lambda (f n) (push (list f n (gethash f self 0)) rows)) incl)
    (with-temp-file file
      (insert (format "samples %d\n" all))
      (dolist (r (seq-take (sort rows (lambda (a b) (> (nth 1 a) (nth 1 b)))) 150))
        (insert (format "%6.1f%% incl %6.1f%% self  %s\n"
                        (/ (* 100.0 (nth 1 r)) all) (/ (* 100.0 (nth 2 r)) all)
                        (let ((f (nth 0 r)))
                          (if (symbolp f) f (truncate-string-to-width (format "%S" f) 60)))))))))

(let* ((t0 (float-time))
       (_ (require 'eas))
       (template (getenv "EAS_FR_TEMPLATE"))
       (eas-geo-backend (intern (or (getenv "EAS_GEO_BACKEND") "auto")))
       (bindings (ignore-errors (eas-template-example template)))
       (profile (getenv "EAS_FR_PROFILE"))
       (gcs gcs-done) (gce gc-elapsed)
       (_ (when profile (profiler-start 'cpu)))
       (t1 (float-time))
       (view (eas-view-open template :bindings bindings :target 'svg))
       (svg (eas-svg-render (eas-view-scene view)))
       (t2 (float-time))
       (gcs (- gcs-done gcs)) (gce (- gc-elapsed gce))
       (t3 (progn (garbage-collect) (float-time))))
  (when profile (geo-first-render--profile profile))
  (princ (format "%s total %.0f render %.0f gc %d %.0f trailing-gc %.0f backend %s md5 %s\n"
                 template (* 1000 (- t2 t0)) (* 1000 (- t2 t1)) gcs (* 1000 gce)
                 (* 1000 (- t3 t2)) (eas-geo-backend-active) (md5 svg))))

;;; geo-first-render.el ends here
