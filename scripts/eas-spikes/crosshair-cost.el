;;; crosshair-cost.el --- terminal crosshair redraw: patch vs full rewrite -*- lexical-binding: t; -*-

;; fc-qx1.2.  The line template with "crosshair": true at 100x30 text
;; cells, N rows.  A move is one pointermove dispatch one cell to the
;; right plus one buffer redraw.  "patch" is `eas-mode-redraw'
;; (`eas-mode-patch-text'); "full" erases and inserts the grid, the
;; redraw before fc-qx1.2.  Batch, so no redisplay: the tty spike
;; (engine-spikes.md section 5) measured that part separately.
;;   scripts/eas-spikes/run-compiled.sh scripts/eas-spikes/crosshair-cost.el

(require 'eas)

;; run-compiled.sh copies sources flat; point at the repo's templates.
(setq eas-template-directories
      (list (expand-file-name "../../templates" (file-name-directory load-file-name))))

(defun crosshair-cost--rows (n)
  "N daily rows."
  (vconcat (cl-loop for i below n
                    collect (list :date (format-time-string "%Y-%m-%d" (* 86400 (+ 19000 i)) t)
                                  :value (+ 100 (* 5 (sin (/ i 7.0))))))))

(defun crosshair-cost--run (n full)
  "Mean ms and cells written per move over 60 moves at N rows; FULL rewrites."
  (let* ((eas-views (make-hash-table :test 'equal))
         (gc-cons-threshold (* 64 1024 1024))
         ;; One redraw per move, done below; no echo-area tooltip.
         (eas-view-changed-functions nil)
         (eas-mode-tip-display-function #'ignore)
         (view (eas-view-open "line" :bindings (list :data (crosshair-cost--rows n) :crosshair t)
                                :id "c" :target 'text :size '(:cols 100 :rows 30)))
         (cw (aref (plist-get (plist-get (eas-view-scene view) :size) :cell) 0))
         (written 0) (moves 60))
    (with-temp-buffer
      (eas-view-mode)
      (setq eas-mode--view view)
      (setf (eas-view-buffer view) (current-buffer))
      (eas-mode-redraw)
      (add-hook 'after-change-functions (lambda (b e _) (setq written (+ written (- e b)))) nil t)
      (setq written 0)
      (let ((start (float-time)))
        (dotimes (k moves)
          (eas-dispatch view (list :type "pointermove" :px (vector (* cw (+ 10.5 k)) 100)))
          (if full
              (let ((inhibit-read-only t))
                (erase-buffer) (insert (eas-text-render (eas-view-scene view))))
            (eas-mode-redraw)))
        (list :ms (/ (* 1000 (- (float-time) start)) moves) :cells (/ written moves))))))

(dolist (n '(1000 10000))
  (dolist (full '(nil t))
    (let ((r (crosshair-cost--run n full)))
      (princ (format "rows %6d  %-5s  %7.2f ms/move  %5d cells/move\n"
                     n (if full "full" "patch") (plist-get r :ms) (plist-get r :cells))))))
