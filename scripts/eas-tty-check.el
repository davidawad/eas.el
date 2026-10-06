;;; eas-tty-check.el --- layouts for scripts/eas-tty-check.sh -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loaded by scripts/eas-tty-check.sh into a real `emacs -nw' inside
;; tmux (fc-qx1.53: the batch fake window passed while the terminal
;; showed `$' at the right edge).  Each layout shows eas views as text
;; and writes the edges of the windows showing them to
;; `eas-tty-check-edges-file', one "LEFT TOP RIGHT BOTTOM ID" line per
;; window, so the script can check each window's last column in the
;; captured pane.

;;; Code:

(require 'eas-demo-candles)
(require 'eas-mode)
(require 'eas-text-gallery)

(defvar eas-tty-check-edges-file nil
  "File the layout writes window edges to.")

(defun eas-tty-check--write-edges ()
  "Write the edges of every window showing an eas view."
  (redisplay t)
  (with-temp-file eas-tty-check-edges-file
    (dolist (w (window-list nil 'no-minibuf))
      (when-let* ((view (buffer-local-value 'eas-mode--view (window-buffer w))))
        (pcase-let ((`(,l ,top ,r ,b) (window-edges w)))
          (insert (format "%d %d %d %d %s\n" l top r b (eas-view-id view))))))))

(defun eas-tty-check--show-in (window view)
  "Show VIEW as text in WINDOW."
  (with-selected-window window
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (eas-show view 'text))))

(defun eas-tty-check-full ()
  "The candles demo alone in a full-frame window."
  (eas-demo-candles)
  (delete-other-windows (get-buffer-window "*eas ohlc:TSM*"))
  ;; The resize redraws from a timer: write the edges after it.
  (run-at-time 1 nil #'eas-tty-check--write-edges))

(defun eas-tty-check--split (views)
  "Show VIEWS, four (WINDOW-INDEX . VIEW-FUNCTION), in a 2x2 split.
Window indices: 0 top left, 1 top right, 2 bottom left, 3 bottom right."
  (delete-other-windows)
  (let* ((tl (selected-window))
         (tr (split-window tl nil 'right))
         (windows (vector tl tr (split-window tl nil 'below) (split-window tr nil 'below))))
    (pcase-dolist (`(,i . ,open) views)
      (eas-tty-check--show-in (aref windows i) (funcall open))))
  (run-at-time 1 nil #'eas-tty-check--write-edges))

(defun eas-tty-check--gallery (group name)
  "A function opening gallery example NAME of GROUP as a text view."
  (lambda () (eas-view-open (eas-text-gallery-spec group name) :id name :target 'text)))

(defun eas-tty-check-split ()
  "2x2: the candles demo and the gallery examples that overflowed (fc-qx1.53)."
  (eas-tty-check--split
   `((0 . ,(lambda () (eas-demo-candles-open 'text)))
     (1 . ,(eas-tty-check--gallery "layered" "layer_bar_annotations"))
     (2 . ,(eas-tty-check--gallery "distributions" "layer_point_errorbar_ci"))
     (3 . ,(eas-tty-check--gallery "layered" "layer_bar_annotations")))))

(defun eas-tty-check-split-mirror ()
  "`eas-tty-check-split' with its columns swapped: each chart on the other side."
  (eas-tty-check--split
   `((1 . ,(lambda () (eas-demo-candles-open 'text)))
     (0 . ,(eas-tty-check--gallery "layered" "layer_bar_annotations"))
     (3 . ,(eas-tty-check--gallery "distributions" "layer_point_errorbar_ci"))
     (2 . ,(eas-tty-check--gallery "distributions" "layer_point_errorbar_ci")))))

(provide 'eas-tty-check)
;;; eas-tty-check.el ends here
