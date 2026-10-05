;;; eas-babel-inline.el --- org-babel: the live view inline in the org buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L7 (fc-qx1.13).  After an eas block runs with :as view,
;; its result (the text chart, as : lines or an example block) gets an
;; overlay that makes it the view:
;;
;;   GUI        the overlay displays the view's SVG over the text; mouse
;;              motion, press/release (click, brush), wheel and the
;;              image's :map hot spots reach the reducer.
;;   terminal   the text stays and is the view: moving point over it
;;              hovers, RET clicks, and the result is rewritten when
;;              zoom, pan, brush or data change what is drawn.
;;
;; Keys on the result: + - 0 zoom, [ ] history, < > pan, ESC clears,
;; RET clicks at point, o opens the chart in its own eas buffer, g
;; redraws.  Like eas-mode.el this only translates native input into
;; event/v1 for `eas-dispatch'.  The hovered row shows in the echo
;; area.  Text mapping assumes the result starts in column 0.

;;; Code:

(require 'eas-babel)
(require 'eas-babel-source)

(declare-function org-babel-where-is-src-block-result "ob-core" (&optional insert info hash))
(declare-function org-babel-result-end "ob-core" ())

(defvar eas-babel-inline-map)

(defvar eas-babel-inline--views (make-hash-table :test 'equal)
  "Inline views: view id -> (:block MARKER :overlay OVERLAY).")

(defvar eas-babel-inline--pending nil
  "Views opened by the block being executed: (VIEW . BLOCK-MARKER).")

(defun eas-babel-inline--on-open (view _source block)
  "Queue VIEW, opened by the block at marker BLOCK, for an overlay."
  (when block
    (push (cons view (copy-marker block)) eas-babel-inline--pending)))

(add-hook 'eas-babel-after-open-functions #'eas-babel-inline--on-open)

;;; The result region

(defun eas-babel-inline--region (block)
  "(START . END) of the result lines of the block at BLOCK, or nil.
START is the first chart line; END the end of the last."
  (with-current-buffer (marker-buffer block)
    (save-excursion
      (goto-char block)
      (when-let* ((res (org-babel-where-is-src-block-result)))
        (goto-char res)
        (forward-line 1)
        (let ((case-fold-search t) (start (point))
              (end (save-excursion (org-babel-result-end))))
          (when (looking-at-p "[ \t]*#\\+begin_example")
            (forward-line 1)
            (setq start (point))
            (goto-char end)
            (when (re-search-backward "^[ \t]*#\\+end_example" start t)
              (setq end (point))))
          (goto-char end)
          (skip-chars-backward "\n" start)
          (and (< start (point)) (cons start (point))))))))

(defun eas-babel-inline--prefix ()
  "Columns before the chart on the current result line (`: ' or `,')."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "[ \t]*:\\(?: \\|$\\)") (- (match-end 0) (point)))
          ((looking-at-p ",\\(?:\\*\\|#\\+\\)") 1)
          (t 0))))

(defun eas-babel-inline--format (text fixed)
  "Chart TEXT as result lines: `: ' prefixed when FIXED, else escaped."
  (mapconcat (lambda (line)
               (cond (fixed (if (string-empty-p line) ":" (concat ": " line)))
                     ((string-match-p "\\`\\(?:\\*\\|#\\+\\)" line) (concat "," line))
                     (t line)))
             (split-string (string-trim-right text "\n+") "\n")
             "\n"))

;;; Views and overlays

(defun eas-babel-inline--entry (view)
  "The inline entry of VIEW (an id or view), or nil."
  (gethash (if (stringp view) view (eas-view-id view)) eas-babel-inline--views))

(defun eas-babel-inline--gui-p (view)
  "Non-nil when VIEW draws an image."
  (eq (eas-view-target view) 'svg))

(defun eas-babel-inline--show-image (ov view)
  "Display VIEW's SVG on overlay OV, its :map hot spots reaching the reducer.
A click on a hot spot arrives as [AREA-ID mouse-1], so each area id is
bound like the plain mouse keys (as `eas-mode' does)."
  (let ((image (eas-svg-image (eas-view-scene view)))
        (map (make-sparse-keymap)))
    (set-keymap-parent map eas-babel-inline-map)
    (dolist (area (plist-get (cdr image) :map))
      (dolist (key '(mouse-1 down-mouse-1 mouse-movement))
        (define-key map (vector (nth 1 area) key) (lookup-key eas-babel-inline-map (vector key)))))
    (overlay-put ov 'display image)
    (overlay-put ov 'keymap map)))

(defun eas-babel-inline-attach (view block)
  "Make the result of the block at BLOCK show VIEW; return the overlay."
  (let ((old (eas-babel-inline--entry view)))
    (when-let* ((ov (plist-get old :overlay))) (delete-overlay ov)))
  (when-let* ((region (eas-babel-inline--region block)))
    (with-current-buffer (marker-buffer block)
      (let ((ov (make-overlay (car region) (cdr region) nil nil t)))
        (overlay-put ov 'eas-babel-view (eas-view-id view))
        (overlay-put ov 'keymap eas-babel-inline-map)
        (overlay-put ov 'help-echo nil)
        (when (eas-babel-inline--gui-p view)
          (setq-local track-mouse t)
          (eas-babel-inline--show-image ov view))
        (add-hook 'post-command-hook #'eas-babel-inline--post-command nil t)
        (puthash (eas-view-id view) (list :block block :overlay ov) eas-babel-inline--views)
        ov))))

(defun eas-babel-inline-attach-pending ()
  "Attach every view the last block execution opened."
  (let ((pending (nreverse eas-babel-inline--pending)))
    (setq eas-babel-inline--pending nil)
    (pcase-dolist (`(,view . ,block) pending)
      (when (and (buffer-live-p (marker-buffer block)) (gethash (eas-view-id view) eas-views))
        (eas-babel-inline-attach view block)))))

(add-hook 'org-babel-after-execute-hook #'eas-babel-inline-attach-pending)

(defun eas-babel-inline-redraw (view)
  "Show VIEW's current scene in its inline result."
  (when-let* ((entry (eas-babel-inline--entry view))
              (block (plist-get entry :block))
              ((buffer-live-p (marker-buffer block))))
    (if (eas-babel-inline--gui-p view)
        (when-let* ((ov (plist-get entry :overlay)) ((overlay-buffer ov)))
          (eas-babel-inline--show-image ov view))
      (when-let* ((region (eas-babel-inline--region block)))
        (with-current-buffer (marker-buffer block)
          (let* ((inhibit-read-only t)
                 (fixed (save-excursion (goto-char (car region))
                                        (looking-at-p "[ \t]*:\\(?: \\|$\\)")))
                 (line (and (<= (car region) (point) (cdr region)) (count-lines (car region) (point))))
                 (col (current-column)))
            (save-excursion
              (goto-char (car region))
              (delete-region (car region) (cdr region))
              (insert (eas-babel-inline--format
                       (substring-no-properties (eas-text-render (eas-view-scene view))) fixed)))
            (when line
              (goto-char (car region))
              (forward-line (max 0 (1- line)))
              (move-to-column col))
            (eas-babel-inline-attach view block)))))))

(defun eas-babel-inline--on-change (view)
  "Redraw VIEW inline when it has an inline result."
  (when (eas-babel-inline--entry view)
    (eas-babel-inline-redraw view)))

(add-hook 'eas-view-changed-functions #'eas-babel-inline--on-change)

;;; Input

(defun eas-babel-inline--overlay-at (pos)
  "The inline overlay at POS, or nil."
  (seq-find (lambda (ov) (overlay-get ov 'eas-babel-view)) (overlays-at pos)))

(defun eas-babel-inline--view-at (pos)
  "The live view whose inline result covers POS, or nil."
  (when-let* ((ov (eas-babel-inline--overlay-at pos)))
    (gethash (overlay-get ov 'eas-babel-view) eas-views)))

(defun eas-babel-inline-pos-px (pos)
  "Scene pixels at the centre of the chart cell at POS, or nil.
POS lies in a text inline result."
  (when-let* ((ov (eas-babel-inline--overlay-at pos))
              (view (gethash (overlay-get ov 'eas-babel-view) eas-views))
              (cell (plist-get (plist-get (eas-view-scene view) :size) :cell)))
    (save-excursion
      (goto-char pos)
      (let ((col (- (current-column) (eas-babel-inline--prefix)))
            (row (count-lines (overlay-start ov) (line-beginning-position))))
        (when (>= col 0)
          (vector (* (+ col 0.5) (aref cell 0)) (* (+ row 0.5) (aref cell 1))))))))

(defun eas-babel-inline-event-px (event)
  "Scene pixels of mouse EVENT over an inline result, or nil."
  (let ((posn (event-start event)))
    (if (and (posn-image posn) (posn-object-x-y posn))
        (let ((xy (posn-object-x-y posn))
              (scale (or (plist-get (cdr (posn-image posn)) :scale) 1)))
          (vector (/ (car xy) (float scale)) (/ (cdr xy) (float scale))))
      (when-let* ((pos (posn-point posn))) (eas-babel-inline-pos-px pos)))))

(defun eas-babel-inline--readout (view)
  "Echo VIEW's hovered row, when it has one."
  (let ((hover (plist-get (eas-inspect view) :hover)))
    (when (and hover (not (eq hover :null)))
      (message "%s  %s" (eas-view-id view)
               (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                          (cl-loop for (k v) on (plist-get hover :row) by #'cddr
                                   collect (cons (eas-key-name k) v))
                          "  ")))))

(defun eas-babel-inline-send (view event)
  "Dispatch EVENT to VIEW, reporting failures in the echo area."
  (condition-case err
      (eas-dispatch view event)
    (eas-error (message "eas: %s" (plist-get (eas-error-plist err) :message)) nil)))

(defun eas-babel-inline--event-view (event)
  "The view under mouse EVENT."
  (let ((posn (event-start event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (eas-babel-inline--view-at (or (posn-point posn) (point))))))

(defun eas-babel-inline-pointer (event)
  "Translate mouse motion EVENT over an inline result into pointermove."
  (interactive "e")
  (when-let* ((view (eas-babel-inline--event-view event))
              (px (eas-babel-inline-event-px event)))
    (eas-babel-inline-send view (list :type "pointermove" :px px))
    (eas-babel-inline--readout view)))

(defun eas-babel-inline-down (event)
  "Translate a press EVENT into pointerdown."
  (interactive "e")
  (when-let* ((view (eas-babel-inline--event-view event))
              (px (eas-babel-inline-event-px event)))
    (when (posn-point (event-start event)) (goto-char (posn-point (event-start event))))
    (eas-babel-inline-send view (list :type "pointerdown" :px px))))

(defun eas-babel-inline-up (event)
  "Translate a release (or drag end) EVENT into pointerup."
  (interactive "e")
  (let ((event (if (memq 'drag (event-modifiers event)) (list (car event) (event-end event)) event)))
    (when-let* ((view (eas-babel-inline--event-view event))
                (px (eas-babel-inline-event-px event)))
      (eas-babel-inline-send view (list :type "pointerup" :px px)))))

(defun eas-babel-inline-wheel (event)
  "Translate wheel EVENT into wheel zoom around the pointer."
  (interactive "e")
  (when-let* ((view (eas-babel-inline--event-view event))
              (px (eas-babel-inline-event-px event)))
    (eas-babel-inline-send view (list :type "wheel" :px px :delta (eas-zoom-wheel-delta event)))))

(defun eas-babel-inline-key ()
  "Send the key that invoked this command to the view at point."
  (interactive)
  (when-let* ((view (eas-babel-inline--view-at (point))))
    (let* ((keys (this-command-keys-vector)) (k (aref keys (1- (length keys)))))
      (eas-babel-inline-send view (list :type "key"
                                          :key (pcase k
                                                 (?< "left") (?> "right")
                                                 ((or 27 'escape) "escape")
                                                 (_ (string k))))))))

(defun eas-babel-inline-click-at-point ()
  "Click the chart cell at point (terminal RET); in GUI, open the chart."
  (interactive)
  (when-let* ((view (eas-babel-inline--view-at (point))))
    (if (eas-babel-inline--gui-p view) (eas-babel-inline-open)
      (when-let* ((px (eas-babel-inline-pos-px (point))))
        (eas-babel-inline-send view (list :type "click" :px px))))))

(defun eas-babel-inline-open ()
  "Open the chart at point in its own eas buffer, as a second view."
  (interactive)
  (when-let* ((view (eas-babel-inline--view-at (point))))
    (let ((full (eas-view-open (eas-view-spec view) :id (concat (eas-view-id view) "/full")
                                 :rows (plist-get (eas-view-data view) :rows))))
      (setf (eas-view-template full) (eas-view-template view))
      (run-hook-with-args 'eas-babel-after-open-functions full (eas-babel-source-of view) nil)
      (eas-show full))))

(defun eas-babel-inline-refresh ()
  "Redraw the chart at point."
  (interactive)
  (when-let* ((view (eas-babel-inline--view-at (point))))
    (eas-babel-inline-redraw view)))

(defvar-local eas-babel-inline--last-px nil "Last px sent by moving point.")

(defun eas-babel-inline--post-command ()
  "In text inline results, moving point is hovering."
  (when-let* ((view (eas-babel-inline--view-at (point)))
              ((not (eas-babel-inline--gui-p view)))
              (px (eas-babel-inline-pos-px (point))))
    (unless (equal px eas-babel-inline--last-px)
      (setq eas-babel-inline--last-px px)
      (eas-babel-inline-send view (list :type "pointermove" :px px))
      (eas-babel-inline--readout view))))

(defvar eas-babel-inline-map
  (let ((map (make-sparse-keymap)))
    (dolist (k '("+" "=" "-" "0" "[" "]" "<" ">")) (define-key map k #'eas-babel-inline-key))
    (define-key map [escape] #'eas-babel-inline-key)
    (define-key map (kbd "RET") #'eas-babel-inline-click-at-point)
    (define-key map "o" #'eas-babel-inline-open)
    (define-key map "g" #'eas-babel-inline-refresh)
    (define-key map [mouse-movement] #'eas-babel-inline-pointer)
    (define-key map [down-mouse-1] #'eas-babel-inline-down)
    (define-key map [mouse-1] #'eas-babel-inline-up)
    (define-key map [drag-mouse-1] #'eas-babel-inline-up)
    (dolist (k '([wheel-up] [wheel-down] [mouse-4] [mouse-5])) (define-key map k #'eas-babel-inline-wheel))
    map)
  "Keymap on an inline eas result, GUI and terminal alike.")

(provide 'eas-babel-inline)
;;; eas-babel-inline.el ends here
