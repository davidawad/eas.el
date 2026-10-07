;;; eas-mode.el --- native glue: buffers, keys and mouse -> event/v1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  `eas-show' displays a view in a buffer: an SVG image
;; with :map hot spots in GUI frames, propertized text in terminals.
;; The glue only translates native input into event/v1 and calls
;; `eas-dispatch'; all behaviour lives in the reducer.
;;
;;   GUI       posn-object-x-y inside the image (divided by its :scale)
;;             -> px; mouse-movement needs `track-mouse', set locally.
;;   terminal  point motion, and xterm-mouse clicks/motion, -> the
;;             pixel centre of the cell.  Drags are built from
;;             down/motion/up (fc-qx1.14: no drag-mouse-1 under tmux).
;;
;; One keymap serves both: + - 0 zoom, [ ] history, < > pan, ESC
;; clears selections, RET clicks at point, g redraws, q quits.  In GUI
;; buffers the arrows pan; in text buffers they move point (hover) and
;; S-arrows pan.  The wheel (xterm-mouse's mouse-4/5 in terminals) and a
;; trackpad pinch zoom around the pointer; horizontal scrolling pans.
;; Redraws are idle-coalesced (spikes 8.8) while the header-line readout
;; updates on every move; text redraws rewrite only the changed cells
;; (`eas-mode-patch-text').

;;; Code:

(require 'eas-view)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-zoom)
(require 'eas-crosshair)
(require 'eas-mode-patch)
(require 'eas-mode-strip)
(require 'eas-readout)
(require 'eas-gc)
(require 'eas-live)

(defvar-local eas-mode--view nil "The view this buffer shows.")
(defvar-local eas-mode--timer nil "Pending idle redraw.")
(defvar-local eas-mode--last-px nil "Last pointer position sent.")
(defvar-local eas-mode--last-cell nil "Point's (LINE . COLUMN) when it last hovered.")
(defvar-local eas-mode--pinch nil "Last pinch scale of the current gesture.")
(defvar-local eas-mode--stale nil "Non-nil when the view changed while hidden.")

(defun eas-mode--gui-p ()
  "Non-nil when this buffer draws an image."
  (eq (eas-view-target eas-mode--view) 'svg))

(defun eas-mode--cell ()
  "The scene's cell size [W H]."
  (plist-get (plist-get (eas-view-scene eas-mode--view) :size) :cell))

(defun eas-mode-event-px (event &optional snap)
  "Scene pixels of mouse EVENT, or nil when it is not over the chart.
GUI events use `posn-object-x-y' divided by the image :scale; text
events use the cell under the pointer (SNAP as in `eas-mode-point-px')."
  (let* ((posn (event-start event)))
    (if (and (posn-image posn) (posn-object-x-y posn))
        (let* ((xy (posn-object-x-y posn))
               (scale (plist-get (cdr (posn-image posn)) :scale))
               ;; `create-image' leaves :scale `default' when none is given.
               (scale (if (numberp scale) scale 1)))
          (vector (/ (car xy) (float scale)) (/ (cdr xy) (float scale))))
      (when-let* ((pos (posn-point posn)))
        (eas-mode-point-px pos snap)))))

(defun eas-mode-point-px (&optional pos snap)
  "Scene pixels at the centre of the text cell at POS (default point).
With SNAP, the datum drawn in the cell when it holds one: a cell is
wider than the click slop, so clicks and presses snap to hit what is
shown.  Point-motion hover snaps too, since hover needs the pointer on a
mark; xterm-mouse motion does not."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((cell (eas-mode--cell)))
      (or (and snap (eas-mode--datum-px (get-text-property (point) 'eas-view)
                                          (get-text-property (point) 'eas-mark)
                                          (get-text-property (point) 'eas-datum)
                                          (current-column) (1- (line-number-at-pos)) cell))
          (vector (* (+ (current-column) 0.5) (aref cell 0))
                  (* (+ (1- (line-number-at-pos)) 0.5) (aref cell 1)))))))

(defun eas-mode--datum-px (view-id mark-id datum col row cell)
  "Pixel of DATUM of MARK-ID in VIEW-ID when it lies in or next to cell COL ROW.
The text renderer pulls glyphs on the plot's edge one cell inward."
  (when-let* ((datum)
              (view (seq-find (lambda (v) (equal (plist-get v :id) view-id))
                              (plist-get (eas-view-scene eas-mode--view) :views)))
              (mark (seq-find (lambda (m) (equal (plist-get m :id) mark-id)) (plist-get view :marks))))
    (cl-loop for item across (plist-get mark :items)
             for d = (plist-get item :datum)
             for k = (and (vectorp d) (seq-position d datum))
             for p = (cond (k (eas-hit--item-point item k)) ((equal d datum) (eas-hit--item-point item nil)))
             when (and p (<= (abs (- (floor (car p) (aref cell 0)) col)) 1)
                       (<= (abs (- (floor (cdr p) (aref cell 1)) row)) 1))
             return (vector (float (car p)) (float (cdr p))))))

(defun eas-mode--send (event)
  "Dispatch EVENT to this buffer's view, reporting failures in the echo area.
Collection waits until Emacs is idle (`eas-gc-defer')."
  (eas-gc-defer)
  (condition-case err
      (eas-dispatch eas-mode--view event)
    (eas-error (message "eas: %s" (plist-get (eas-error-plist err) :message)) nil)))

;;; Rendering

(defun eas-mode--window-size (window target)
  "Size to compile for WINDOW and TARGET, leaving the readout's lines.
A text chart is as wide as WINDOW shows a line whole: its body less
line numbers and, in a terminal, the column the truncation glyph takes
\(`window-max-chars-per-line'), so no line ends in `$' (fc-qx1.52)."
  (if (eq target 'svg)
      (cons (window-body-width window t)
            (- (window-body-height window t) 4
               (* (eas-mode--readout-lines) (with-selected-window window (default-line-height)))))
    ;; A terminal window with a neighbour to its right also loses the
    ;; vertical-border column: `window-max-chars-per-line' still counts it,
    ;; so a full-width line ends in `$' (seen in neomacs -nw, 2026-10-05).
    (list :cols (max 20 (- (with-selected-window window (window-max-chars-per-line window))
                           (if (and (not (display-graphic-p (window-frame window)))
                                    (window-right window))
                               1 0)))
          ;; The header line counts once: the body excludes it when it is set.
          :rows (max 6 (- (window-body-height window) (eas-mode--readout-lines)
                          (if (buffer-local-value 'header-line-format (window-buffer window)) 0 1))))))

(defun eas-mode--readout-lines ()
  "Lines this buffer's view reserves for its readout (eas-anj)."
  (if eas-mode--view (eas-readout-max-lines eas-mode--view) 1))

(defun eas-mode--flush-replaced (old new)
  "Drop image OLD from the image cache, unless the image NEW repeats it.
NEW is the image drawn next, or nil.
Each redraw is a new image; without a flush the cache keeps every one
\(fc-qx1.23: +1.28 MB per 800x400 move; fc-qx1.24: about 8 MB per
frame on a 1000x640 NS window).  When NEW has the same :data as OLD, the two
share one cache entry: flushing it would rasterize the same SVG again
\(fc-qx1.24: 90 ms for a hover that changed nothing), so it stays."
  (when (and (eq (car-safe old) 'image) (fboundp 'image-flush)
             (not (and (eq (car-safe new) 'image)
                       (equal (plist-get (cdr old) :data) (plist-get (cdr new) :data)))))
    (image-flush old t)))

(defun eas-mode--same-image-p (old new)
  "Non-nil when image NEW draws as OLD, the image already in the buffer.
Both must have the same SVG data and the same :map hot spots."
  (and (eq (car-safe old) 'image) (eq (car-safe new) 'image)
       (equal (plist-get (cdr old) :data) (plist-get (cdr new) :data))
       (equal (plist-get (cdr old) :map) (plist-get (cdr new) :map))
       (save-excursion (goto-char (point-min)) (forward-line 1) (bolp))))

(defun eas-mode-redraw (&optional buffer)
  "Redraw BUFFER (default current) from its view's scene."
  (with-current-buffer (or buffer (current-buffer))
    (setq eas-mode--timer nil eas-mode--stale nil)
    (let* ((view eas-mode--view) (scene (eas-view-scene view))
           ;; The marks the last update changed: renderers skip comparing
           ;; them, and compare the rest by `eq' (eas-b2s.7).
           (eas-render-cache-dirty (eas-view-dirty view))
           (inhibit-read-only t) (pos (point))
           ;; Text lines change length as labels change: keep point's cell.
           (line (line-number-at-pos)) (col (current-column)))
      (let ((old (get-text-property (point-min) 'display)))
        (cond
         ((not (eas-view-interactive view))
          (eas-mode--flush-replaced old nil) (erase-buffer) (eas-mode--insert-static view))
         ((eas-mode--gui-p)
          (let ((image (eas-svg-image scene :scale 1)))
            (if (eas-mode--same-image-p old image)
                ;; The same picture and hot spots: leave the image be.
                (progn (goto-char (point-min)) (forward-line 1)
                       (delete-region (point) (point-max))
                       (insert (eas-mode-strip-string view)))
              (eas-mode--flush-replaced old image)
              (erase-buffer)
              (insert-image image "[chart]")
              (insert "\n" (eas-mode-strip-string view)))
            (eas-mode--hot-spot-keys image)))
         ;; Terminal hover moves one column: rewrite only changed cells (fc-qx1.14).
         ;; A live frame (a push, a tick) defers collection as an event
         ;; does: at 4 frames a second the default threshold collected
         ;; every other frame, most of each frame's time (eas-b2s.5).
         (t (eas-gc-defer)
            (eas-mode--flush-replaced old nil)
            (eas-mode-patch-lines (nconc (eas-text-render-lines scene) (list (eas-mode-strip-string view)))))))
      (if (eas-mode--gui-p) (goto-char (min pos (point-max)))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col))
      (eas-mode--readout))))

(defun eas-mode--insert-static (view)
  "Insert why VIEW is static, after bin/chart's image if one was drawn.
There is an image only when `eas-static-fallback' is non-nil."
  (let ((fallback (eas-view-fallback view)))
    (when (and (plist-get fallback :data) (display-graphic-p) (image-type-available-p 'svg))
      (insert-image (create-image (plist-get fallback :data) 'svg t) "[chart]")
      (insert "\n"))
    (insert "Static chart (not interactive): the native engine does not support\n")
    (seq-doseq (w (eas-view-warnings view))
      (insert (format "  %s at %s\n" (or (plist-get w :feature) (plist-get w :code)) (plist-get w :path))))
    (when-let* ((err (plist-get fallback :error)))
      (insert (format "No static image (%s): %s\n" (plist-get err :code) (plist-get err :message))))))

(defvar eas-view-mode-map)

(defvar-local eas-mode--hot-spot-map nil
  "The local map binding hot-spot ids: (KEYMAP . IDS), IDS a hash table.")

(defun eas-mode--hot-spot-keys (image)
  "Bind IMAGE's :map area ids so clicks on hot spots reach the reducer.
The buffer keeps one map: a redraw binds only ids no earlier frame
bound (eas-b2s.2: rebinding 1000 points took 60 ms a frame)."
  (unless (and eas-mode--hot-spot-map (eq (current-local-map) (car eas-mode--hot-spot-map)))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map eas-view-mode-map)
      (use-local-map map)
      (setq eas-mode--hot-spot-map (cons map (make-hash-table :test 'eq)))))
  (pcase-let ((`(,map . ,ids) eas-mode--hot-spot-map))
    (dolist (area (plist-get (cdr image) :map))
      (let ((id (nth 1 area)))
        (unless (gethash id ids)
          (puthash id t ids)
          (dolist (key '(mouse-1 down-mouse-1 mouse-movement))
            (define-key map (vector id key) (lookup-key eas-view-mode-map (vector key)))))))))

(defun eas-mode--readout ()
  "Show the hovered datum's readout (or the view id) in the header line.
The readout lists its encoding.tooltip fields (`eas-crosshair-readout').
The values strip under the chart follows the pointer's column."
  (let* ((view eas-mode--view) (hover (plist-get (eas-view-state view) :hover))
         (last (car (eas-view-log view))))
    (setq header-line-format
          (string-trim-right
           (if hover
               (format " %s  %s" (eas-view-id view)
                       (eas-crosshair-format (eas-crosshair-view-readout view)))
             (format " %s  %s" (eas-view-id view) (or (plist-get last :summary) "")))))
    (eas-mode-strip-update view)))

(defun eas-mode--schedule (view)
  "Coalesce a redraw of every buffer showing VIEW.
A hidden buffer is only marked stale; it redraws when shown (eas-live)."
  (when-let* ((buffer (eas-view-buffer view)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond
         ((not (eas-live-visible-p view)) (setq eas-mode--stale t) (eas-live-defer view))
         (eas-mode--timer)
         (t
          (setq eas-mode--timer
                (if noninteractive (progn (eas-mode-redraw buffer) nil)
                  (run-with-idle-timer 0 nil #'eas-mode-redraw buffer)))))))))

(add-hook 'eas-view-changed-functions #'eas-mode--schedule)

(defun eas-mode--wake (view)
  "Redraw VIEW's buffer when it changed while hidden."
  (when-let* ((buffer (eas-view-buffer view)))
    (when (and (buffer-live-p buffer) (buffer-local-value 'eas-mode--stale buffer))
      (eas-mode-redraw buffer))))

(add-hook 'eas-live-wake-functions #'eas-mode--wake)

;;; Commands

(defun eas-mode-pointer (event)
  "Translate mouse motion EVENT into pointermove."
  (interactive "e")
  (when-let* ((px (eas-mode-event-px event)))
    (unless (equal px eas-mode--last-px)
      (setq eas-mode--last-px px)
      (eas-mode--send (list :type "pointermove" :px px))
      (eas-mode--readout))))

(defun eas-mode-down (event)
  "Translate a mouse press EVENT into pointerdown."
  (interactive "e")
  (when-let* ((px (eas-mode-event-px event t)))
    (eas-mode--send (list :type "pointerdown" :px px))))

(defun eas-mode-up (event)
  "Translate a mouse release (or drag end) EVENT into pointerup."
  (interactive "e")
  (when-let* ((px (eas-mode-event-px (if (memq 'drag (event-modifiers event))
                                           (list (car event) (event-end event))
                                         event)
                                       t)))
    (eas-mode--send (list :type "pointerup" :px px))
    (eas-mode--readout)))

(defun eas-mode-dblclick (event)
  "Translate a double click EVENT into dblclick."
  (interactive "e")
  (when-let* ((px (eas-mode-event-px event t)))
    (eas-mode--send (list :type "dblclick" :px px))))

(defun eas-mode-wheel (event)
  "Translate wheel EVENT into wheel zoom around the pointer."
  (interactive "e")
  (when-let* ((px (eas-mode-event-px event)))
    (eas-mode--send (list :type "wheel" :px px :delta (eas-zoom-wheel-delta event)))))

(defun eas-mode-pinch (event)
  "Translate a trackpad pinch EVENT into wheel zoom around the pointer.
EVENT is (pinch POSITION DX DY SCALE ANGLE); SCALE is relative to the
gesture's start, so each event zooms by its change from the last."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (or (null eas-mode--pinch) (and (= scale 1.0) (zerop (nth 2 event)) (zerop (nth 3 event))))
      (setq eas-mode--pinch 1.0))
    (let ((delta (eas-zoom-pinch-delta scale eas-mode--pinch)))
      (setq eas-mode--pinch scale)
      (when-let* (((/= delta 0)) (px (eas-mode-event-px event)))
        (eas-mode--send (list :type "wheel" :px px :delta delta))))))

(defun eas-mode-hscroll (event)
  "Translate horizontal scroll EVENT into a pan along x."
  (interactive "e")
  (eas-mode--send (list :type "key" :key (if (memq (event-basic-type event) '(wheel-left mouse-6))
                                               "left" "right"))))

(defun eas-mode-key ()
  "Send the key that invoked this command as an event/v1 key."
  (interactive)
  (let* ((key (this-command-keys-vector)) (k (aref key (1- (length key)))))
    (eas-mode--send (list :type "key"
                            :key (pcase k
                                   ((or 'left 'S-left ?<) "left") ((or 'right 'S-right ?>) "right")
                                   ((or 'up 'S-up) "up") ((or 'down 'S-down) "down")
                                   ((or 27 'escape) "escape") (_ (string k)))))))

(defun eas-mode-arrow ()
  "Pan in GUI buffers; move point (hover) in text buffers."
  (interactive)
  (if (eas-mode--gui-p) (eas-mode-key)
    (pcase (aref (this-command-keys-vector) 0)
      ('left (backward-char)) ('right (forward-char))
      ('up (line-move -1)) ('down (line-move 1)))))

(defun eas-mode-click-at-point ()
  "Click at point (terminal RET)."
  (interactive)
  (eas-mode--send (list :type "click" :px (eas-mode-point-px nil t))))

(defun eas-mode--post-command ()
  "In text buffers, moving point is hovering.
Only a move to another cell hovers: mouse events (xterm-mouse) carry
their own pointer, and a key pressed after them keeps the mouse's hover."
  (when (and eas-mode--view (not (eas-mode--gui-p)))
    (let ((cell (cons (line-number-at-pos) (current-column))))
      (cond ((consp last-command-event) (setq eas-mode--last-cell cell))
            ((not (equal cell eas-mode--last-cell))
             (setq eas-mode--last-cell cell)
             ;; Hover needs the pointer on a mark (fc-qx1.34): a cell that
             ;; draws a datum aims at it, as clicks do.
             (let ((px (eas-mode-point-px nil t)))
               (unless (equal px eas-mode--last-px)
                 (setq eas-mode--last-px px)
                 (eas-mode--send (list :type "pointermove" :px px))
                 (eas-mode--readout))))))))

(defun eas-mode-refresh ()
  "Recompile at the window's size and redraw."
  (interactive)
  (eas-view-resize eas-mode--view
                     (eas-mode--window-size (get-buffer-window) (eas-view-target eas-mode--view)))
  (eas-mode-redraw))

(defun eas-mode--follow-window (window)
  "Recompile this buffer's view when WINDOW, showing it, changed size.
Run from `window-size-change-functions' and `window-buffer-change-functions'
\(buffer-locally), so a chart always fills its window: after
`delete-other-windows', a split, a resized frame or a window switched
to the buffer.  WINDOW's buffer is made current: the hook may run
with another buffer current."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and eas-mode--view (eas-view-interactive eas-mode--view))
        (let* ((target (eas-view-target eas-mode--view))
               (size (eas-mode--window-size window target)))
          (unless (equal size (eas-view-size eas-mode--view))
            (eas-view-resize eas-mode--view size)
            ;; This hook runs inside redisplay: text changed here is not
            ;; shown until the next redisplay, which with no further input
            ;; never comes (a stale half-width chart after
            ;; `delete-other-windows').  Redraw from a timer instead, which
            ;; is followed by a redisplay.
            (let ((buffer (current-buffer)))
              (run-at-time 0 nil (lambda ()
                                   (when (buffer-live-p buffer)
                                     (eas-mode-redraw buffer)))))))))))

(defvar eas-view-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (k '("+" "=" "-" "0" "[" "]" "<" ">")) (define-key map k #'eas-mode-key))
    (dolist (k '([escape] "c")) (define-key map k (lambda () (interactive) (eas-mode--send '(:type "key" :key "escape")))))
    (dolist (k '([left] [right] [up] [down])) (define-key map k #'eas-mode-arrow))
    (dolist (k '([S-left] [S-right] [S-up] [S-down])) (define-key map k #'eas-mode-key))
    (define-key map (kbd "RET") #'eas-mode-click-at-point)
    (define-key map "g" #'eas-mode-refresh)
    (define-key map [mouse-movement] #'eas-mode-pointer)
    (define-key map [down-mouse-1] #'eas-mode-down)
    (define-key map [mouse-1] #'eas-mode-up)
    (define-key map [drag-mouse-1] #'eas-mode-up)
    (define-key map [double-mouse-1] #'eas-mode-dblclick)
    (dolist (k '([wheel-up] [wheel-down] [mouse-4] [mouse-5])) (define-key map k #'eas-mode-wheel))
    (dolist (k '([wheel-left] [wheel-right] [mouse-6] [mouse-7])) (define-key map k #'eas-mode-hscroll))
    (define-key map [pinch] #'eas-mode-pinch)
    map)
  "Keymap shared by GUI and terminal eas buffers.")

(define-derived-mode eas-view-mode special-mode "Eas"
  "Major mode for a live eas chart.  See `eas-view-mode-map'."
  (setq-local track-mouse t)
  (setq truncate-lines t)
  ;; Line numbers would take columns from the chart.
  (when (bound-and-true-p display-line-numbers-mode) (display-line-numbers-mode -1))
  (setq-local display-line-numbers nil)
  (add-hook 'post-command-hook #'eas-mode--post-command nil t)
  (add-hook 'window-size-change-functions #'eas-mode--follow-window nil t)
  (add-hook 'window-buffer-change-functions #'eas-mode--follow-window nil t))

(defvar eas-show-functions nil
  "Hook run with VIEW and its BUFFER after `eas-show' drew VIEW.
Runtime extensions start there what needs a shown view (eas-play's
timer and keys).")

(defun eas-show (view &optional target)
  "Show VIEW (an id or view) in its buffer; TARGET overrides svg/text.
The target defaults to svg in graphic frames with SVG support, else text."
  (let* ((view (eas-view-get view))
         (buffer (get-buffer-create (format "*eas %s*" (eas-view-id view))))
         (target (or target (if (and (display-graphic-p) (image-type-available-p 'svg)) 'svg 'text))))
    (with-current-buffer buffer
      (eas-view-mode)
      (setq eas-mode--view view)
      (setf (eas-view-buffer view) buffer))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (eas-view-resize view (eas-mode--window-size (get-buffer-window buffer) target) target)
      (eas-mode-redraw buffer))
    (run-hook-with-args 'eas-show-functions view buffer)
    buffer))

(provide 'eas-mode)
;;; eas-mode.el ends here
