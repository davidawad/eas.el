;;; eas-vui.el --- optional vui.el components: an eas chart on any vui page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L7.  vui.el (declarative, component-based buffer UIs) is a
;; soft dependency: eas never requires it, and `eas' does not load this
;; file.  Load it yourself, before or after vui:
;;
;;   (require 'eas-vui)
;;
;; Once vui is loaded (`with-eval-after-load'), two components exist:
;;
;;   (vui-component 'eas-vui-chart
;;     :source "sankey" :bindings BINDINGS   ; or :source SPEC
;;     :size '(:cols 80 :rows 20)            ; text; (W . H) pixels for SVG
;;     :target nil                           ; svg in graphic frames, else text
;;     :on-click (lambda (row click) ...))   ; a datum was clicked
;;
;;   (vui-component 'eas-vui-card :title "Node" :row ROW)
;;
;; The chart is a live eas view (`eas-view-open'), drawn as the SVG
;; image or the text rendering the engine makes anyway, so hover text
;; (`help-echo') and colors come with it.  Keys on the chart: RET or
;; mouse-1 clicks the datum there (the echo area shows its tooltip and
;; :on-click gets its row), `o' opens the chart in its own eas buffer
;; with every interaction (`eas-show').  When a click changes what is
;; drawn (a selection), the component redraws.  The card lists a row's
;; fields as the engine's data tooltip does: every field but the
;; engine's own eas_* ones.
;;
;; Without vui nothing here is a component, but the functions under
;; them work in any buffer: `eas-vui-chart-open', `eas-vui-chart-string'
;; and `eas-vui-card-lines'.

;;; Code:

(require 'eas)

(declare-function vui-text "vui" (content &rest props))
(declare-function vui-vstack "vui" (&rest args))
(declare-function vui-set-state "vui" (key value))

(defvar eas-vui--count 0 "Views this file has opened, for fresh ids.")

(defvar eas-vui--callbacks (make-hash-table :test 'equal)
  "View id -> plist (:on-click FN :redraw FN) for the chart widgets.")

(defvar eas-vui--sources (make-hash-table :test 'equal)
  "View id -> what a chart widget opened it from, to reopen on change.")

(defvar eas-vui-chart-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eas-vui-click)
    (define-key map [mouse-1] #'eas-vui-click)
    (define-key map (kbd "o") #'eas-vui-open)
    map)
  "Keys on a chart drawn by `eas-vui-chart-string'.")

(defun eas-vui-available-p ()
  "Non-nil when vui.el is loaded, so the components exist."
  (featurep 'vui))

(defun eas-vui-default-target ()
  "The target a chart draws for here: svg in graphic frames, else text."
  (if (and (display-graphic-p) (image-type-available-p 'svg)) 'svg 'text))

(cl-defun eas-vui-chart-open (source &key id bindings rows size target)
  "Open a live view of SOURCE for a vui page; return the view.
SOURCE is a template name (resolved with BINDINGS) or a chart/v1 spec;
ROWS replaces its data.  SIZE is (:cols C :rows R) for text, (W . H)
pixels for svg (default 80x20 cells, or the spec's own size).  TARGET
defaults to `eas-vui-default-target'.  ID names the view (default
eas-vui-N); an open view of that id is replaced."
  (let* ((target (or target (eas-vui-default-target)))
         (id (or id (format "eas-vui-%d" (cl-incf eas-vui--count)))))
    (when (gethash id eas-views) (eas-view-close id))
    (eas-view-open source :id id :bindings bindings :rows rows :target target
                   :size (or size (and (eq target 'text) '(:cols 80 :rows 20))))))

(defun eas-vui-chart-close (view)
  "Forget VIEW (an id or view) and its widget callbacks."
  (when-let* ((view (ignore-errors (eas-view-get view))))
    (remhash (eas-view-id view) eas-vui--callbacks)
    (remhash (eas-view-id view) eas-vui--sources)
    (eas-view-close view)))

(cl-defun eas-vui-chart-string (view &key on-click redraw)
  "VIEW (an id or view) drawn as a string to insert in any buffer.
SVG views are one character displaying the image (with :map hot
spots), text views the text rendering; both carry `eas-vui-view' and
`eas-vui-chart-map'.  ON-CLICK is called with the clicked row and the
click plist; REDRAW with no argument when a click changed the chart."
  (let* ((view (eas-view-get view))
         (id (eas-view-id view))
         (scene (eas-view-scene view))
         (image (unless (equal (plist-get scene :target) "text") (eas-svg-image scene)))
         (map (if (not image) eas-vui-chart-map
                ;; A click on a hot spot arrives as [AREA-ID mouse-1].
                (let ((m (make-sparse-keymap)))
                  (set-keymap-parent m eas-vui-chart-map)
                  (dolist (area (plist-get (cdr image) :map) m)
                    (define-key m (vector (nth 1 area) 'mouse-1) #'eas-vui-click)))))
         (string (if image (propertize " " 'display image) (eas-text-render scene))))
    (puthash id (list :on-click on-click :redraw redraw) eas-vui--callbacks)
    (add-text-properties 0 (length string) (list 'eas-vui-view id 'keymap map) string)
    string))

(defun eas-vui--chart-start (pos id)
  "Where the chart ID around POS begins."
  (if (and (> pos (point-min)) (equal (get-text-property (1- pos) 'eas-vui-view) id))
      (or (previous-single-property-change pos 'eas-vui-view) (point-min))
    pos))

(defun eas-vui-point-px (&optional pos)
  "Scene pixels of the chart cell at POS (default point), or nil.
A text chart's cell is counted from where the chart begins, so the
chart may sit anywhere on a page, indented or not."
  (let* ((pos (or pos (point)))
         (id (get-text-property pos 'eas-vui-view))
         (view (and id (gethash id eas-views))))
    (when view
      (save-excursion
        (let* ((start (eas-vui--chart-start pos id))
               (col0 (progn (goto-char start) (current-column)))
               (line0 (line-number-at-pos start))
               (cell (plist-get (plist-get (eas-view-scene view) :size) :cell)))
          (goto-char pos)
          (vector (* (+ (- (current-column) col0) 0.5) (aref cell 0))
                  (* (+ (- (line-number-at-pos pos) line0) 0.5) (aref cell 1))))))))

(defun eas-vui--event-px (event)
  "Scene pixels and view id of mouse EVENT on a chart: (ID . PX), or nil."
  (let* ((posn (event-start event)) (pos (posn-point posn))
         (id (and pos (get-text-property pos 'eas-vui-view (window-buffer (posn-window posn))))))
    (when id
      (cons id
            (if-let* ((xy (and (posn-image posn) (posn-object-x-y posn))))
                (let ((scale (plist-get (cdr (posn-image posn)) :scale)))
                  (vector (/ (car xy) (float (if (numberp scale) scale 1)))
                          (/ (cdr xy) (float (if (numberp scale) scale 1)))))
              (with-current-buffer (window-buffer (posn-window posn)) (eas-vui-point-px pos)))))))

(defun eas-vui-click-px (id px)
  "Click chart view ID at scene pixel PX; return the click plist or nil.
The echo area shows the clicked datum's tooltip; the widget's
on-click gets its row, and redraw runs when the chart changed."
  (when-let* ((view (gethash id eas-views)))
    (let* ((before (eas-view-scene view))
           (click (plist-get (eas-dispatch view (list :type "click" :px px)) :click))
           (callbacks (gethash id eas-vui--callbacks)))
      (when (and click (not (eq click :null)))
        (when-let* ((text (eas-tip-text (let ((tip (plist-get click :tooltip))) (and (vectorp tip) tip)))))
          (message "%s" text))
        (when-let* ((fn (plist-get callbacks :on-click)))
          (funcall fn (plist-get click :row) click)))
      (when-let* ((fn (and (not (eq before (eas-view-scene view))) (plist-get callbacks :redraw))))
        (funcall fn))
      (and (not (eq click :null)) click))))

(defun eas-vui-click (&optional event)
  "Click the chart datum at point, or under mouse EVENT."
  (interactive (list last-nonmenu-event))
  (when-let* ((hit (if (and event (mouse-event-p event)) (eas-vui--event-px event)
                     (let ((id (get-text-property (point) 'eas-vui-view)))
                       (and id (cons id (eas-vui-point-px)))))))
    (eas-vui-click-px (car hit) (cdr hit))))

(defun eas-vui-open ()
  "Open the chart at point in its own eas buffer, with every interaction."
  (interactive)
  (when-let* ((id (get-text-property (point) 'eas-vui-view)))
    (eas-show id)))

(defun eas-vui-card-lines (row &optional fields)
  "ROW's fields as (NAME . VALUE) strings, for a card.
FIELDS (names) picks and orders them; by default every field but the
engine's own, in row order, as a {\"content\": \"data\"} tooltip shows."
  (mapcar (lambda (p) (cons (plist-get p :title) (plist-get p :value)))
          (eas-encode-tooltip-row
           'data (if fields (cl-loop for f in fields append (list (eas-key f) (plist-get row (eas-key f))))
                   row))))

;;; The components, once vui is there

(defun eas-vui--define ()
  "Define the vui components `eas-vui-chart' and `eas-vui-card'.
Evaluated, not compiled: vui's macros exist only once vui is loaded."
  (eval
   '(progn
      (vui-defcomponent eas-vui-chart (source bindings rows id size target on-click)
        "An eas chart: SOURCE (template or spec) with BINDINGS, live."
        :state ((view-id (or id (format "eas-vui-%d" (cl-incf eas-vui--count))))
                (rev 0))
        :on-unmount (eas-vui-chart-close view-id)
        :render
        (let ((view (gethash view-id eas-views))
              (key (list source bindings rows size target)))
          (unless (and view (equal (gethash view-id eas-vui--sources) key))
            (setq view (eas-vui-chart-open source :id view-id :bindings bindings :rows rows
                                           :size size :target target))
            (puthash view-id key eas-vui--sources))
          (ignore rev)
          (vui-text (eas-vui-chart-string
                     view :on-click on-click
                     :redraw (vui-with-async-context (vui-set-state :rev #'1+))))))
      (vui-defcomponent eas-vui-card (title row fields)
        "A card listing ROW's fields (FIELDS, or all but eas_*) under TITLE."
        :render
        (let* ((lines (eas-vui-card-lines row fields))
               (width (apply #'max 0 (mapcar (lambda (l) (string-width (car l))) lines))))
          (vui-vstack
           (when title (vui-text title :face 'bold))
           (mapcar (lambda (l)
                     (vui-text (format "%s  %s" (string-pad (car l) width) (cdr l))))
                   lines)))))
   t))

(with-eval-after-load 'vui (eas-vui--define))

(provide 'eas-vui)
;;; eas-vui.el ends here
