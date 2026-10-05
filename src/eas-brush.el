;;; eas-brush.el --- brush select: interval selections on x as data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6/L7.  A brush is a Vega-Lite interval selection with
;; encodings ["x"] (design section 4); its store lives in view state
;; like every other param.  This file adds what a brush is for:
;;
;;   setting it   GUI: drag (the reducer already brushes on drag).
;;                Terminal: C-SPC at one end, move point, b.  Both
;;                ends become one data-space brush event, so the log
;;                reads "brush x 2026-03-01..2026-03-15".
;;   zooming      z (event {"type":"key","key":"z"}) sets the view's
;;                domains to the brush and clears it; [ undoes.
;;   exporting    `eas-brush-summary' (n, min, max, first, last,
;;                change, change %) and `eas-brush-emit', which sends
;;                the selection as rows, an org table or JSON to the
;;                echo area, the kill-ring or a callback (w copies).
;;   reacting     `eas-brush-functions' run whenever a brush changes.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-scale)
(require 'eas-params)
(require 'eas-reduce)
(require 'eas-view)
(require 'eas-mode)

(defvar eas-brush-functions (list #'eas-brush-echo)
  "Functions called with VIEW, PARAM name and SUMMARY when a brush changes.
SUMMARY is `eas-brush-summary' of the new selection, or nil when the
brush was cleared.")

(defun eas-brush--fmt (scale v)
  "Data value V on SCALE as an event/v1 value (ISO dates for time scales)."
  (if (and (member (plist-get scale :type) '("time" "utc")) (numberp v)) (eas-time-iso v) v))

(defun eas-brush-event-between (scene from to &optional param)
  "The brush event/v1 spanning scene pixels FROM..TO in SCENE.
The view is the one under FROM (else TO); its first interval selection
not bound to scales, or PARAM, is brushed.  Pixels outside the plot
clamp to its edge.  Signals EVENT_INVALID when there is nothing to brush."
  (let* ((view (or (eas-reduce--view-at scene from) (eas-reduce--view-at scene to)))
         (id (plist-get view :id))
         (p (and view (car (eas-reduce--params
                            scene id (lambda (p) (and (eas-reduce--brush-p p)
                                                      (or (null param) (equal (plist-get p :name) param))))))))
         (event (and p (list :type "brush" :param (plist-get p :name)))))
    (unless p
      (eas-signal "EVENT_INVALID"
                    (if view (format "View %s has no brush (an interval selection not bound to scales)" id)
                      "Both ends of a brush must be inside a plot")
                    :field "param"))
    (dolist (ch (eas-reduce--channels p))
      (when-let* ((scale (eas-reduce--scale scene id ch)))
        (let* ((i (if (eq ch :x) 0 1)) (r (plist-get scale :range))
               (clamp (lambda (px) (max (min (aref r 0) (aref r 1)) (min (max (aref r 0) (aref r 1)) (aref px i)))))
               (a (eas-scale-invert scale (funcall clamp from)))
               (b (eas-scale-invert scale (funcall clamp to))))
          (setq event (append event (list ch (vector (eas-brush--fmt scale (min a b))
                                                     (eas-brush--fmt scale (max a b)))))))))
    event))

;;; Summary and export

(defun eas-brush--param (view name)
  "VIEW's brush param NAME, or its first brush holding a value."
  (let* ((params (plist-get (eas-view-state view) :params))
         (brushes (seq-filter #'eas-reduce--brush-p (eas-params-of (eas-view-scene view)))))
    (if name (seq-find (lambda (p) (equal (plist-get p :name) name)) brushes)
      (seq-find (lambda (p) (plist-get params (eas-key (plist-get p :name)))) brushes))))

(defun eas-brush-summary (view &optional name)
  "Summary of VIEW's brush NAME (default: the first non-empty), or nil.
A plist: :param :view :x [FROM TO] :field (the y field) :n, and when
the selection has y values :min :max :first :last :change :change-pct,
first and last in x order."
  (when-let* ((view (eas-view-get view))
              (p (eas-brush--param view name))
              (store (plist-get (plist-get (eas-view-state view) :params) (eas-key (plist-get p :name)))))
    (let* ((scales (plist-get (eas-reduce--view (eas-view-scene view) (plist-get p :view)) :scales))
           (yf (plist-get (plist-get scales :y) :field))
           (xf (plist-get (plist-get store :fields) :x))
           (x-of (lambda (r) (or (eas-params--number (plist-get r (eas-key xf))) 0)))
           (rows (append (eas-selection view (plist-get p :name)) nil))
           (rows (if xf (sort rows (lambda (a b) (< (funcall x-of a) (funcall x-of b)))) rows))
           (values (and yf (seq-filter #'numberp (mapcar (lambda (r) (plist-get r (eas-key yf))) rows))))
           (first (car values)) (last (car (last values))))
      (append (list :param (plist-get p :name) :view (plist-get p :view)
                    :x (if-let* ((r (plist-get store :x)))
                           (vector (eas-brush--fmt (plist-get scales :x) (aref r 0))
                                   (eas-brush--fmt (plist-get scales :x) (aref r 1)))
                         :null)
                    :field (or yf :null) :n (length rows))
              (when values
                (list :min (apply #'min values) :max (apply #'max values) :first first :last last
                      :change (- last first)
                      :change-pct (if (zerop first) :null
                                    (/ (round (* 10000.0 (/ (- last first) (float first)))) 100.0))))))))

(defun eas-brush-format (summary)
  "SUMMARY as one line for the echo area."
  (let ((r (plist-get summary :x)))
    (concat (format "%s %s: n=%d" (plist-get summary :param)
                    (if (vectorp r) (format "%s..%s" (aref r 0) (aref r 1)) "") (plist-get summary :n))
            (when (plist-get summary :min)
              (format "  %s min %s max %s first %s last %s change %s%s"
                      (plist-get summary :field) (plist-get summary :min) (plist-get summary :max)
                      (plist-get summary :first) (plist-get summary :last) (plist-get summary :change)
                      (let ((pct (plist-get summary :change-pct)))
                        (if (numberp pct) (format " (%+.2f%%)" pct) "")))))))

(cl-defun eas-brush-emit (view &key param as to)
  "Emit VIEW's brushed rows; return (:summary SUMMARY :selection DATA).
PARAM names the brush (default: the first non-empty).  AS is rows,
json or org (default rows, or org when TO is `kill-ring').  TO is
`echo' (message the summary), `kill-ring' (copy the selection, rows as
JSON, and message the summary), a function called with DATA and
SUMMARY, or nil to only return.  Signals NOT_FOUND when nothing is
brushed."
  (let* ((view (eas-view-get view))
         (summary (eas-brush-summary view param)))
    (unless summary
      (eas-signal "NOT_FOUND" (format "View %s has no brushed selection%s; drag (or C-SPC, move, b) to brush"
                                        (eas-view-id view) (if param (format " %s" param) ""))
                    :view (eas-view-id view) :param (or param :null)))
    (let* ((as (or as (if (eq to 'kill-ring) 'org 'rows)))
           (data (eas-selection view (plist-get summary :param) as)))
      (pcase to
        ('nil nil)
        ('echo (message "%s" (eas-brush-format summary)))
        ('kill-ring (kill-new (if (stringp data) data (eas-json-encode data)))
                    (message "Copied %s" (eas-brush-format summary)))
        ((pred functionp) (funcall to data summary))
        (_ (eas-signal "INVALID_INPUT" "TO is echo, kill-ring, a function or nil" :to (format "%S" to))))
      (list :summary summary :selection data))))

;;; Change notification

(defvar eas-brush--seen (make-hash-table :test 'eq :weakness 'key)
  "Brush stores last reported, per view.")

(defun eas-brush--changed (view)
  "Run `eas-brush-functions' for each of VIEW's brushes that changed."
  (let ((params (plist-get (eas-view-state view) :params))
        (seen (gethash view eas-brush--seen)))
    (dolist (p (seq-filter #'eas-reduce--brush-p (eas-params-of (eas-view-scene view))))
      (let* ((key (eas-key (plist-get p :name))) (store (plist-get params key)))
        (unless (equal store (plist-get seen key))
          (setq seen (eas-plist-put seen key store))
          (run-hook-with-args 'eas-brush-functions view (plist-get p :name)
                              (and store (eas-brush-summary view (plist-get p :name)))))))
    (puthash view seen eas-brush--seen)))

(add-hook 'eas-view-changed-functions #'eas-brush--changed)

(defun eas-brush-echo (view _param summary)
  "Echo SUMMARY when VIEW is shown in a buffer; headless dispatch stays quiet."
  (when (and summary (buffer-live-p (eas-view-buffer view)))
    (message "%s" (eas-brush-format summary))))

;;; Commands

(defun eas-brush-region ()
  "Brush from mark to point: the terminal's drag."
  (interactive)
  (unless (mark t) (user-error "Set the mark (C-SPC) at one end of the range, move point, then b"))
  (let ((from (eas-mode-point-px (mark t))) (to (eas-mode-point-px)))
    (deactivate-mark)
    (condition-case err
        (eas-mode--send (eas-brush-event-between (eas-view-scene eas-mode--view) from to))
      (eas-error (message "eas: %s" (plist-get (eas-error-plist err) :message))))))

(defun eas-brush-copy (&optional as)
  "Copy the brushed rows to the kill-ring as an org table (AS json with a prefix)."
  (interactive (list (if current-prefix-arg 'json 'org)))
  (condition-case err
      (eas-brush-emit eas-mode--view :as (or as 'org) :to 'kill-ring)
    (eas-error (message "eas: %s" (plist-get (eas-error-plist err) :message)))))

(define-key eas-view-mode-map "b" #'eas-brush-region)
(define-key eas-view-mode-map "z" #'eas-mode-key)
(define-key eas-view-mode-map "w" #'eas-brush-copy)

(provide 'eas-brush)
;;; eas-brush.el ends here
