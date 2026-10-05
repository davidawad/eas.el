;;; eas-tip.el --- tooltips and click targets for hit-tested data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.1).  Vega-Lite's encoding.tooltip and
;; encoding.href, answered for whatever datum a pixel hits:
;;
;;   discrete marks  the item's own :tooltip and :href, compiled into the
;;                   scene (and so into SVG :map areas and text help-echo)
;;   series          a line or area item is one path per series, so the
;;                   datum comes from the hit index (a bisect, i.e. the
;;                   inverted x scale) and its tooltip is encoded from
;;                   the unit's encoding in the view's compile plan
;;
;; `eas-tip-click-target' decides whether an event is a click (a click
;; event, or a press released within the click slop) and what it landed
;; on.  Everything here is pure: scene, plan and state in, data out.

;;; Code:

(require 'eas-core)
(require 'eas-hit)
(require 'eas-encode)
(require 'eas-params)
(require 'eas-reduce)

(defconst eas-tip-series-marks '("line" "area" "trail")
  "Marks drawn as one path per series; they hit-test through the scene index.")

(defun eas-tip--mark (scene view-id mark-id)
  "Mark MARK-ID of SCENE's view VIEW-ID."
  (when-let* ((view (seq-find (lambda (v) (equal (plist-get v :id) view-id)) (plist-get scene :views))))
    (seq-find (lambda (m) (equal (plist-get m :id) mark-id)) (plist-get view :marks))))

(defun eas-tip--unit (plan mark-id)
  "The compile-plan unit behind scene mark MARK-ID, or nil."
  (cl-loop for group in (plist-get plan :groups)
           thereis (cl-loop for unit in (plist-get group :units)
                            for k from 0
                            when (equal (or (plist-get unit :name) (format "%s/%d" (plist-get group :id) k))
                                        mark-id)
                            return unit)))

(defun eas-tip--item (scene hit)
  "The scene item HIT points at, or nil once a recompile dropped it."
  (when-let* ((mark (eas-tip--mark scene (plist-get hit :view) (plist-get hit :mark)))
              (items (plist-get mark :items)))
    (and (< (plist-get hit :item) (length items)) (aref items (plist-get hit :item)))))

(defun eas-tip-tooltip (scene plan hit)
  "Tooltip of HIT (an `eas-hit' result) as [(:title T :value V) ...], or nil.
A discrete item's compiled tooltip wins; a series datum is encoded from
PLAN's unit encoding, since its item is the whole series."
  (when hit
    (or (plist-get (eas-tip--item scene hit) :tooltip)
        (when-let* ((unit (eas-tip--unit plan (plist-get hit :mark))))
          (eas-encode-tooltip (plist-get unit :encoding) (plist-get unit :mark) (plist-get hit :row))))))

(defun eas-tip-href (scene plan hit)
  "The href (a string) HIT links to, or nil."
  (when hit
    (or (plist-get (eas-tip--item scene hit) :href)
        (when-let* ((unit (eas-tip--unit plan (plist-get hit :mark)))
                    (def (plist-get (plist-get unit :encoding) :href))
                    (href (eas-encode-raw def (plist-get hit :row))))
          (and (stringp href) href)))))

(defun eas-tip-text (tooltip)
  "TOOLTIP pairs as \"title: value\" lines, or nil when there are none."
  (when (> (length tooltip) 0)
    (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tooltip "\n")))

(defun eas-tip-series-p (scene hit)
  "Non-nil when HIT is on a line or area mark of SCENE."
  (member (plist-get (eas-tip--mark scene (plist-get hit :view) (plist-get hit :mark)) :mark)
          eas-tip-series-marks))

(defun eas-tip--tolerance (scene mark)
  "Pixels a click may miss MARK of SCENE by and still hit it."
  (cond ((member (plist-get mark :mark) eas-tip-series-marks) eas-reduce-hover-radius)
        ((equal (plist-get scene :target) "text")
         ;; PX is the clicked cell's centre: anything touching the cell.
         (let ((cell (plist-get (plist-get scene :size) :cell)))
           (max eas-reduce-click-slop (/ (sqrt (+ (expt (aref cell 0) 2) (expt (aref cell 1) 2))) 2.0))))
        (t eas-reduce-click-slop)))

(defun eas-tip-hit (scene px)
  "The datum a click at PX lands on in SCENE, or nil.
PX must be inside a view's plot.  Each mark is hit within its own
tolerance (`eas-tip--tolerance'): `eas-reduce-click-slop' pixels
for discrete marks, `eas-reduce-hover-radius' for series; the
nearest such hit wins.  The result is shaped like `eas-hit''s."
  (when-let* ((view (seq-find (lambda (v) (eas-hit--contains (plist-get v :bounds) (aref px 0) (aref px 1)))
                              (plist-get scene :views))))
    (let (best)
      (seq-doseq (mark (plist-get view :marks))
        (unless (plist-get mark :interactive-off)
          (when-let* ((c (eas-hit-mark mark (aref px 0) (aref px 1))))
            (when (and (<= (plist-get c :distance) (eas-tip--tolerance scene mark))
                       (or (null best) (< (plist-get c :distance) (plist-get best :distance))))
              (setq best (append c (list :row (aref (plist-get mark :rows) (plist-get c :datum)))))))))
      (when best (append (list :view (plist-get view :id)) best)))))

(defun eas-tip-click-px (event old-state)
  "Pixel EVENT clicks at, given the view state OLD-STATE before it, or nil.
A click event clicks; so does a pointerup ending a press that moved no
more than `eas-reduce-click-slop' pixels (the reducer's own rule)."
  (pcase (plist-get event :type)
    ("click" (plist-get event :px))
    ("pointerup"
     (when-let* ((drag (plist-get old-state :drag)))
       (let ((start (plist-get drag :start)) (px (plist-get event :px)))
         (unless (or (plist-get drag :moved)
                     (> (+ (abs (- (aref px 0) (aref start 0))) (abs (- (aref px 1) (aref start 1))))
                        eas-reduce-click-slop))
           px))))))

(defun eas-tip-click-target (event old-state scene plan)
  "What EVENT clicked in SCENE (compiled from PLAN) under OLD-STATE, or nil.
Returns (:view :mark :datum :row :px :tooltip :href), JSON-ready."
  (when-let* ((px (eas-tip-click-px event old-state))
              (hit (eas-tip-hit scene px)))
    (list :view (plist-get hit :view) :mark (plist-get hit :mark) :datum (plist-get hit :datum)
          :row (eas--plist-without (plist-get hit :row) eas-params-row-key)
          :px px
          :tooltip (or (eas-tip-tooltip scene plan hit) :null)
          :href (or (eas-tip-href scene plan hit) :null))))

(provide 'eas-tip)
;;; eas-tip.el ends here
