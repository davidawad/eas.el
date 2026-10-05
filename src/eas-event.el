;;; eas-event.el --- event/v1: interactions as data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  Everything that changes a view is an event/v1 object,
;; whether a mouse, a key, a timer or an agent sent it:
;;
;;   {"type": "pointermove", "px": [X, Y], "view"?: SCENE-VIEW}
;;   {"type": "pointerdown" | "pointerup" | "click" | "dblclick", "px": [X, Y]}
;;   {"type": "pointerleave"}
;;   {"type": "wheel", "px": [X, Y], "delta": N}      (N < 0 zooms in)
;;   {"type": "drag", "from": [X, Y], "to": [X, Y]}
;;   {"type": "brush", "param"?: NAME, "x"?: [LO, HI], "y"?: [LO, HI]}
;;   {"type": "key", "key": "+" | "-" | "0" | "left" | "right" | "up" | "down"
;;                        | "escape" | "[" | "]" | "z"}   (z zooms into the brush)
;;   {"type": "push", "rows": [ROW, ...], "window"?: N}  (keep the last N rows)
;;
;; Pixel coordinates are scene pixels.  `eas-event-parse' validates
;; and signals EVENT_INVALID naming the offending field.

;;; Code:

(require 'eas-core)

(defconst eas-event-types
  '("pointermove" "pointerdown" "pointerup" "pointerleave" "click" "dblclick"
    "wheel" "drag" "brush" "key" "push")
  "event/v1 types.")

(defconst eas-event-keys '("+" "=" "-" "0" "left" "right" "up" "down" "escape" "[" "]" "z")
  "Keys the runtime understands.")

(defun eas-event--invalid (field message)
  "Signal EVENT_INVALID for FIELD with MESSAGE."
  (eas-signal "EVENT_INVALID" message :field field))

(defun eas-event--point-p (v)
  "Non-nil when V is a [X Y] pair of numbers."
  (and (vectorp v) (= (length v) 2) (numberp (aref v 0)) (numberp (aref v 1))))

(defun eas-event-parse (event)
  "Validate EVENT (JSON string or plist); return it as a plist."
  (let* ((event (if (stringp event) (eas-json-parse event) event))
         (type (plist-get event :type)))
    (unless (and (eas-object-p event) event)
      (eas-event--invalid "type" "An event is a JSON object with a type"))
    (unless (member type eas-event-types)
      (eas-event--invalid "type" (format "Unknown event type %S; types: %s" type
                                           (string-join eas-event-types ", "))))
    (pcase type
      ((or "pointermove" "pointerdown" "pointerup" "click" "dblclick" "wheel")
       (unless (eas-event--point-p (plist-get event :px))
         (eas-event--invalid "px" (format "%s needs px: [x, y] in scene pixels" type)))
       (when (and (equal type "wheel") (not (numberp (plist-get event :delta))))
         (eas-event--invalid "delta" "wheel needs a numeric delta (negative zooms in)")))
      ("drag" (dolist (f '(:from :to))
                (unless (eas-event--point-p (plist-get event f))
                  (eas-event--invalid (eas-key-name f) "drag needs from and to as [x, y]"))))
      ("brush" (unless (or (plist-get event :x) (plist-get event :y))
                 (eas-event--invalid "x" "brush needs x and/or y as [lo, hi] in data space"))
               (dolist (f '(:x :y))
                 (let ((r (plist-get event f)))
                   (when (and r (not (and (vectorp r) (= (length r) 2))))
                     (eas-event--invalid (eas-key-name f) "brush ranges are [lo, hi]")))))
      ("key" (unless (member (plist-get event :key) eas-event-keys)
               (eas-event--invalid "key" (format "Unknown key %S; keys: %s" (plist-get event :key)
                                                   (string-join eas-event-keys " ")))))
      ("push" (unless (or (vectorp (plist-get event :rows)) (listp (plist-get event :rows)))
                (eas-event--invalid "rows" "push needs rows: [{...}, ...]"))
              (let ((window (plist-get event :window)))
                (when (and window (not (and (natnump window) (> window 0))))
                  (eas-event--invalid "window" "push window is a positive integer: the rows to keep")))))
    event))

(defun eas-event-describe (event)
  "One line describing EVENT for logs and agents."
  (pcase (plist-get event :type)
    ("brush" (format "brush %s" (string-join
                                 (delq nil (mapcar (lambda (f) (when-let* ((r (plist-get event f)))
                                                                 (format "%s %s..%s" (eas-key-name f) (aref r 0) (aref r 1))))
                                                   '(:x :y)))
                                 " ")))
    ("key" (format "key %s" (plist-get event :key)))
    ("push" (format "push %d rows%s" (length (plist-get event :rows))
                    (if-let* ((w (plist-get event :window))) (format " (window %d)" w) "")))
    ("drag" (format "drag %s -> %s" (plist-get event :from) (plist-get event :to)))
    (type (if (plist-get event :px) (format "%s at %s" type (plist-get event :px)) type))))

(provide 'eas-event)
;;; eas-event.el ends here
