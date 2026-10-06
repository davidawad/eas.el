;;; eas-independent.el --- independent scales and axes across layers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A layer shares one scale per channel unless its
;; resolve.scale makes the channel "independent" (a dual-axis chart).
;; Then each layer drawing on that channel gets its own scale: the
;; first stays under the channel's key, the next ones are kept in the
;; view's scales as :y_1, :y_2 ... (:x_1 ...), each with its own axis,
;; on the opposite side (right or top) and without a grid, as
;; Vega-Lite draws them.  `eas-independent-unit-scales' gives a layer
;; the scales its marks are drawn with.

;;; Code:

(require 'eas-core)
(require 'eas-compile-scales)

(defun eas-independent-channels (group)
  "Positional channels that GROUP's resolve declares independent."
  (let ((scale (plist-get (plist-get group :resolve) :scale)))
    (seq-filter (lambda (ch) (equal (plist-get scale ch) "independent")) '(:x :y))))

(defun eas-independent-key (channel k)
  "The view-scale key of extra scale K (from 1) for CHANNEL."
  (intern (format "%s_%d" channel k)))

(defun eas-independent-scales (group zoom)
  "Give each layer of GROUP its own scale on independent channels.
ZOOM is the view's zoomed domains.  Adds the extra scales and their
axis definitions to GROUP and the mapping to each unit."
  (dolist (ch (eas-independent-channels group))
    (let* ((units (seq-filter (lambda (u) (eas-compile--defs (list u) ch)) (plist-get group :units)))
           (k 0))
      (when (cdr units)
        (dolist (u units)
          (let ((scale (eas-compile-position-scale (list u) ch (plist-get zoom ch)))
                (def (cdar (eas-compile--defs (list u) ch))))
            (if (zerop k)
                (progn
                  (plist-put group :scales (plist-put (plist-get group :scales) ch scale))
                  (plist-put group :axis-defs (plist-put (plist-get group :axis-defs) ch def)))
              (let* ((key (eas-independent-key ch k))
                     (axis (let ((a (plist-get def :axis))) (if (eas-object-p a) a nil))))
                (plist-put group :scales (append (plist-get group :scales) (list key scale)))
                (unless (memq (plist-get def :axis) '(:null :false))
                  (unless (plist-get axis :orient)
                    (setq axis (eas-plist-put axis :orient (if (eq ch :x) "top" "right"))))
                  (unless (plist-member axis :grid) (setq axis (eas-plist-put axis :grid :false)))
                  (plist-put group :extra-axes
                             (append (plist-get group :extra-axes) (list (cons key (eas-plist-put def :axis axis))))))
                (plist-put u :scale-keys (append (plist-get u :scale-keys) (list ch key)))))
            (setq k (1+ k))))))))

(defun eas-independent-ranges (group)
  "Map GROUP's extra scales onto its placed plot."
  (let ((x0 (plist-get group :x0)) (y0 (plist-get group :y0))
        (w (plist-get group :w)) (h (plist-get group :h)) (scales (plist-get group :scales)))
    (dolist (pair (plist-get group :extra-axes))
      (let* ((key (car pair)) (s (plist-get scales key))
             (x (string-prefix-p ":x" (symbol-name key))))
        (when s
          (setq scales (plist-put scales key
                                  (eas-compile-set-range
                                   s (cond (x (vector x0 (+ x0 w)))
                                           ((member (plist-get s :type) '("band" "point")) (vector y0 (+ y0 h)))
                                           (t (vector (+ y0 h) y0)))))))))
    ;; Scales of layers whose axis is hidden still need their range.
    (dolist (u (plist-get group :units))
      (cl-loop for (ch key) on (plist-get u :scale-keys) by #'cddr
               unless (assq key (plist-get group :extra-axes))
               do (when-let* ((s (plist-get scales key)))
                    (setq scales (plist-put scales key
                                            (eas-compile-set-range
                                             s (if (eq ch :x) (vector x0 (+ x0 w)) (vector (+ y0 h) y0))))))))
    (plist-put group :scales scales)))

(defun eas-independent-unit-scales (group unit)
  "The scales UNIT of GROUP draws with."
  (let ((scales (plist-get group :scales)) (keys (plist-get unit :scale-keys)))
    (if (null keys) scales
      (let ((out (copy-sequence scales)))
        (cl-loop for (ch key) on keys by #'cddr
                 do (setq out (plist-put out ch (plist-get scales key))))
        out))))

(defun eas-independent-local-scale (group key)
  "GROUP's scale KEY mapped onto its plot with the origin at 0,0, or nil."
  (when-let* ((s (plist-get (plist-get group :scales) key)))
    (eas-compile-set-range s (if (or (string-prefix-p ":x" (symbol-name key))
                                     (member (plist-get s :type) '("band" "point")))
                                 (vector 0 (if (string-prefix-p ":x" (symbol-name key))
                                               (plist-get group :w) (plist-get group :h)))
                               (vector (plist-get group :h) 0)))))

(provide 'eas-independent)
;;; eas-independent.el ends here
