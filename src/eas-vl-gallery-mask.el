;;; eas-vl-gallery-mask.el --- oracle masks for platform-dependent pixels -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Some pixels of a reference depend on the machine that built it, not
;; on Vega: emoji glyphs come from the platform's color emoji font
;; (bin/chart's references were built with Apple Color Emoji).  A
;; gallery example whose status.json entry names a mask ("mask":
;; "emoji") is compared with those regions painted over with each
;; image's background, in both images.  The regions come from the
;; native scene, so a glyph the reference draws elsewhere still counts
;; as a difference: positions stay checked, glyph pixels do not.

;;; Code:

(require 'eas-core)

(defconst eas-vl-gallery-mask-em 1.25
  "Width and height of a masked emoji glyph, in em (Apple Color Emoji
glyphs are 1.0-1.2em wide).")

(defun eas-vl-gallery-mask--emoji-p (text)
  "Non-nil when TEXT holds a pictographic (emoji) character."
  (and (stringp text)
       (seq-some (lambda (c) (or (<= #x1F000 c #x1FAFF) (<= #x2600 c #x27BF))) text)))

(defun eas-vl-gallery-mask--text-box (item)
  "Box [X Y W H] covering the glyphs of text ITEM, by its align and baseline."
  (let* ((fs (or (plist-get item :fontSize) 11))
         (w (* eas-vl-gallery-mask-em fs (length (plist-get item :text))))
         (h (* eas-vl-gallery-mask-em fs))
         (x (plist-get item :x)) (y (plist-get item :y)))
    (vector (pcase (plist-get item :align) ("left" x) ("right" (- x w)) (_ (- x (/ w 2))))
            (pcase (plist-get item :baseline)
              ("top" y) ("middle" (- y (/ h 2))) (_ (- y (* 0.9 h))))
            w h)))

(defun eas-vl-gallery-mask-boxes (kind scene)
  "Boxes [X Y W H] of SCENE that mask KIND (\"emoji\") hides."
  (pcase kind
    ("emoji"
     (let (out)
       (seq-doseq (view (plist-get scene :views))
         (seq-doseq (mark (plist-get view :marks))
           (when (equal (plist-get mark :mark) "text")
             (seq-doseq (item (plist-get mark :items))
               (when (and (numberp (plist-get item :x)) (numberp (plist-get item :y))
                          (eas-vl-gallery-mask--emoji-p (plist-get item :text)))
                 (push (eas-vl-gallery-mask--text-box item) out))))))
       (nreverse out)))
    (_ (eas-signal "INVALID_INPUT" (format "Unknown oracle mask %S; the masks are: emoji" kind)
                   :path "mask"))))

(defun eas-vl-gallery-mask-image (img boxes)
  "Decoded IMG (`eas-png-read') with BOXES painted in its background."
  (if (null boxes) img
    (let* ((w (plist-get img :w)) (h (plist-get img :h))
           (s (copy-sequence (plist-get img :rgba)))
           (bg (substring s 0 4)))
      (dolist (b boxes)
        (let ((x0 (max 0 (floor (aref b 0)))) (x1 (min w (ceiling (+ (aref b 0) (aref b 2)))))
              (y0 (max 0 (floor (aref b 1)))) (y1 (min h (ceiling (+ (aref b 1) (aref b 3))))))
          (when (< x0 x1)
            (let ((row (apply #'concat (make-list (- x1 x0) bg))))
              (cl-loop for y from y0 below y1
                       do (store-substring s (* 4 (+ (* y w) x0)) row))))))
      (list :w w :h h :rgba s))))

(provide 'eas-vl-gallery-mask)
;;; eas-vl-gallery-mask.el ends here
