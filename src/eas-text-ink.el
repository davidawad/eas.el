;;; eas-text-ink.el --- legible text colors on the terminal's background -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (fc-qx1.51).  A scene's colors are chosen
;; for the theme's light paper; a text frame draws on whatever the
;; Emacs default face's background is.  Every foreground color the text
;; renderer emits goes through `eas-text-ink-legible':
;;
;;   - a color with WCAG contrast >= `eas-text-ink-min-contrast' against
;;     the background is kept as the spec or theme gave it;
;;   - a neutral one (the theme's ink, a spec's "black") is ink: the
;;     default face's foreground, light on a dark background;
;;   - a hued one keeps its hue and saturation and moves its lightness
;;     away from the background until it is legible.
;;
;; `eas-text-background-mode' picks light or dark (nil: the frame's
;; `background-mode'); the colors themselves come from the default face
;; when it has real ones, else from `eas-text-ink-fallbacks'.

;;; Code:

(require 'color)
(require 'eas-core)
(require 'eas-color)
(require 'eas-color-names)

(defcustom eas-text-background-mode nil
  "Background the text renderer draws for: `light', `dark' or nil (the
selected frame's `background-mode')."
  :type '(choice (const :tag "From the frame" nil) (const light) (const dark))
  :group 'eas)

(defconst eas-text-ink-fallbacks
  '((light :background "#ffffff" :foreground "#000000")
    (dark :background "#282828" :foreground "#ebdbb2"))
  "Background and ink per mode when the default face has no real colors
\(batch, or a terminal's own unspecified colors).")

(defconst eas-text-ink-min-contrast 3.0
  "Least WCAG contrast ratio a text mark's color keeps against the background.")

(defun eas-text-ink--rgb (color)
  "COLOR (CSS hex, name or rgb()/rgba(), or an Emacs color name) as (R G B)
in 0..1, or nil.  CSS colors parse exactly; `color-name-to-rgb' would
round them to a batch terminal's palette."
  (cond ((eas-color-hex color) (mapcar (lambda (v) (/ v 255.0)) (eas-color--hex-rgb (eas-color-hex color))))
        ((and (stringp color)
              (string-match "\\`rgba?(\\s-*\\([0-9.]+\\)\\s-*,\\s-*\\([0-9.]+\\)\\s-*,\\s-*\\([0-9.]+\\)" color))
         (mapcar (lambda (k) (/ (min 255.0 (string-to-number (match-string k color))) 255.0)) '(1 2 3)))
        ((and (stringp color) (not noninteractive) (not (equal color "transparent"))) (color-name-to-rgb color))))

(defun eas-text-ink-mode ()
  "The background mode text is drawn for: `light' or `dark'."
  (or eas-text-background-mode
      (and (eq (frame-parameter nil 'background-mode) 'dark) 'dark)
      'light))

(defun eas-text-ink--face-color (attribute mode)
  "The default face's ATTRIBUTE color when it is real and fits MODE."
  (let ((c (if (eq attribute :background) (face-background 'default nil t) (face-foreground 'default nil t))))
    (and (stringp c) (not (string-prefix-p "unspecified" c)) (eas-text-ink--rgb c)
         ;; A face from another mode (a let-bound mode in batch) does not count.
         (eq (eq attribute :background)
             (eq (if (> (eas-text-ink-luminance c) 0.18) 'light 'dark) mode))
         c)))

(defun eas-text-ink-background (&optional mode)
  "The background color text is drawn on for MODE (default `eas-text-ink-mode')."
  (let ((mode (or mode (eas-text-ink-mode))))
    (or (and (null noninteractive) (eas-text-ink--face-color :background mode))
        (plist-get (alist-get mode eas-text-ink-fallbacks) :background))))

(defun eas-text-ink-foreground (&optional mode)
  "The ink (default foreground) for MODE (default `eas-text-ink-mode')."
  (let ((mode (or mode (eas-text-ink-mode))))
    (or (and (null noninteractive)
             (let ((c (face-foreground 'default nil t)))
               (and (stringp c) (not (string-prefix-p "unspecified" c)) (eas-text-ink--rgb c)
                    (>= (eas-text-ink-contrast c (eas-text-ink-background mode)) eas-text-ink-min-contrast)
                    c)))
        (plist-get (alist-get mode eas-text-ink-fallbacks) :foreground))))

(defun eas-text-ink-luminance (color)
  "WCAG relative luminance of COLOR (a name or #hex)."
  (let ((rgb (or (eas-text-ink--rgb color) '(0 0 0))))
    (apply #'+ (cl-mapcar (lambda (c w) (* w (if (<= c 0.03928) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))))
                          rgb '(0.2126 0.7152 0.0722)))))

(defun eas-text-ink-contrast (a b)
  "WCAG contrast ratio of colors A and B (1 to 21)."
  (let ((la (eas-text-ink-luminance a)) (lb (eas-text-ink-luminance b)))
    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))

(defvar eas-text-ink--memo (make-hash-table :test 'equal)
  "(COLOR BACKGROUND INK) -> legible color.")

(defun eas-text-ink--adjust (color bg ink)
  "COLOR made legible on BG; neutral colors become INK."
  (let* ((rgb (eas-text-ink--rgb color))
         (hsl (apply #'color-rgb-to-hsl rgb))
         (dark-bg (< (eas-text-ink-luminance bg) 0.18)))
    (if (< (* (nth 1 hsl) (- 1 (abs (- (* 2 (nth 2 hsl)) 1)))) 0.12)
        ink
      (let ((l (nth 2 hsl)) (out color))
        (while (and (< (eas-text-ink-contrast out bg) eas-text-ink-min-contrast) (< 0 l 1))
          (setq l (max 0.0 (min 1.0 (+ l (if dark-bg 0.02 -0.02))))
                out (apply #'color-rgb-to-hex (append (color-hsl-to-rgb (nth 0 hsl) (nth 1 hsl) l) '(2)))))
        out))))

(defvar eas-text-ink--colors nil
  "(BACKGROUND . INK) while a scene renders (`eas-text-ink-with').")

(defmacro eas-text-ink-with (&rest body)
  "Run BODY with the background and ink looked up once."
  (declare (indent 0))
  `(let ((eas-text-ink--colors (cons (eas-text-ink-background) (eas-text-ink-foreground))))
     ,@body))

(defun eas-text-ink-legible (color &optional mode)
  "COLOR as the text renderer draws it for MODE (default `eas-text-ink-mode').
Nil for \"transparent\" (the cell keeps the default face); other unknown
color names are returned as they are."
  (if (not (eas-text-ink--rgb color)) (unless (equal color "transparent") color)
    (let* ((colors (if (and eas-text-ink--colors (null mode)) eas-text-ink--colors
                     (cons (eas-text-ink-background mode) (eas-text-ink-foreground mode))))
           (bg (car colors)) (ink (cdr colors))
           (key (list color bg ink)))
      (or (gethash key eas-text-ink--memo)
          (puthash key (if (>= (eas-text-ink-contrast color bg) eas-text-ink-min-contrast) color
                         (eas-text-ink--adjust color bg ink))
                   eas-text-ink--memo)))))

(defun eas-text-ink-shade (&optional mode)
  "Background of brushed cells for MODE: a quarter of the way from the
background to the ink, so glyphs on it stay readable in either mode."
  (let* ((colors (if (and eas-text-ink--colors (null mode)) eas-text-ink--colors
                   (cons (eas-text-ink-background mode) (eas-text-ink-foreground mode))))
         (bg (eas-text-ink--rgb (car colors))) (ink (eas-text-ink--rgb (cdr colors))))
    (apply #'format "#%02x%02x%02x" (cl-mapcar (lambda (b i) (round (* 255 (+ b (* 0.25 (- i b)))))) bg ink))))

(provide 'eas-text-ink)
;;; eas-text-ink.el ends here
