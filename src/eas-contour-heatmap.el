;;; eas-contour-heatmap.el --- heatmap images of value grids -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  The "heatmap" domain transform is Vega's heatmap: each
;; grid (kde2d's field, or tidy x/y/value rows) becomes a raster image,
;; one pixel per cell of its x1..x2 by y1..y2 part, as a PNG data: URL
;; the image mark draws (stretched over the plot, smoothed, as Vega's
;; canvas does).  A pixel's color is a constant, a continuous scheme
;; at value / max, or a categorical color per row (by a field, as a
;; nominal color scale would pick it); its opacity is a constant, or
;; value / max by default.  resolve "shared" takes max over every grid.
;;
;; `eas-contour-png' encodes the image natively: 8-bit RGBA, stored
;; (uncompressed) deflate blocks, so no compressor is needed.

;;; Code:

(require 'eas-core)
(require 'eas-scheme)
(require 'eas-scale)
(require 'eas-color-names)
(require 'eas-contour-grid)
(require 'eas-transform-domain)

;;; PNG

(defconst eas-contour--crc-table
  (let ((table (make-vector 256 0)))
    (dotimes (n 256)
      (let ((c n))
        (dotimes (_ 8) (setq c (if (= (logand c 1) 1) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (aset table n c)))
    table)
  "CRC-32 of every byte, for PNG chunks.")

(defun eas-contour--crc32 (s)
  "CRC-32 of unibyte string S."
  (let ((c #xffffffff))
    (dotimes (i (length s))
      (setq c (logxor (aref eas-contour--crc-table (logand (logxor c (aref s i)) #xff)) (ash c -8))))
    (logxor c #xffffffff)))

(defun eas-contour--u32 (n)
  "N as four big-endian bytes."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255) (logand (ash n -8) 255) (logand n 255)))

(defun eas-contour--chunk (type data)
  "A PNG chunk of TYPE holding DATA."
  (let ((body (concat type data)))
    (concat (eas-contour--u32 (length data)) body (eas-contour--u32 (eas-contour--crc32 body)))))

(defun eas-contour--zlib-stored (raw)
  "RAW wrapped as a zlib stream of stored deflate blocks."
  (let ((a 1) (b 0) (n (length raw)) (blocks nil) (i 0))
    (dotimes (k n) (setq a (% (+ a (aref raw k)) 65521) b (% (+ b a) 65521)))
    (while (or (< i n) (and (= n 0) (null blocks)))
      (let* ((len (min 65535 (- n i))) (last (>= (+ i len) n)))
        (push (concat (unibyte-string (if last 1 0) (logand len 255) (ash len -8)
                                      (logand (lognot len) 255) (logand (ash (lognot len) -8) 255))
                      (substring raw i (+ i len)))
              blocks)
        (setq i (+ i len))))
    (concat (unibyte-string #x78 #x01) (apply #'concat (nreverse blocks)) (eas-contour--u32 (logior (ash b 16) a)))))

(defun eas-contour-png (w h rgba)
  "A PNG file (unibyte string) of W x H pixels from RGBA (W*H*4 bytes)."
  (let ((raw (make-string (* h (1+ (* 4 w))) 0)))
    (dotimes (j h)
      (store-substring raw (+ 1 (* j (1+ (* 4 w)))) (substring rgba (* j 4 w) (* (1+ j) 4 w))))
    (concat "\x89PNG\r\n\x1a\n"
            (eas-contour--chunk "IHDR" (concat (eas-contour--u32 w) (eas-contour--u32 h) (unibyte-string 8 6 0 0 0)))
            (eas-contour--chunk "IDAT" (eas-contour--zlib-stored raw))
            (eas-contour--chunk "IEND" ""))))

(defun eas-contour-png-url (w h rgba)
  "A data: URL of the PNG of W x H RGBA pixels."
  (concat "data:image/png;base64," (base64-encode-string (eas-contour-png w h rgba) t)))

;;; Colors

(defun eas-contour--rgb (color)
  "CSS COLOR as a list of three bytes."
  (let ((hex (eas-color-hex color)))
    (unless (and (stringp hex) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" hex))
      (eas-signal "INVALID_INPUT" (format "heatmap color %S is not a CSS color" color) :path "/color"))
    (list (string-to-number (substring hex 1 3) 16) (string-to-number (substring hex 3 5) 16)
          (string-to-number (substring hex 5 7) 16))))

(defun eas-contour--ramp (scheme)
  "A function of [0, 1] to the RGB of continuous SCHEME, sampled 256 times."
  (let* ((stops (eas-scheme-ramp scheme))
         (table (vconcat (cl-loop for i below 256
                                  collect (eas-contour--rgb (eas-scheme-interpolate stops (/ i 255.0)))))))
    (lambda (v) (aref table (max 0 (min 255 (floor (+ 0.5 (* 255 v)))))))))

(defun eas-contour--category (color rows)
  "The function ROW -> RGB of categorical COLOR over ROWS.
COLOR is {field, scheme or range, sort}, as a nominal color scale."
  (let* ((key (eas-key (plist-get color :field)))
         (values (delete-dups (mapcar (lambda (r) (plist-get r key)) rows)))
         (sort (or (plist-get color :sort) "ascending"))
         (less (lambda (a b) (string< (format "%s" a) (format "%s" b))))
         (domain (pcase sort
                   ("ascending" (sort values less))
                   ("descending" (nreverse (sort values less)))
                   (_ values)))
         (range (or (and (vectorp (plist-get color :range)) (plist-get color :range))
                    (eas-scheme-discrete-range (or (plist-get color :scheme) "tableau10") (length domain)))))
    (lambda (row)
      (let ((i (or (cl-position (plist-get row key) domain :test #'equal) 0)))
        (eas-contour--rgb (aref range (% i (length range))))))))

;;; The transform

(defun eas-contour--raster (grid max rgb-at opacity)
  "RGBA bytes and size (W H RGBA) of GRID's drawn part.
RGB-AT maps value / MAX to a color (a constant when it is a list) and
OPACITY is a number or nil for value / MAX."
  (let* ((n (plist-get grid :width)) (vals (plist-get grid :values))
         (x1 (or (plist-get grid :x1) 0)) (y1 (or (plist-get grid :y1) 0))
         (x2 (or (plist-get grid :x2) n)) (y2 (or (plist-get grid :y2) (plist-get grid :height)))
         (w (- x2 x1)) (h (- y2 y1))
         (rgba (make-string (* 4 w h) 0)) (k 0))
    (cl-loop for j from y1 below y2
             do (cl-loop for i from x1 below x2
                         for v = (or (aref vals (+ i (* j n))) 0.0)
                         for f = (if (> max 0) (/ v max) 0.0)
                         for c = (if (functionp rgb-at) (funcall rgb-at f) rgb-at)
                         do (aset rgba k (nth 0 c)) (aset rgba (+ k 1) (nth 1 c)) (aset rgba (+ k 2) (nth 2 c))
                         (aset rgba (+ k 3) (max 0 (min 255 (truncate (* 255 (if (numberp opacity) opacity f))))))
                         (setq k (+ k 4))))
    (list w h rgba)))

(defun eas-contour-heatmap (rows params)
  "The heatmap transform: an image of each grid in ROWS under PARAMS."
  (let* ((pairs (eas-contour-grids rows params))
         (shared (equal (plist-get params :resolve) "shared"))
         (top (and shared (apply #'max 0 (mapcar (lambda (p) (eas-contour-grid-max (cdr p))) pairs))))
         (color (plist-get params :color))
         (opacity (plist-get params :opacity))
         (by-row (and (eas-object-p color) (plist-get color :field)
                      (eas-contour--category color (mapcar #'car pairs))))
         (fixed (cond (by-row nil)
                      ((eas-object-p color) (if color (eas-contour--ramp (or (plist-get color :scheme) "viridis"))
                                              (eas-contour--rgb "#888")))
                      (t (eas-contour--rgb color))))
         (as (eas-key (plist-get params :as))))
    (vconcat
     (mapcar (lambda (p)
               (pcase-let ((`(,w ,h ,rgba) (eas-contour--raster (cdr p) (or top (eas-contour-grid-max (cdr p)))
                                                                (if by-row (funcall by-row (car p)) fixed)
                                                                (and (numberp opacity) opacity))))
                 (append (car p) (list as (eas-contour-png-url w h rgba)))))
             pairs))))

;; Per-pixel loops, compiled when the source is loaded interpreted.
(dolist (f '(eas-contour--crc32 eas-contour--zlib-stored eas-contour-png eas-contour--raster))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(eas-register-transform
 "heatmap"
 :doc "Vega's heatmap: a PNG data: URL image of each value grid, for the image mark."
 :schema '(:field (:type "string" :doc "field holding a grid (kde2d's); else rows are x/y/value cells")
           :x (:type "string" :default "x" :doc "cell x field of tidy grid rows")
           :y (:type "string" :default "y" :doc "cell y field of tidy grid rows")
           :value (:type "string" :default "value" :doc "cell value field of tidy grid rows")
           :groupby (:type "array" :default [] :doc "fields making one grid per group of tidy rows")
           :color (:type "any" :default "#888"
                   :doc "a CSS color, {scheme} at value/max, or {field, scheme|range, sort} per row")
           :opacity (:type "number" :doc "pixel opacity; else value/max")
           :resolve (:type "string" :default "independent" :doc "shared: max over every grid")
           :as (:type "string" :default "image" :doc "output image URL field"))
 :fn #'eas-contour-heatmap)

(provide 'eas-contour-heatmap)
;;; eas-contour-heatmap.el ends here
