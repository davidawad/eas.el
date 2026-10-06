;;; eas-contour-text.el --- path symbols and raster images on the character grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5 (text).  Two things the character grid draws cell by
;; cell instead of as one glyph, used by eas-text.el when this file is
;; loaded:
;;
;; - a point whose shape is SVG path data (geopath's contours and map
;;   shapes) fills every cell whose centre the path holds, by the
;;   nonzero rule, as SVG fills it: `eas-contour-text-path-cells';
;; - an image whose url is PNG data (heatmap's) colors each cell with
;;   the pixel under its centre: `eas-contour-text-image-cells'.
;;
;; Paths take M, L, H, V and Z, absolute or relative, which is what
;; geopath writes.  A path with curves or arcs is a symbol icon (an
;; isotype's), which keeps its single glyph.

;;; Code:

(require 'eas-core)
(require 'eas-png)

;;; Paths

(defun eas-contour-text--path-rings (d)
  "The subpaths of path data D as lists of (X . Y), or nil when D is no path.
nil too when D draws curves or arcs: a symbol icon, not geopath's."
  (when (and (stringp d) (string-match-p "\\`[ \t]*[Mm]" d)
             (not (string-match-p "[^MmLlHhVvZzEe0-9.,+ \t\n-]" d)))
    (let ((tokens nil) (pos 0) rings ring (x 0.0) (y 0.0) cmd)
      (while (string-match "[MmLlHhVvZz]\\|[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?" d pos)
        (let ((tok (match-string 0 d)))
          (push (if (string-match-p "\\`[A-Za-z]\\'" tok) (aref tok 0) (string-to-number tok)) tokens))
        (setq pos (match-end 0)))
      (setq tokens (nreverse tokens))
      (cl-flet ((close () (when (cdr ring) (push (nreverse ring) rings)) (setq ring nil)))
        (while tokens
          (when (characterp (car tokens)) (setq cmd (pop tokens)))
          (pcase cmd
            ((or ?Z ?z) (when ring (setq x (car (car (last ring))) y (cdr (car (last ring))))) (close)
             ;; Numbers after a close draw on from the start point.
             (setq cmd (if (eq cmd ?Z) ?L ?l)))
            ((or ?M ?m ?L ?l)
             (let ((a (pop tokens)) (b (pop tokens)))
               (if (memq cmd '(?m ?l)) (setq x (+ x a) y (+ y b)) (setq x (float a) y (float b)))
               (when (memq cmd '(?M ?m)) (close) (setq cmd (if (eq cmd ?M) ?L ?l)))
               (push (cons x y) ring)))
            ((or ?H ?h) (let ((a (pop tokens))) (setq x (if (eq cmd ?h) (+ x a) (float a))) (push (cons x y) ring)))
            ((or ?V ?v) (let ((a (pop tokens))) (setq y (if (eq cmd ?v) (+ y a) (float a))) (push (cons x y) ring)))
            (_ (pop tokens))))
        (close))
      (nreverse rings))))

(defun eas-contour-text--outline (edges cw ch)
  "Cells (COL . ROW) along EDGES ((A . B) pixel point pairs) of CW x CH cells."
  (let ((seen (make-hash-table :test 'equal)) cells)
    (dolist (e edges)
      (let* ((a (car e)) (b (cdr e))
             (n (max 1 (ceiling (/ (sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cdr b) (cdr a)) 2)))
                                   (* 0.5 (min cw ch)))))))
        (dotimes (k (1+ n))
          (let* ((f (/ k (float n)))
                 (c (cons (floor (+ (car a) (* f (- (car b) (car a)))) cw)
                          (floor (+ (cdr a) (* f (- (cdr b) (cdr a)))) ch))))
            (unless (or (gethash c seen) (< (car c) 0) (< (cdr c) 0))
              (puthash c t seen) (push c cells))))))
    (nreverse cells)))

(defun eas-contour-text-path-cells (shape x y size cw ch &optional outline)
  "Return the cells (COL . ROW) whose centres the path SHAPE covers, or nil.
SHAPE is drawn as a symbol of area SIZE at pixel X Y (scaled by
sqrt(SIZE) / 2), on cells CW x CH pixels.  OUTLINE gives the cells its
edges cross instead, for a path stroked but not filled.  nil when SHAPE
is not path data, `empty' when it covers no cell centre."
  (when-let* ((rings (eas-contour-text--path-rings shape)))
    (let* ((s (/ (sqrt (max 0 size)) 2.0))
           (edges nil) (all nil) (y0 1.0e+INF) (y1 -1.0e+INF) cells)
      (dolist (ring rings)
        (let ((pts (mapcar (lambda (p) (cons (+ x (* s (car p))) (+ y (* s (cdr p))))) ring)))
          (cl-loop for (a . rest) on pts
                   for b = (or (car rest) (car pts))
                   do (setq y0 (min y0 (cdr a)) y1 (max y1 (cdr a)))
                   do (push (cons a b) all)
                   unless (= (cdr a) (cdr b)) do (push (cons a b) edges))))
      (if outline
          (setq cells (nreverse (eas-contour-text--outline (nreverse all) cw ch)))
       (cl-loop for row from (max 0 (floor y0 ch)) to (floor y1 ch)
               for cy = (* (+ row 0.5) ch)
               for xs = (sort (cl-loop for (a . b) in edges
                                       when (or (and (<= (cdr a) cy) (< cy (cdr b))) (and (<= (cdr b) cy) (< cy (cdr a))))
                                       collect (cons (+ (car a) (/ (* (- cy (cdr a)) (- (car b) (car a))) (- (cdr b) (cdr a))))
                                                     (if (< (cdr a) (cdr b)) 1 -1)))
                              (lambda (p q) (< (car p) (car q))))
               do (let ((wind 0) (start nil))
                    (dolist (c xs)
                      (let ((before wind))
                        (setq wind (+ wind (cdr c)))
                        (cond ((and (= before 0) (/= wind 0)) (setq start (car c)))
                              ((and (/= before 0) (= wind 0))
                               (cl-loop for col from (max 0 (ceiling (- (/ start cw) 0.5))) to (floor (- (/ (car c) cw) 0.5))
                                        do (push (cons col row) cells)))))))))
      (or (nreverse cells) 'empty))))

;;; Images

(defun eas-contour-text--png-data (url)
  "The (:w W :h H :rgba S) of a data:image/png;base64 URL, or nil."
  (when (and (stringp url) (string-prefix-p "data:image/png;base64," url))
    (let* ((data (base64-decode-string (substring url (length "data:image/png;base64,"))))
           (i 8) w h ctype idat)
      (while (< (+ i 8) (length data))
        (let ((len (eas-png--u32 data i)) (type (substring data (+ i 4) (+ i 8))))
          (pcase type
            ("IHDR" (setq w (eas-png--u32 data (+ i 8)) h (eas-png--u32 data (+ i 12)) ctype (aref data (+ i 17))))
            ("IDAT" (push (substring data (+ i 8) (+ i 8 len)) idat)))
          (setq i (+ i 12 len))))
      (when (and w (eql ctype 6))
        (let ((raw (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (apply #'insert (nreverse idat))
                     (and (zlib-decompress-region (point-min) (point-max)) (buffer-string)))))
          (when (and raw (= (length raw) (* h (1+ (* 4 w)))))
            (list :w w :h h :rgba (eas-png--unfilter raw w h 4))))))))

(defvar eas-contour-text--image-cache (make-hash-table :test 'equal :weakness 'key)
  "PNG data URL -> its decoded pixels.")

(defun eas-contour-text-image-cells (url x y w h cw ch)
  "Cells (COL ROW COLOR ALPHA) of the PNG data URL drawn at X Y, W x H pixels.
Cells are CW x CH pixels; each takes the pixel under its centre.  nil
when URL is not PNG data."
  (when-let* ((img (or (gethash url eas-contour-text--image-cache)
                       (let ((d (eas-contour-text--png-data url))) (and d (puthash url d eas-contour-text--image-cache))))))
    (let ((iw (plist-get img :w)) (ih (plist-get img :h)) (px (plist-get img :rgba)) cells)
      (when (and (> w 0) (> h 0))
        (cl-loop for row from (max 0 (floor y ch)) below (ceiling (+ y h) ch)
                 for cy = (* (+ row 0.5) ch)
                 when (and (<= y cy) (< cy (+ y h)))
                 do (cl-loop for col from (max 0 (floor x cw)) below (ceiling (+ x w) cw)
                             for cx = (* (+ col 0.5) cw)
                             when (and (<= x cx) (< cx (+ x w)))
                             do (let* ((i (min (1- iw) (floor (* iw (/ (- cx x) w)))))
                                       (j (min (1- ih) (floor (* ih (/ (- cy y) h)))))
                                       (o (* 4 (+ i (* j iw)))))
                                  (push (list col row (format "#%02x%02x%02x" (aref px o) (aref px (+ o 1)) (aref px (+ o 2)))
                                              (/ (aref px (+ o 3)) 255.0))
                                        cells)))))
      (nreverse cells))))

;; Per-cell loops, compiled when the source is loaded interpreted.
(dolist (f '(eas-contour-text--path-rings eas-contour-text-path-cells eas-contour-text-image-cells))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(provide 'eas-contour-text)
;;; eas-contour-text.el ends here
