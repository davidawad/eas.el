;;; eas-png.el --- PNG decoding and size-tolerant image comparison -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The conformance oracle compares a native rendering with bin/chart's
;; reference PNG.  Vega pads its canvas for marks and labels that
;; overhang the plot by a few pixels, so two faithful renderings of one
;; spec rarely share a canvas size, and a plain pixel diff scores any
;; size mismatch as total failure.  `eas-png-compare' instead aligns
;; the two images (the best offset within `eas-png-max-shift' pixels,
;; found from their ink profiles and refined by trial), pads both onto
;; their union canvas with the reference's background, and reports the
;; size delta separately from the ratio of differing pixels.
;;
;; A pixel differs when its YIQ color distance exceeds
;; `eas-png-pixel-threshold' (pixelmatch's measure and default), after
;; compositing over white.  Images stay RGBA; identical runs are skipped
;; at C speed, so only mismatching pixels cost Lisp time.  Decoding needs only Emacs's zlib: 8-bit
;; non-interlaced gray, RGB, gray+alpha and RGBA, which covers
;; rsvg-convert and vl-convert output.

;;; Code:

(require 'eas-core)

(defvar eas-png-pixel-threshold 0.1
  "YIQ distance, as a fraction of the maximum, above which a pixel differs.")

(defvar eas-png-max-shift 8
  "Largest offset in pixels tried when aligning two images.")

;;; Decoding

(defun eas-png--u32 (s i)
  "Big-endian unsigned 32-bit integer in unibyte S at I."
  (logior (ash (aref s i) 24) (ash (aref s (+ i 1)) 16) (ash (aref s (+ i 2)) 8) (aref s (+ i 3))))

(defun eas-png--unfilter (raw w h bpp)
  "Reverse the PNG scanline filters of RAW (W x H, BPP bytes per pixel).
Return a unibyte string of H rows of W*BPP bytes."
  (let* ((stride (* w bpp)) (out (make-string (* stride h) 0)))
    (dotimes (y h)
      (let* ((in (1+ (* y (1+ stride)))) (filter (aref raw (1- in))) (row (* y stride)) (up (- row stride)))
        (cond
         ((or (= filter 0) (and (= filter 2) (= y 0)))
          (store-substring out row (substring raw in (+ in stride))))
         ((= filter 1)
          (dotimes (i stride)
            (aset out (+ row i) (logand 255 (+ (aref raw (+ in i)) (if (< i bpp) 0 (aref out (- (+ row i) bpp))))))))
         ((= filter 2)
          (dotimes (i stride)
            (aset out (+ row i) (logand 255 (+ (aref raw (+ in i)) (aref out (+ up i)))))))
         ((= filter 3)
          (dotimes (i stride)
            (aset out (+ row i)
                  (logand 255 (+ (aref raw (+ in i))
                                 (ash (+ (if (< i bpp) 0 (aref out (- (+ row i) bpp))) (if (= y 0) 0 (aref out (+ up i))))
                                      -1))))))
         ((= filter 4)
          (dotimes (i stride)
            (let* ((a (if (< i bpp) 0 (aref out (- (+ row i) bpp))))
                   (b (if (= y 0) 0 (aref out (+ up i))))
                   (c (if (or (= y 0) (< i bpp)) 0 (aref out (- (+ up i) bpp))))
                   (p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
              (aset out (+ row i)
                    (logand 255 (+ (aref raw (+ in i)) (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c))))))))
         (t (eas-signal "INVALID_INPUT" (format "PNG filter type %d is unknown" filter))))))
    out))

(defun eas-png--rgba (px n bpp)
  "N pixels of BPP bytes (gray, gray+alpha or RGB) in PX as RGBA."
  (let ((out (make-string (* 4 n) 0)))
    (dotimes (k n)
      (let ((o (* k bpp)) (q (* 4 k)))
        (dotimes (c 3) (aset out (+ q c) (aref px (if (< bpp 3) o (+ o c)))))
        (aset out (+ q 3) (if (= bpp 2) (aref px (1+ o)) 255))))
    out))

(defun eas-png--rgb-at (s o)
  "The RGBA pixel of S at O, composited over white, as (R G B)."
  (let ((alpha (aref s (+ o 3))))
    (if (= alpha 255) (list (aref s o) (aref s (+ o 1)) (aref s (+ o 2)))
      (mapcar (lambda (c) (/ (+ (* (aref s (+ o c)) alpha) (* 255 (- 255 alpha)) 127) 255)) '(0 1 2)))))

(defun eas-png-read (file)
  "Decode PNG FILE to (:w W :h H :rgba STRING), STRING holding W*H RGBA pixels.
Signals INVALID_INPUT for PNGs this decoder does not handle."
  (let ((data (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally file)
                (buffer-string)))
        (i 8) w h depth ctype interlace idat)
    (unless (string-prefix-p "\x89PNG\r\n\x1a\n" data)
      (eas-signal "INVALID_INPUT" (format "%s is not a PNG file" file) :file file))
    (while (< (+ i 8) (length data))
      (let ((len (eas-png--u32 data i)) (type (substring data (+ i 4) (+ i 8))))
        (pcase type
          ("IHDR" (setq w (eas-png--u32 data (+ i 8)) h (eas-png--u32 data (+ i 12))
                        depth (aref data (+ i 16)) ctype (aref data (+ i 17)) interlace (aref data (+ i 20))))
          ("IDAT" (push (substring data (+ i 8) (+ i 8 len)) idat)))
        (setq i (+ i 12 len))))
    (unless (and (eql depth 8) (memq ctype '(0 2 4 6)) (eql interlace 0))
      (eas-signal "INVALID_INPUT"
                    (format "%s: only 8-bit non-interlaced gray/RGB(A) PNGs are supported (depth %s, color type %s)"
                            file depth ctype)
                    :file file))
    (let* ((bpp (pcase ctype (0 1) (2 3) (4 2) (_ 4)))
           (raw (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (apply #'insert (nreverse idat))
                  (unless (zlib-decompress-region (point-min) (point-max))
                    (eas-signal "INVALID_INPUT" (format "%s: corrupt image data" file) :file file))
                  (buffer-string))))
      (unless (= (length raw) (* h (1+ (* w bpp))))
        (eas-signal "INVALID_INPUT" (format "%s: image data is truncated" file) :file file))
      (let ((px (eas-png--unfilter raw w h bpp)))
        (list :w w :h h :rgba (if (= bpp 4) px (eas-png--rgba px (* w h) bpp)))))))

;;; Comparison

(defun eas-png--yiq-delta (r1 g1 b1 r2 g2 b2)
  "pixelmatch's squared YIQ distance between colors R1 G1 B1 and R2 G2 B2."
  (let* ((dr (- r1 r2)) (dg (- g1 g2)) (db (- b1 b2))
         (y (+ (* 0.29889531 dr) (* 0.58662247 dg) (* 0.11448223 db)))
         (i (- (* 0.59597799 dr) (* 0.2741761 dg) (* 0.32180189 db)))
         (q (+ (* 0.21147017 dr) (* -0.52261711 dg) (* 0.31114694 db))))
    (+ (* 0.5053 y y) (* 0.299 i i) (* 0.1957 q q))))

(defun eas-png--background (img)
  "IMG's background: its top-left pixel, as RGBA bytes (R G B A)."
  (let ((s (plist-get img :rgba))) (list (aref s 0) (aref s 1) (aref s 2) (aref s 3))))

(defun eas-png--scan (s1 o1 s2 o2 n limit)
  "Pixels among N RGBA pixels of S1 at O1 and S2 at O2 farther apart than LIMIT.
Identical runs are skipped with `compare-strings', at C speed."
  (let ((diff 0) (i 0))
    (while (< i n)
      (let ((m (compare-strings s1 (+ o1 (* 4 i)) (+ o1 (* 4 n)) s2 (+ o2 (* 4 i)) (+ o2 (* 4 n)))))
        (if (eq m t) (setq i n)
          (let ((j (+ i (/ (1- (abs m)) 4))))
            (when (> (apply #'eas-png--yiq-delta (append (eas-png--rgb-at s1 (+ o1 (* 4 j)))
                                                           (eas-png--rgb-at s2 (+ o2 (* 4 j)))))
                     limit)
              (setq diff (1+ diff)))
            (setq i (1+ j))))))
    diff))

(defun eas-png--count (a b dx dy bg limit &optional bound)
  "Differing pixels of A and B (B shifted by DX DY) on their union canvas.
Pixels off either image read as BG; a pixel differs when its squared
YIQ distance exceeds LIMIT.  Return (DIFFERING . TOTAL), stopping early
once DIFFERING exceeds BOUND."
  (let* ((aw (plist-get a :w)) (ah (plist-get a :h)) (as (plist-get a :rgba))
         (bw (plist-get b :w)) (bh (plist-get b :h)) (bs (plist-get b :rgba))
         (x0 (min 0 dx)) (y0 (min 0 dy))
         (x1 (max aw (+ bw dx))) (y1 (max ah (+ bh dy)))
         (bgrow (apply #'unibyte-string (apply #'append (make-list (- x1 x0) bg))))
         (bound (or bound most-positive-fixnum))
         (diff 0) (y y0))
    (while (and (< y y1) (<= diff bound))
      (let* ((ra (and (>= y 0) (< y ah) (* 4 y aw)))
             (rb (and (>= (- y dy) 0) (< (- y dy) bh) (* 4 (- y dy) bw)))
             ;; Columns covered by A, by B and by both.
             (a0 (if ra 0 x1)) (a1 (if ra aw x1))
             (b0 (if rb dx x1)) (b1 (if rb (+ bw dx) x1))
             (c0 (max a0 b0)) (c1 (min a1 b1)))
        (cl-flet ((alone (img row lo hi off)
                    ;; IMG's canvas columns LO..HI that the other image lacks, against background.
                    (dolist (seg (if (< c0 c1) (list (cons lo (min hi c0)) (cons (max lo c1) hi))
                                   (list (cons lo hi))))
                      (when (< (car seg) (cdr seg))
                        (setq diff (+ diff (eas-png--scan img (+ row (* 4 (- (car seg) off))) bgrow 0
                                                            (- (cdr seg) (car seg)) limit)))))))
          (when ra (alone as ra a0 a1 0))
          (when rb (alone bs rb b0 b1 dx))
          (when (< c0 c1)
            (setq diff (+ diff (eas-png--scan as (+ ra (* 4 c0)) bs (+ rb (* 4 (- c0 dx))) (- c1 c0) limit)))))
        (setq y (1+ y))))
    (cons diff (* (- x1 x0) (- y1 y0)))))

(defun eas-png--profiles (img bg)
  "Ink of IMG per column and per row, as (COLUMNS . ROWS) vectors.
Ink is any pixel whose RGB differs from BG; background runs are
skipped with `compare-strings'."
  (let* ((w (plist-get img :w)) (h (plist-get img :h)) (s (plist-get img :rgba))
         (r (nth 0 bg)) (g (nth 1 bg)) (b (nth 2 bg))
         (bgrow (apply #'unibyte-string (apply #'append (make-list w bg))))
         (cols (make-vector w 0)) (rows (make-vector h 0)))
    (dotimes (y h)
      (let ((o (* 4 y w)) (i 0))
        (while (< i w)
          (let ((m (compare-strings s (+ o (* 4 i)) (+ o (* 4 w)) bgrow (* 4 i) (* 4 w))))
            (if (eq m t) (setq i w)
              (let* ((j (+ i (/ (1- (abs m)) 4))) (q (+ o (* 4 j))))
                (unless (and (= (aref s q) r) (= (aref s (+ q 1)) g) (= (aref s (+ q 2)) b))
                  (aset cols j (1+ (aref cols j)))
                  (aset rows y (1+ (aref rows y))))
                (setq i (1+ j))))))))
    (cons cols rows)))

(defun eas-png--best-shift (pa pb max-shift)
  "Shift of profile PB against PA, within MAX-SHIFT, with least mismatch."
  (let ((best 0) best-score)
    (cl-loop for d from (- max-shift) to max-shift do
             (let ((score 0))
               (dotimes (i (+ (max (length pa) (length pb)) max-shift))
                 (let ((j (- i d)))
                   (setq score (+ score (abs (- (if (< i (length pa)) (aref pa i) 0)
                                                (if (and (>= j 0) (< j (length pb))) (aref pb j) 0)))))))
               (when (or (null best-score) (< score best-score)
                         (and (= score best-score) (< (abs d) (abs best))))
                 (setq best d best-score score))))
    best))

(defun eas-png-compare (native reference &optional max-shift)
  "Compare decoded images NATIVE and REFERENCE (from `eas-png-read').
Return (:ratio R :offset [DX DY] :size-delta [DW DH] :native [W H]
:reference [W H]).  R is the fraction of differing pixels on the union
canvas with NATIVE shifted by the offset (at most MAX-SHIFT pixels per
axis); DW DH is NATIVE's size minus REFERENCE's."
  (let* ((max-shift (or max-shift eas-png-max-shift))
         (bg (eas-png--background reference))
         (limit (* 35215 eas-png-pixel-threshold eas-png-pixel-threshold))
         (score (lambda (d &optional bound)
                  (car (eas-png--count reference native (car d) (cdr d) bg limit bound))))
         (rp (eas-png--profiles reference bg)) (np (eas-png--profiles native bg))
         (best (cons (eas-png--best-shift (car rp) (car np) max-shift)
                     (eas-png--best-shift (cdr rp) (cdr np) max-shift)))
         (best-score (funcall score best))
         (improved t))
    ;; Profiles find the shift of the bulk of the ink; refine it locally
    ;; by differing-pixel count, which every pixel of both images feeds.
    (while improved
      (setq improved nil)
      (dolist (d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
        (let ((c (cons (+ (car best) (car d)) (+ (cdr best) (cdr d)))))
          (when (and (<= (abs (car c)) max-shift) (<= (abs (cdr c)) max-shift))
            (let ((s (funcall score c best-score)))
              (when (< s best-score) (setq best c best-score s improved t)))))))
    (let ((full (eas-png--count reference native (car best) (cdr best) bg limit)))
      (list :ratio (/ (float (car full)) (max 1 (cdr full)))
            :offset (vector (car best) (cdr best))
            :size-delta (vector (- (plist-get native :w) (plist-get reference :w))
                                (- (plist-get native :h) (plist-get reference :h)))
            :native (vector (plist-get native :w) (plist-get native :h))
            :reference (vector (plist-get reference :w) (plist-get reference :h))))))

;; These loops touch every pixel; interpreted (as when `make test' loads
;; the source) they take seconds per image, compiled a fraction of one.
(dolist (f '(eas-png--unfilter eas-png--rgba eas-png--rgb-at eas-png--yiq-delta eas-png--scan eas-png--count
             eas-png--profiles eas-png--best-shift))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(provide 'eas-png)
;;; eas-png.el ends here
