;;; eas-font-file.el --- user font files: metrics and the registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Charts may name any font family; the built-in tables in eas-font.el
;; only know Arial, Times New Roman and the monospace families.  A
;; user font file (TrueType or OpenType, .ttf or .otf) registered here
;; gives layout that font's own advance widths, read from the file's
;; cmap and hmtx tables, and gives the SVG renderer an @font-face rule
;; for it.
;;
;; Register a file from Lisp (a theme package does it once at load):
;;
;;   (eas-font-register "~/fonts/Inter-Regular.ttf")
;;   (eas-font-register "~/fonts/Inter-Bold.ttf" :weight 700)
;;
;; or from a chart, under its top-level "x-eas" key; resolve registers
;; them, so the exported Vega-Lite carries only the family names:
;;
;;   "x-eas": {"fonts": [{"src": "fonts/Inter-Regular.ttf"}]}
;;
;; The family defaults to the one the file's name table gives, the
;; weight and style to its OS/2 table's.  A relative "src" expands
;; against the template's directory (`default-directory' for a plain
;; spec).
;;
;; Fonts apply to the SVG backend only: the text backend draws every
;; glyph in one cell of the frame's own faces.  Emacs rasterizes SVG
;; with librsvg, which finds fonts through fontconfig and ignores
;; @font-face, so a chart shown in an Emacs frame draws a registered
;; family only when it is also installed (for instance in
;; ~/.local/share/fonts); its layout is measured from the file either
;; way.  An exported SVG carries the @font-face, so a browser draws it.

;;; Code:

(require 'eas-core)
(require 'url-util)

(defgroup eas-font nil
  "User font files for eas charts."
  :group 'eas :prefix "eas-font-")

(defvar eas-font-file--faces nil
  "Registered font faces, newest first.
Each is a plist (:family :weight :style :src :format :metrics), where
:metrics is what `eas-font-file-read' returns.")

(defvar eas-font-file-directory nil
  "Directory relative x-eas.fonts files expand against.
Nil means `default-directory'; resolving a template binds it to the
template's own directory.")

(defvar eas-font-file--cache (make-hash-table :test 'equal)
  "Parsed font files: (FILE . MTIME) -> metrics plist.")

;;; Reading the sfnt tables

(defsubst eas-font-file--u16 (data i)
  "Unsigned 16-bit big-endian integer in DATA at I."
  (logior (ash (aref data i) 8) (aref data (1+ i))))

(defsubst eas-font-file--s16 (data i)
  "Signed 16-bit big-endian integer in DATA at I."
  (let ((v (eas-font-file--u16 data i))) (if (>= v #x8000) (- v #x10000) v)))

(defsubst eas-font-file--u32 (data i)
  "Unsigned 32-bit big-endian integer in DATA at I."
  (logior (ash (eas-font-file--u16 data i) 16) (eas-font-file--u16 data (+ i 2))))

(defun eas-font-file--bad (file why)
  "Signal that FILE is not a font eas can read, because of WHY."
  (eas-signal "INVALID_INPUT" (format "Font file %s: %s" file why)
              :file file :next "pass a TrueType (.ttf) or OpenType (.otf) file"))

(defun eas-font-file--tables (data file)
  "The table directory of sfnt DATA (from FILE): alist TAG -> offset."
  (let ((tag (and (>= (length data) 12) (substring data 0 4))))
    (pcase tag
      ((or "\0\1\0\0" "OTTO" "true") nil)
      ((or "wOFF" "wOF2") (eas-font-file--bad file "WOFF is compressed; convert it to .ttf or .otf"))
      ("ttcf" (eas-font-file--bad file "font collections are not read; extract one font"))
      (_ (eas-font-file--bad file "not an sfnt font")))
    (let ((n (eas-font-file--u16 data 4)))
      (when (> (+ 12 (* 16 n)) (length data))
        (eas-font-file--bad file "truncated table directory"))
      (cl-loop for k below n for at = (+ 12 (* 16 k))
               collect (cons (substring data at (+ at 4)) (eas-font-file--u32 data (+ at 8)))))))

(defun eas-font-file--cmap (data at)
  "The best Unicode subtable of the cmap table of DATA at AT, as an offset.
A format 12 (full Unicode) subtable wins over a format 4 (BMP) one."
  (let (best rank)
    (dotimes (k (eas-font-file--u16 data (+ at 2)))
      (let* ((rec (+ at 4 (* 8 k)))
             (pid (eas-font-file--u16 data rec)) (eid (eas-font-file--u16 data (+ rec 2)))
             (off (+ at (eas-font-file--u32 data (+ rec 4))))
             (fmt (eas-font-file--u16 data off))
             (r (and (or (= pid 0) (and (= pid 3) (memq eid '(1 10))))
                     (pcase fmt (12 2) (4 1)))))
        (when (and r (or (null rank) (> r rank))) (setq best off rank r))))
    best))

(defun eas-font-file--map-chars (data off fn)
  "Call FN with (CHAR GLYPH) for every mapping of cmap subtable OFF in DATA."
  (pcase (eas-font-file--u16 data off)
    (4 (let* ((segs (/ (eas-font-file--u16 data (+ off 6)) 2))
              (ends (+ off 14)) (starts (+ ends (* 2 segs) 2))
              (deltas (+ starts (* 2 segs))) (ranges (+ deltas (* 2 segs))))
         (dotimes (s segs)
           (let ((end (eas-font-file--u16 data (+ ends (* 2 s))))
                 (start (eas-font-file--u16 data (+ starts (* 2 s))))
                 (delta (eas-font-file--u16 data (+ deltas (* 2 s))))
                 (range-at (+ ranges (* 2 s))))
             (unless (= start #xffff)
               (cl-loop for c from start to end
                        for g = (if (zerop (eas-font-file--u16 data range-at)) (logand (+ c delta) #xffff)
                                  (let ((g (eas-font-file--u16 data (+ range-at (eas-font-file--u16 data range-at)
                                                                       (* 2 (- c start))))))
                                    (if (zerop g) 0 (logand (+ g delta) #xffff))))
                        unless (zerop g) do (funcall fn c g)))))))
    (12 (dotimes (k (eas-font-file--u32 data (+ off 12)))
          (let ((at (+ off 16 (* 12 k))))
            (cl-loop with g0 = (eas-font-file--u32 data (+ at 8))
                     with start = (eas-font-file--u32 data at)
                     for c from start to (min (eas-font-file--u32 data (+ at 4)) (max-char))
                     do (funcall fn c (+ g0 (- c start)))))))))

(defun eas-font-file--name (data at)
  "The family name in the name table of DATA at AT, or nil.
The typographic family (name 16) wins over the legacy one (name 1)."
  (let ((count (eas-font-file--u16 data (+ at 2)))
        (strings (+ at (eas-font-file--u16 data (+ at 4))))
        found)
    (dolist (id '(1 16))
      (dotimes (k count)
        (let* ((rec (+ at 6 (* 12 k)))
               (pid (eas-font-file--u16 data rec))
               (len (eas-font-file--u16 data (+ rec 8)))
               (start (+ strings (eas-font-file--u16 data (+ rec 10)))))
          (when (= (eas-font-file--u16 data (+ rec 6)) id)
            (let ((raw (substring data start (+ start len))))
              (pcase pid
                ((or 0 3) (setq found (decode-coding-string raw 'utf-16be)))
                (1 (unless found (setq found (decode-coding-string raw 'mac-roman))))))))))
    (and found (not (string-empty-p found)) found)))

(defun eas-font-file-parse (data &optional file)
  "Metrics of the sfnt font DATA, a unibyte string read from FILE.
Return (:family F :weight W :style S :units-per-em U :ascent A
:descent D :widths HASH :missing M), where HASH maps a character to its
advance width in font units and M is the advance of the missing glyph."
  (let* ((tables (eas-font-file--tables data file))
         (table (lambda (tag) (or (cdr (assoc tag tables))
                                  (eas-font-file--bad file (format "no %s table" tag)))))
         (head (funcall table "head")) (hhea (funcall table "hhea"))
         (hmtx (funcall table "hmtx")) (cmap (funcall table "cmap"))
         (os2 (cdr (assoc "OS/2" tables))) (name (cdr (assoc "name" tables)))
         (nh (eas-font-file--u16 data (+ hhea 34)))
         (advance (lambda (g) (eas-font-file--u16 data (+ hmtx (* 4 (min g (1- nh)))))))
         (sub (or (eas-font-file--cmap data cmap)
                  (eas-font-file--bad file "no Unicode cmap (format 4 or 12)")))
         (widths (make-hash-table :test 'eql)))
    (eas-font-file--map-chars data sub (lambda (c g) (puthash c (funcall advance g) widths)))
    (list :family (and name (eas-font-file--name data name))
          :weight (if os2 (eas-font-file--u16 data (+ os2 4)) 400)
          :style (if (and os2 (= 1 (logand 1 (eas-font-file--u16 data (+ os2 62))))) "italic" "normal")
          :units-per-em (eas-font-file--u16 data (+ head 18))
          :ascent (eas-font-file--s16 data (+ hhea 4)) :descent (eas-font-file--s16 data (+ hhea 6))
          :widths widths :missing (funcall advance 0))))

(defun eas-font-file-read (file)
  "Metrics of font FILE (see `eas-font-file-parse'), cached by mtime."
  (let ((file (expand-file-name file)))
    (unless (file-readable-p file)
      (eas-signal "NOT_FOUND" (format "Font file %s not found" file) :file file))
    (let ((key (cons file (file-attribute-modification-time (file-attributes file)))))
      (or (gethash key eas-font-file--cache)
          (puthash key (eas-font-file-parse (with-temp-buffer
                                              (set-buffer-multibyte nil)
                                              (insert-file-contents-literally file)
                                              (buffer-string))
                                            file)
                   eas-font-file--cache)))))

;;; The registry

(defun eas-font-file--weight (weight)
  "CSS WEIGHT (a number or a keyword string) as a number."
  (cond ((numberp weight) weight)
        ((member weight '("bold" "bolder")) 700)
        ((and (stringp weight) (string-match-p "\\`[0-9]+\\'" weight)) (string-to-number weight))
        (t 400)))

(cl-defun eas-font-register (file &key family weight style)
  "Register font FILE so charts can name its family; return the family.
FAMILY, WEIGHT (a CSS weight) and STYLE (\"normal\" or \"italic\")
default to what the file says.  Registering the same family, weight
and style again replaces the earlier face."
  (let* ((src (expand-file-name file))
         (metrics (eas-font-file-read src))
         (family (or family (plist-get metrics :family)
                     (eas-font-file--bad src "no family name; pass :family")))
         (face (list :family family :weight (eas-font-file--weight (or weight (plist-get metrics :weight)))
                     :style (or style (plist-get metrics :style)) :src src
                     :format (if (string-suffix-p ".otf" (downcase src)) "opentype" "truetype")
                     :metrics metrics)))
    (setq eas-font-file--faces
          (cons face (cl-remove-if (lambda (f) (and (eas-font-file--same-family-p (plist-get f :family) family)
                                                    (equal (plist-get f :weight) (plist-get face :weight))
                                                    (equal (plist-get f :style) (plist-get face :style))))
                                   eas-font-file--faces)))
    family))

(defun eas-font-unregister (family)
  "Forget every registered face of FAMILY."
  (setq eas-font-file--faces
        (cl-remove-if (lambda (f) (eas-font-file--same-family-p (plist-get f :family) family))
                      eas-font-file--faces)))

(defun eas-font-file--same-family-p (a b)
  "Non-nil when family names A and B match, as CSS matches them."
  (and (stringp a) (stringp b) (string= (downcase a) (downcase b))))

(defun eas-font-file--families (family)
  "The family names of CSS font-family list FAMILY, unquoted."
  (and (stringp family)
       (mapcar (lambda (s) (string-trim s "[ \t\"']+" "[ \t\"']+"))
               (split-string family "," t))))

(defun eas-font-file-faces (family)
  "The registered faces of the first registered family in FAMILY.
FAMILY is a CSS font-family list; nil when it names none."
  (cl-loop for name in (eas-font-file--families family)
           for faces = (cl-remove-if-not (lambda (f) (eas-font-file--same-family-p (plist-get f :family) name))
                                         eas-font-file--faces)
           when faces return faces))

(defvar eas-font-file--lookups (cons nil nil)
  "Memoized `eas-font-file-face' answers: (FACES . HASH).
HASH holds answers for the registry FACES; a new registry starts over.")

(defun eas-font-file-face (family &optional bold italic)
  "The registered face that best draws FAMILY, or nil.
BOLD and ITALIC ask for a bold or italic face; the nearest weight
and then the requested style win."
  (unless (and (cdr eas-font-file--lookups) (eq (car eas-font-file--lookups) eas-font-file--faces))
    (setq eas-font-file--lookups (cons eas-font-file--faces (make-hash-table :test 'equal))))
  (let ((key (list family bold italic)) (hash (cdr eas-font-file--lookups)))
    (pcase (gethash key hash 'none)
      ('none (puthash key (eas-font-file--best family bold italic) hash))
      (face face))))

(defun eas-font-file--best (family bold italic)
  "The registered face that best draws FAMILY with BOLD and ITALIC."
  (when-let* ((faces (eas-font-file-faces family)))
    (let ((want (if bold 700 400)) (style (if italic "italic" "normal")))
      (car (sort (copy-sequence faces)
                 (lambda (a b)
                   (let ((sa (if (equal (plist-get a :style) style) 0 1))
                         (sb (if (equal (plist-get b :style) style) 0 1)))
                     (if (/= sa sb) (< sa sb)
                       (< (abs (- (plist-get a :weight) want)) (abs (- (plist-get b :weight) want)))))))))))

(defun eas-font-file-width (face text size)
  "Width in pixels of TEXT at SIZE px set in registered FACE."
  (let* ((m (plist-get face :metrics)) (widths (plist-get m :widths))
         (missing (plist-get m :missing)) (units 0))
    (dotimes (i (length text))
      (setq units (+ units (or (gethash (aref text i) widths) missing))))
    (/ (* units size) (float (plist-get m :units-per-em)))))

(defun eas-font-file-used (families)
  "The registered faces of every family named in FAMILIES (CSS lists).
Each face is (:family :weight :style :src :format), the form scene/v1
carries for the SVG renderer's @font-face rules."
  (let (out)
    (dolist (family families)
      (dolist (name (eas-font-file--families family))
        (dolist (f (reverse eas-font-file--faces))
          (when (eas-font-file--same-family-p (plist-get f :family) name)
            (let ((face (list :family (plist-get f :family) :weight (plist-get f :weight)
                              :style (plist-get f :style) :src (plist-get f :src) :format (plist-get f :format))))
              (unless (member face out) (push face out)))))))
    (nreverse out)))

(defun eas-font-file-scene-fonts (&rest nodes)
  "The registered faces whose families NODES (spec or config) name.
Every string under a font key (font, labelFont, titleFont, ...)
counts; data is skipped.  Nil when no font file is registered."
  (when eas-font-file--faces
    (let (families)
      (cl-labels ((walk (v)
                    (cond ((vectorp v) (seq-do #'walk v))
                          ((and (consp v) (keywordp (car v)))
                           (cl-loop for (k x) on v by #'cddr
                                    unless (memq k '(:data :datasets :values))
                                    do (if (and (stringp x) (or (eq k :font) (string-suffix-p "Font" (symbol-name k))))
                                           (cl-pushnew x families :test #'equal)
                                         (walk x)))))))
        (mapc #'walk nodes))
      (eas-font-file-used (nreverse families)))))

(defvar eas-font-file--data-urls (make-hash-table :test 'equal)
  "Base64 data: URLs of font files: (FILE . MTIME) -> URL.")

(defun eas-font-file--data-url (file format)
  "FILE as a data: URL of font FORMAT (\"truetype\" or \"opentype\")."
  (let ((key (cons file (file-attribute-modification-time (file-attributes file)))))
    (or (gethash key eas-font-file--data-urls)
        (puthash key (concat (if (equal format "opentype") "data:font/otf" "data:font/ttf") ";base64,"
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (base64-encode-region (point-min) (point-max) t)
                               (buffer-string)))
                 eas-font-file--data-urls))))

(defun eas-font-file-css (faces embed)
  "CSS @font-face rules for scene FACES (see `eas-font-file-used').
EMBED `data' inlines each file as a data: URL, so the CSS stands
alone; `url' links the file by a file: URL.  A face whose file has gone
is left out."
  (mapconcat
   (lambda (f)
     (let ((src (plist-get f :src)))
       (when (file-readable-p src)
         (format "@font-face{font-family:'%s';font-weight:%s;font-style:%s;src:url('%s') format('%s')}"
                 (plist-get f :family) (plist-get f :weight) (plist-get f :style)
                 (if (eq embed 'data) (eas-font-file--data-url src (plist-get f :format))
                   (concat "file://" (url-hexify-string src url-path-allowed-chars)))
                 (plist-get f :format)))))
   faces ""))

(defun eas-font-file-register-spec (spec)
  "Register the font files SPEC lists under \"x-eas\": {\"fonts\": [...]}.
Each entry is {\"src\": FILE} with optional family, weight and style."
  (let ((fonts (plist-get (plist-get spec :x-eas) :fonts)))
    (when fonts
      (unless (vectorp fonts)
        (eas-signal "INVALID_INPUT" "x-eas.fonts must be an array of {src, family?, weight?, style?}"
                    :path "/x-eas/fonts"))
      (seq-do-indexed
       (lambda (entry i)
         (let ((src (and (eas-object-p entry) (plist-get entry :src))))
           (unless (stringp src)
             (eas-signal "INVALID_INPUT" "x-eas.fonts entry needs a \"src\" font file"
                         :path (format "/x-eas/fonts/%d" i)))
           (eas-font-register (expand-file-name src eas-font-file-directory) :family (plist-get entry :family) :weight (plist-get entry :weight)
                              :style (plist-get entry :style))))
       fonts))))

(provide 'eas-font-file)
;;; eas-font-file.el ends here
