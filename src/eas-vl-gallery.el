;;; eas-vl-gallery.el --- the official Vega-Lite example gallery, natively -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; test/vl-examples/GROUP/NAME.vl.json are the official Vega-Lite
;; examples, unmodified; their data lives in test/vl-examples/data/ and
;; bin/chart's PNG of each in GROUP/ref/NAME.png.  For one example this
;; file loads the spec with its url data inlined (Vega's loader: JSON
;; arrays as they are, CSV/TSV with numbers inferred), renders it
;; natively for both backends, checks its layout at several sizes, and
;; compares the native SVG with the reference via `eas-png-compare'.
;;
;; GROUP/status.json records the verdict per example:
;;   {"NAME": {"status": "pass"|"partial"|"unsupported",
;;             "reason": "...", "threshold": T}}
;; "threshold" is the differing-pixel ratio the example must stay
;; within; `eas-vl-gallery-check' re-runs an example against it.  An
;; entry may also carry "refOmits": {"marks": [TYPE ...], "reason": R}
;; when bin/chart's reference is known to lack those marks (a Vega-Lite
;; defect, R says which): the comparison then leaves them out of the
;; native image too, and the example must still draw them natively.
;; An entry may also name its own reference ("ref", relative to GROUP,
;; with the command that built it in "ref_build") where bin/chart's is
;; wrong, and an oracle mask ("mask", eas-vl-gallery-mask.el) for
;; pixels that depend on the machine that built the reference.
;;
;; GROUP/custom/ holds customization specs, checked by
;; eas-vl-gallery-custom.el (their thresholds live in usermeta.eas).

;;; Code:

(require 'eas-core)
(require 'eas-adapters)
(require 'eas-template)
(require 'eas-compile)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-chart)
(require 'eas-png)
(require 'eas-vl-gallery-mask)

(defvar eas-vl-gallery-directory
  (expand-file-name "test/vl-examples" eas-template--root)
  "Directory holding the official Vega-Lite examples, one subdirectory per group.")

(defvar eas-vl-gallery-zone "America/Chicago"
  "Time zone the committed references were built in (bin/chart draws local time).")

(defconst eas-vl-gallery-sizes '((320 . 200) (480 . 300) (900 . 560))
  "Pixel sizes a passing example must lay out at without overlap.")

(defconst eas-vl-gallery-text-sizes '((:cols 50 :rows 14) (:cols 80 :rows 24) (:cols 120 :rows 36))
  "Text sizes a passing example must lay out at without overlap.")

(defun eas-vl-gallery-group-directory (group)
  "Directory of example GROUP."
  (expand-file-name group eas-vl-gallery-directory))

(defun eas-vl-gallery-groups ()
  "Groups of the gallery that record a status.json, sorted."
  (seq-filter (lambda (g) (file-exists-p (eas-vl-gallery-status-file g)))
              (and (file-directory-p eas-vl-gallery-directory)
                   (directory-files eas-vl-gallery-directory nil "\\`[^.]"))))

(defun eas-vl-gallery-names (group)
  "Example names in GROUP, sorted."
  (mapcar (lambda (f) (string-remove-suffix ".vl.json" f))
          (directory-files (eas-vl-gallery-group-directory group) nil "\\.vl\\.json\\'")))

;;; Loading

(defun eas-vl-gallery--read-url (url dir format)
  "Rows of data URL (relative to DIR) parsed per FORMAT (data.format)."
  (let* ((file (expand-file-name url dir))
         (type (or (plist-get format :type) (file-name-extension file))))
    (unless (file-readable-p file)
      (eas-signal "NOT_FOUND" (format "No data file %s" file) :path url))
    (pcase type
      ("json" (let ((v (eas-json-read-file file)))
                (if (vectorp v) v
                  (eas-signal "UNSUPPORTED_FEATURE" (format "%s is not an array of rows" url)
                              :feature "data/format"))))
      ("csv" (plist-get (eas-data-from "csv" (list :file file)) :rows))
      ("tsv" (plist-get (eas-data-from "tsv" (list :file file)) :rows))
      (_ (eas-signal "UNSUPPORTED_FEATURE" (format "data format %s" type)
                     :feature (concat "data/format/" type))))))

(defun eas-vl-gallery-inline (spec dir)
  "SPEC with every data.url (relative to DIR) replaced by inline values."
  (cond
   ((vectorp spec) (vconcat (mapcar (lambda (s) (eas-vl-gallery-inline s dir)) spec)))
   ((eas-object-p spec)
    (cl-loop for (k v) on spec by #'cddr
             append (list k (if (and (eq k :data) (eas-object-p v) (stringp (plist-get v :url)))
                                (list :values (eas-vl-gallery--read-url
                                               (plist-get v :url) dir (plist-get v :format)))
                              (eas-vl-gallery-inline v dir)))))
   (t spec)))

(defun eas-vl-gallery-spec (group name)
  "Example NAME of GROUP with its data inlined."
  (let ((dir (eas-vl-gallery-group-directory group)))
    (eas-vl-gallery-inline (eas-json-read-file (expand-file-name (concat name ".vl.json") dir)) dir)))

;;; Rendering and checking

(defmacro eas-vl-gallery--native (&rest body)
  "Run BODY in the references' zone, unrestricted by supported.json."
  `(let ((eas-time-zone eas-vl-gallery-zone) (eas-spec-supported-function nil)) ,@body))

(defun eas-vl-gallery-svg (spec &optional size)
  "Native SVG of SPEC (at SIZE, a (W . H) pixel cons)."
  (eas-vl-gallery--native (eas-svg-render (eas-compile spec :size size))))

(defun eas-vl-gallery-text (spec &optional size)
  "Text rendering of SPEC at SIZE (:cols C :rows R), without properties."
  (eas-vl-gallery--native
   (substring-no-properties
    (eas-text-render (eas-compile spec :target 'text :size (or size '(:cols 60 :rows 16)))))))

(defun eas-vl-gallery--intersects-p (a b)
  "Non-nil when boxes A and B ([X Y W H]) overlap by more than a pixel."
  (and (< (+ (aref a 0) 1) (+ (aref b 0) (aref b 2))) (< (+ (aref b 0) 1) (+ (aref a 0) (aref a 2)))
       (< (+ (aref a 1) 1) (+ (aref b 1) (aref b 3))) (< (+ (aref b 1) 1) (+ (aref a 1) (aref a 3)))))

(defun eas-vl-gallery--legend-box (legend)
  "Placed LEGEND's extent as [X Y W H]: its svg :box, else the text
legend's column from its title to its last entry."
  (if-let* ((b (plist-get legend :box)))
      (vector (aref b 0) (aref b 1) (- (aref b 2) (aref b 0)) (- (aref b 3) (aref b 1)))
    (let ((bottom (cl-loop for e across (plist-get legend :entries)
                           for eb = (plist-get e :bounds)
                           when (vectorp eb) maximize (+ (aref eb 1) (aref eb 3)))))
      (when (and (numberp (plist-get legend :x)) bottom)
        (vector (plist-get legend :x) (plist-get legend :y) (plist-get legend :width)
                (- bottom (plist-get legend :y)))))))

(defun eas-vl-gallery--label-collisions (scene)
  "Axes of SCENE whose shown tick labels collide, as problem strings.
An axis whose spec sets labelOverlap false asked for every label, as
Vega draws them, colliding or not: its labels are not a layout problem."
  (let* ((metrics (and (plist-get scene :target)
                       (eas-layout-metrics (intern (plist-get scene :target)) (plist-get (plist-get scene :size) :cell)
                                           (plist-get scene :config))))
         (size (plist-get metrics :label-size)) out)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (axis (seq-remove (lambda (a) (plist-get a :label-overlap-off)) (plist-get view :axes)))
        (let ((boxes (cl-loop for tk across (plist-get axis :ticks)
                              unless (string-empty-p (plist-get tk :label))
                              collect (eas-layout-text-bounds
                                       metrics (plist-get tk :label) size (plist-get tk :lx) (plist-get tk :ly)
                                       (plist-get tk :align) (plist-get tk :baseline)
                                       (and (equal (plist-get axis :orient) "bottom") (plist-get axis :labelAngle))))))
          (when (cl-loop for (a b) on boxes while b
                         thereis (and (< (+ (aref a 0) 1) (aref b 2)) (< (+ (aref b 0) 1) (aref a 2))
                                      (< (+ (aref a 1) 1) (aref b 3)) (< (+ (aref b 1) 1) (aref a 3))))
            (push (format "%s axis labels of view %s collide" (plist-get axis :channel) (plist-get view :id)) out)))))
    (nreverse out)))

(defconst eas-vl-gallery--inner-orients '("none" "top-left" "top-right" "bottom-left" "bottom-right")
  "Legend orients that place a legend inside the plot.")

(defun eas-vl-gallery-overlaps (scene)
  "Layout problems in SCENE: views overlapping each other or a legend,
views or legends leaving the canvas, axis labels colliding.  Return a
list of strings (nil when clean)."
  (let* ((size (plist-get scene :size)) (w (plist-get size :w)) (h (plist-get size :h))
         (views (append (plist-get scene :views) nil))
         (boxes (mapcar (lambda (v) (cons (plist-get v :id) (plist-get v :bounds))) views))
         (legends (apply #'append
                         (mapcar (lambda (v)
                                   ;; Legends placed with orient "none" or in a corner sit in the plot on purpose.
                                   (delq nil (mapcar (lambda (l) (let ((b (eas-vl-gallery--legend-box l)))
                                                                   (and (vectorp b) (not (member (plist-get l :orient) eas-vl-gallery--inner-orients))
                                                                        (cons (plist-get v :id) b))))
                                                     (plist-get v :legends))))
                                 views)))
         (outside (lambda (r) (or (< (aref r 0) -0.5) (< (aref r 1) -0.5)
                                  (> (+ (aref r 0) (aref r 2)) (+ w 0.5)) (> (+ (aref r 1) (aref r 3)) (+ h 0.5)))))
         problems)
    (dolist (l legends)
      (when (funcall outside (cdr l))
        (push (format "a legend of view %s leaves the %sx%s canvas" (car l) w h) problems)))
    (dolist (b boxes)
      (let ((r (cdr b)))
        (when (funcall outside r)
          (push (format "view %s leaves the %sx%s canvas" (car b) w h) problems))
        (dolist (o boxes)
          (when (and (string< (car b) (car o)) (eas-vl-gallery--intersects-p r (cdr o)))
            (push (format "views %s and %s overlap" (car b) (car o)) problems)))
        (dolist (l legends)
          (when (eas-vl-gallery--intersects-p r (cdr l))
            (push (format "a legend overlaps view %s" (car b)) problems)))))
    (append (nreverse problems) (eas-vl-gallery--label-collisions scene))))

(defun eas-vl-gallery-resize-problems (spec)
  "Overlap problems of SPEC at every size in `eas-vl-gallery-sizes' and
`eas-vl-gallery-text-sizes', each prefixed with the size."
  (eas-vl-gallery--native
   (let (out)
     (dolist (size eas-vl-gallery-sizes)
       (dolist (p (eas-vl-gallery-overlaps (eas-compile spec :size size)))
         (push (format "%dx%d: %s" (car size) (cdr size) p) out)))
     (dolist (size eas-vl-gallery-text-sizes)
       (dolist (p (eas-vl-gallery-overlaps (eas-compile spec :target 'text :size size)))
         (push (format "%dx%d cells: %s" (plist-get size :cols) (plist-get size :rows) p) out)))
     (nreverse out))))

(defun eas-vl-gallery-ref-file (group name)
  "The reference PNG of example NAME in GROUP: its status.json \"ref\",
else bin/chart's ref/NAME.png, else its \"interim_ref\"."
  (let* ((dir (eas-vl-gallery-group-directory group))
         (bin-chart (expand-file-name (concat "ref/" name ".png") dir))
         (interim (eas-vl-gallery--entry-field group name :interim_ref)))
    (cond ((eas-vl-gallery--entry-field group name :ref)
           (expand-file-name (eas-vl-gallery--entry-field group name :ref) dir))
          ((or (null interim) (file-exists-p bin-chart)) bin-chart)
          (t (expand-file-name interim dir)))))

(defun eas-vl-gallery-build-ref (group name)
  "Build bin/chart's reference ref/NAME.png of NAME in GROUP.
Signals NOT_FOUND without bin/chart."
  (let ((file (expand-file-name (concat "ref/" name ".png") (eas-vl-gallery-group-directory group)))
        (process-environment (cons (concat "TZ=" eas-vl-gallery-zone) process-environment))
        (coding-system-for-write 'no-conversion))
    (make-directory (file-name-directory file) t)
    (let ((png (eas-chart-build (eas-vl-gallery-spec group name) "png")))
      (with-temp-file file (set-buffer-multibyte nil) (insert png)))
    file))

(defun eas-vl-gallery--entry-field (group name key)
  "KEY of NAME's status.json entry in GROUP, or nil."
  (plist-get (plist-get (eas-vl-gallery-status group) (eas-key name)) key))

(defun eas-vl-gallery-mask (group name spec)
  "Boxes of SPEC's native scene that NAME's status.json mask hides, or nil."
  (when-let* ((kind (eas-vl-gallery--entry-field group name :mask)))
    (eas-vl-gallery-mask-boxes kind (eas-vl-gallery--native (eas-compile spec)))))

(defun eas-vl-gallery-rasterizer-p ()
  "Non-nil when native SVG can be rasterized and PNGs decoded here."
  (and (executable-find eas-chart-rsvg-program) (zlib-available-p)))

(defun eas-vl-gallery-omit-marks (scene types)
  "SCENE without its marks of TYPES (mark type strings)."
  (if (null types) scene
    (plist-put (copy-sequence scene) :views
               (vconcat (mapcar (lambda (v) (plist-put (copy-sequence v) :marks
                                                        (vconcat (seq-remove (lambda (m) (member (plist-get m :mark) types))
                                                                             (plist-get v :marks)))))
                                (plist-get scene :views))))))

(defun eas-vl-gallery-ref-omits (group name)
  "Mark types NAME's reference in GROUP lacks (status.json refOmits), or nil."
  (append (plist-get (plist-get (plist-get (eas-vl-gallery-status group) (eas-key name)) :refOmits) :marks) nil))

(defun eas-vl-gallery--omitted-drawn-p (spec types)
  "Non-nil when SPEC natively draws items of every mark type in TYPES."
  (let ((scene (eas-vl-gallery--native (eas-compile spec))))
    (seq-every-p (lambda (type)
                   (seq-some (lambda (v) (seq-some (lambda (m) (and (equal (plist-get m :mark) type)
                                                                    (> (length (plist-get m :items)) 0)))
                                                   (plist-get v :marks)))
                             (plist-get scene :views)))
                 types)))

(defun eas-vl-gallery-compare (group name svg &optional mask)
  "Compare native SVG of NAME in GROUP with its reference.
MASK is boxes painted over in both images (`eas-vl-gallery-mask').
Return `eas-png-compare's plist, or nil when no rasterizer is available."
  (when (eas-vl-gallery-rasterizer-p)
    (let ((mine (make-temp-file "eas-vl" nil ".png")))
      (unwind-protect
          (progn (eas-chart-rasterize svg mine)
                 (eas-png-compare (eas-vl-gallery-mask-image (eas-png-read mine) mask)
                                  (eas-vl-gallery-mask-image
                                   (eas-png-read (eas-vl-gallery-ref-file group name)) mask)))
        (delete-file mine)))))

;;; status.json

(defun eas-vl-gallery-status-file (group)
  "GROUP's status.json."
  (expand-file-name "status.json" (eas-vl-gallery-group-directory group)))

(defun eas-vl-gallery-status (group)
  "Parsed status.json of GROUP, or nil."
  (let ((file (eas-vl-gallery-status-file group)))
    (and (file-exists-p file) (eas-json-read-file file))))

(defun eas-vl-gallery-run (group name &optional compare)
  "Render example NAME of GROUP; with COMPARE, judge it against its reference.
Return (:name :ok :error :svg :text :ratio :size-delta :overlaps).  :ok
is non-nil when both backends rendered."
  (condition-case err
      (let* ((spec (eas-vl-gallery-spec group name))
             (omit (eas-vl-gallery-ref-omits group name))
             (svg (eas-vl-gallery-svg spec))
             (text (eas-vl-gallery-text spec))
             (cmp (and compare (eas-vl-gallery-compare
                                group name (if omit (eas-vl-gallery--native
                                                     (eas-svg-render (eas-vl-gallery-omit-marks (eas-compile spec) omit)))
                                             svg)
                                (eas-vl-gallery-mask group name spec)))))
        (list :name name :ok t :svg svg :text text
              :ratio (plist-get cmp :ratio) :size-delta (plist-get cmp :size-delta)
              :overlaps (append (eas-vl-gallery-resize-problems spec)
                                (when (and omit (not (eas-vl-gallery--omitted-drawn-p spec omit)))
                                  (list (format "native draws no %s, which the reference omits"
                                                (string-join omit ", ")))))))
    (eas-error (list :name name :ok nil :error (eas-error-plist err)))
    ;; Anything else the engine cannot yet do is an unsupported example too.
    (error (list :name name :ok nil :error (list :message (error-message-string err))))))

(defun eas-vl-gallery-check (group name)
  "Re-run NAME of GROUP against its status.json entry.
Return a list of failure strings (nil when it holds its status).  The
image comparison runs only where a rasterizer is available."
  (let* ((entry (plist-get (eas-vl-gallery-status group) (eas-key name)))
         (status (plist-get entry :status))
         (threshold (or (plist-get entry :threshold) 0.05))
         (_ (when (and entry (eas-chart-available-p) (not (plist-get entry :ref))
                       (not (file-exists-p (expand-file-name (concat "ref/" name ".png")
                                                             (eas-vl-gallery-group-directory group)))))
              (eas-vl-gallery-build-ref group name)))
         (r (and entry (not (equal status "unsupported"))
                 (eas-vl-gallery-run group name (eas-vl-gallery-rasterizer-p))))
         problems)
    (cond
     ((null entry) (push (format "%s has no status.json entry" name) problems))
     ((null r) nil)
     ((not (plist-get r :ok))
      (push (format "%s: %s" name (plist-get (plist-get r :error) :message)) problems))
     (t
      (when (and (equal status "pass") threshold (plist-get r :ratio) (> (plist-get r :ratio) threshold))
        (push (format "%s: ratio %.4f > %s" name (plist-get r :ratio) threshold) problems))
      (when (and (equal status "pass") (plist-get r :overlaps))
        (push (format "%s: %s" name (string-join (plist-get r :overlaps) "; ")) problems))))
    problems))

(defconst eas-vl-gallery-default-threshold 0.03
  "Differing-pixel ratio a new example passes within, unless its status.json
entry records another threshold with a reason.")

(defun eas-vl-gallery--verdict (r threshold)
  "status.json fields for run result R judged at THRESHOLD."
  (cond
   ((not (plist-get r :ok))
    (list :status "unsupported" :reason (plist-get (plist-get r :error) :message)))
   ((not (plist-get r :ratio))
    (list :status "partial" :reason "renders natively; no rasterizer here to compare with the reference"))
   (t
    (let* ((ratio (plist-get r :ratio)) (overlaps (plist-get r :overlaps))
           (size (plist-get r :size-delta))
           ;; The conformance oracle's own size bound (eas-conformance-size-tolerance).
           (ok (and (<= ratio threshold) (null overlaps)
                    (or (not (vectorp size)) (<= (max (abs (aref size 0)) (abs (aref size 1))) 8)))))
      (append (list :status (if ok "pass" "partial")
                    :reason (format "svg and text render natively at 3 sizes; oracle ratio %.4f (threshold %s)%s%s"
                                    ratio threshold
                                    (if (and (vectorp size) (not (equal size [0 0]))) (format ", size delta %S px" size) "")
                                    (if overlaps (concat "; overlaps: " (string-join overlaps "; ")) ""))
                    :ratio (/ (round (* ratio 10000)) 10000.0))
              (list :threshold threshold))))))

(defun eas-vl-gallery-write-status (group &optional names)
  "Judge examples NAMES (default all) of GROUP against the references and
record them in GROUP/status.json.  An existing entry keeps its
threshold, reason and oracle (ref, interim_ref, ref_build, mask) fields;
the verdict, ratio and a generated reason are rewritten unless
the entry has a \"note\" (a written reason, kept).  refOmits is kept too."
  (let ((old (eas-vl-gallery-status group)) (new nil))
    (dolist (name (eas-vl-gallery-names group))
      (let* ((prev (plist-get old (eas-key name))))
        (if (and names (not (member name names)))
            (when prev (setq new (append new (list (eas-key name) prev))))
          (let* ((threshold (or (plist-get prev :threshold) eas-vl-gallery-default-threshold))
                 (v (eas-vl-gallery--verdict (eas-vl-gallery-run group name t) threshold))
                 (note (plist-get prev :note)))
            (setq new (append new (list (eas-key name)
                                        (append (if note (plist-put v :reason (concat note "; " (plist-get v :reason))) v)
                                                (when note (list :note note))
                                                (unless (plist-member v :threshold) (list :threshold threshold))
                                                ;; The written oracle fields stay too.
                                                (cl-loop for k in '(:ref :interim_ref :ref_build :mask)
                                                         when (plist-get prev k) append (list k (plist-get prev k)))
                                                (when (plist-get prev :refOmits)
                                                  (list :refOmits (plist-get prev :refOmits)))))))))))
    (with-temp-file (eas-vl-gallery-status-file group)
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (eas-json-pretty new)))
    new))

;;; The conformance gallery

(defun eas-vl-gallery-passing (group)
  "GROUP's examples recorded as passing, as (NAME . THRESHOLD)."
  (cl-loop for (k v) on (eas-vl-gallery-status group) by #'cddr
           when (equal (plist-get v :status) "pass")
           collect (cons (eas-key-name k) (or (plist-get v :threshold) 0.05))))

(defun eas-vl-gallery-ref-problem (group name)
  "Why NAME's reference in GROUP cannot be trusted, or nil."
  (unless (file-exists-p (eas-vl-gallery-ref-file group name))
    (if-let* ((build (eas-vl-gallery--entry-field group name :ref_build)))
        (format "STALE_REF: %s's reference %s is missing; rebuild it: %s"
                name (eas-vl-gallery--entry-field group name :ref) build)
      (format "STALE_REF: %s/ref/%s.png is missing; rebuild with bin/chart" group name))))

(defun eas-vl-gallery-conformance-entries ()
  "Conformance gallery entries for every passing example of every group.
They are judged like test/conformance specs (native text golden at
GROUP/NAME.txt, image oracle against GROUP/ref/NAME.png), and what they
prove goes into supported.json."
  (cl-loop for group in (eas-vl-gallery-groups)
           append (cl-loop for (name . threshold) in (eas-vl-gallery-passing group)
                           for dir = (eas-vl-gallery-group-directory group)
                           for spec = (eas-vl-gallery-spec group name)
                           collect (list :name (concat group "/" name)
                                         :file (expand-file-name (concat name ".vl.json") dir)
                                         :spec spec
                                         :threshold threshold
                                         :ref (eas-vl-gallery-ref-file group name)
                                         :ref-problem (eas-vl-gallery-ref-problem group name)
                                         :oracle-scene (let ((omit (eas-vl-gallery-ref-omits group name)))
                                                         (and omit (lambda (scene) (eas-vl-gallery-omit-marks scene omit))))
                                         ;; A reference of its own is not rebuilt with bin/chart.
                                         :ref-pinned (and (eas-vl-gallery--entry-field group name :ref) t)
                                         :mask (eas-vl-gallery-mask group name spec)
                                         :text-file (expand-file-name (concat name ".txt") dir)))))

(defvar eas-conformance-gallery-functions)
(add-hook 'eas-conformance-gallery-functions #'eas-vl-gallery-conformance-entries)

(provide 'eas-vl-gallery)
;;; eas-vl-gallery.el ends here
