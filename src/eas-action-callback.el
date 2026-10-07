;;; eas-action-callback.el --- click callbacks set up ahead of time -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (eas-7r1.10).  A click (mouse-1, RET at point, or an
;; agent's click event) runs a callback the user set up before any
;; chart exists, e.g. in init.el:
;;
;;   (eas-define-callback "bars" "main/0" #'my-open-day
;;     :when "datum.value > 5000")
;;
;; This file extends eas-action.el's bindings:
;;
;;   - a BINDING may be a function (a symbol or a lambda) or
;;     (:fn FN ARG VALUE ...), not only a registered action name;
;;   - any binding may carry :when, a Vega expression evaluated with
;;     `eas-expr' on the clicked datum (the row, or for areas the
;;     target itself; `target' names the whole target), or a
;;     function called with the target and the view; a binding is a
;;     vector or list of candidates, the first whose :when holds runs,
;;     so regions of one mark run different callbacks.  Candidates
;;     with a :when are tried before unconditional ones, each group in
;;     its own order, so a catch-all never shadows a :when entry;
;;   - `eas-action-default-bindings' holds global entries keyed by
;;     template name (nil for any view) and binding key.  The order,
;;     for each key from the most specific: the view's
;;     `eas-action-bind', global entries of the view's template, the
;;     template's x-eas.actions, global entries for any view;
;;   - clicks on areas that hold no datum become targets:
;;
;;       axis label or tick  (:area "axis" :axis CH :part "label"
;;                            :value V :label L)     keys axis:CH, axis
;;       axis title          (:area "axis" :axis CH :part "title"
;;                            :title T)              keys axis:CH, axis
;;       chart title         (:area "title" :part "title"|"subtitle"
;;                            :title T)              key title
;;       facet header        (:area "title" :part "header" :title T)
;;       plot background     (:area "background" :x X :y Y
;;                            :fields (:x F :y F))   key background
;;
;;     An area target is recorded as the view's :click only when a
;;     binding answers it, so an unbound click on empty space still
;;     clears :click.  Like legend targets, areas never run "*".
;;
;; Every target in a plot (datum or background) also carries the
;; click's data-space :x and :y, inverted through the view's scales.
;;
;; Area geometry is computed from the scene for its target, so GUI and
;; terminal agree with what they draw: SVG scenes measure label text
;; in the scene font, text scenes take the cells the text renderer
;; gives the string (`eas-text-string-span').  GUI frames also get
;; the areas as image :map hot spots (`eas-svg-hot-spot-functions').

;;; Code:

(require 'eas-core)
(require 'eas-expr)
(require 'eas-scale)
(require 'eas-font)
(require 'eas-hit)
(require 'eas-view)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-action)

(defgroup eas-action-callback nil
  "Click callbacks for eas charts."
  :group 'eas
  :prefix "eas-action-")

(defcustom eas-action-default-bindings nil
  "Global click bindings, set before any view exists.
Each entry is a plist: :template NAME (nil for any view), :key KEY (a
mark id, a click or legend param name, \"legend\", \"axis\",
\"axis:CHANNEL\", \"title\", \"background\" or \"*\"), then the binding:
:fn FUNCTION or :action NAME, optional :when (a Vega expression string
or a predicate on target and view), :doc, and any args.  Entries with
the same template and key are tried in order, those with a :when
before unconditional ones; the first that holds runs.
`eas-define-callback' adds entries."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'eas-action-callback)

(defconst eas-action-callback--area-pad 2
  "Pixels an SVG axis or title area reaches past its text.")

;;; Defining callbacks

(cl-defun eas-define-callback (template key fn &key when doc args)
  "Run FN when a click lands on KEY in views of TEMPLATE.
TEMPLATE is a template name, or nil (or \"*\") for any view.  KEY is a
binding key (see `eas-action-default-bindings').  FN is a function of
the target plist and the view, or a registered action name.  WHEN is
a Vega expression on the datum or a predicate on target and view.
ARGS (a plist) reach FN as the target's :args.  DOC is one line.
Redefining the same TEMPLATE, KEY and WHEN replaces the entry, so
re-evaluating an init file is harmless.  Return the entry."
  (unless (and (stringp key) (or (functionp fn) (stringp fn)))
    (eas-signal "INVALID_INPUT" "A callback is a key (string) and a function or action name"
                  :key key))
  (unless (or (null when) (stringp when) (functionp when))
    (eas-signal "INVALID_INPUT" ":when is a Vega expression string or a predicate function" :key key))
  (let* ((template (if (equal template "*") nil template))
         (entry (append (list :template template :key key)
                        (if (stringp fn) (list :action fn) (list :fn fn))
                        (when when (list :when when))
                        (when doc (list :doc doc))
                        args))
         (same (lambda (e) (and (equal (plist-get e :template) template) (equal (plist-get e :key) key)
                                (equal (plist-get e :when) when))))
         (old (seq-find same eas-action-default-bindings)))
    (setq eas-action-default-bindings
          (if old
              (mapcar (lambda (e) (if (eq e old) entry e)) eas-action-default-bindings)
            (append eas-action-default-bindings (list entry))))
    entry))

(defun eas-remove-callback (template key &optional when)
  "Remove the global callbacks for TEMPLATE and KEY (only WHEN's, if given)."
  (let ((template (if (equal template "*") nil template)))
    (setq eas-action-default-bindings
          (seq-remove (lambda (e) (and (equal (plist-get e :template) template) (equal (plist-get e :key) key)
                                       (or (null when) (equal (plist-get e :when) when))))
                      eas-action-default-bindings))))

(defun eas-action-callback--tables (view specific)
  "Global binding tables for VIEW: of its template when SPECIFIC, else of any.
One plist table of KEY -> vector of entries."
  (let ((template (eas-view-template view)) table)
    (dolist (e eas-action-default-bindings)
      (when (if specific
                (and template (equal (plist-get e :template) template))
              (null (plist-get e :template)))
        (let ((k (eas-key (plist-get e :key))))
          (setq table (eas-plist-put table k (vconcat (plist-get table k)
                                                       (list (eas--plist-without (eas--plist-without e :template)
                                                                                 :key))))))))
    (and table (list table))))

;;; :when

(defun eas-action-callback--datum (target)
  "What a :when expression sees as datum for TARGET: its row, else itself."
  (let ((row (plist-get target :row)))
    (if (and row (eas-object-p row)) row target)))

(defun eas-action-callback-when-p (when target view)
  "Return non-nil if WHEN is true for TARGET in VIEW.
WHEN is nil (always), a Vega expression string or a predicate.  A
predicate that fails does not hold."
  (condition-case nil
      (cond ((null when) t)
            ((stringp when)
             (eas-expr-truthy (eas-expr-evaluate when (eas-action-callback--datum target)
                                                     (list :target target))))
            ((functionp when) (funcall when target view)))
    (error nil)))

(defun eas-action-callback--candidates (binding)
  "BINDING's candidates, in order: a vector or list of bindings, or itself."
  (cond ((vectorp binding) (append binding nil))
        ((and (consp binding) (not (eas-object-p binding)) (not (functionp binding))) binding)
        (t (list binding))))

(defun eas-action-callback--when (b)
  "The :when of candidate B, or nil when B is unconditional."
  (and (eas-object-p b) (plist-get b :when)))

(defun eas-action-callback--ordered (binding)
  "BINDING's candidates, those with a :when first.
Each group keeps its own order, so an unconditional candidate never
shadows a conditional one for the same key."
  (let ((candidates (eas-action-callback--candidates binding)))
    (append (seq-filter #'eas-action-callback--when candidates)
            (seq-remove #'eas-action-callback--when candidates))))

(defun eas-action-callback-pick (binding view target)
  "Return the first of BINDING's candidates whose :when is true.
Candidates with a :when are tried before unconditional ones.  The
:when is judged for TARGET in VIEW."
  (seq-find (lambda (b) (and b (eas-action-callback-when-p (eas-action-callback--when b) target view)))
            (eas-action-callback--ordered binding)))

;;; Area geometry

(defun eas-action-callback--text-p (scene)
  "Non-nil when SCENE is laid out for the text renderer."
  (equal (plist-get scene :target) "text"))

(defun eas-action-callback--rotate-box (x y w h dx dy angle)
  "Bounds [x y w h] of box DX DY W H about anchor X Y turned ANGLE degrees."
  (let* ((a (degrees-to-radians (or angle 0))) (c (cos a)) (s (sin a))
         (pts (mapcar (lambda (p) (cons (+ x (- (* c (car p)) (* s (cdr p))))
                                        (+ y (* s (car p)) (* c (cdr p)))))
                      (list (cons dx dy) (cons (+ dx w) dy) (cons dx (+ dy h)) (cons (+ dx w) (+ dy h)))))
         (xs (mapcar #'car pts)) (ys (mapcar #'cdr pts)))
    (vector (apply #'min xs) (apply #'min ys) (- (apply #'max xs) (apply #'min xs)) (- (apply #'max ys) (apply #'min ys)))))

(defun eas-action-callback--text-box (scene text x y align baseline size angle &optional text-align)
  "Bounds [x y w h] of TEXT drawn at X Y in SCENE.
SVG scenes measure TEXT at font SIZE with ALIGN, BASELINE and ANGLE;
text scenes take the cells the text renderer gives it, aligned by
TEXT-ALIGN when the renderer aligns it otherwise than ALIGN."
  (let ((text (if (stringp text) text (format "%s" text))))
    (if (eas-action-callback--text-p scene)
        (let* ((size (plist-get scene :size)) (cell (plist-get size :cell))
               (cw (aref cell 0)) (ch (aref cell 1))
               (cols (max 1 (round (/ (float (plist-get size :w)) cw))))
               (rows (max 1 (round (/ (float (plist-get size :h)) ch))))
               (line (car (split-string text "\n")))
               (span (eas-text-string-span cols cw ch x y line (or text-align align) rows)))
          (vector (* cw (cadr span)) (* ch (car span)) (* cw (- (cddr span) (cadr span)))
                  (* ch (length (split-string text "\n")))))
      (let* ((lines (split-string text "\n"))
             (w (apply #'max 1 (mapcar (lambda (l) (eas-font-text-width l size)) lines)))
             (h (* (length lines) size)) (pad eas-action-callback--area-pad)
             (dx (- (* w (pcase align ("left" 0) ("right" 1) (_ 0.5)))))
             (dy (- (* h (pcase baseline ("top" 0) ("middle" 0.5) (_ 1)))))
             (b (eas-action-callback--rotate-box x y w h dx dy angle)))
        (vector (- (aref b 0) pad) (- (aref b 1) pad) (+ (aref b 2) pad pad) (+ (aref b 3) pad pad))))))

(defun eas-action-callback--union (a b)
  "Bounds [x y w h] holding both A and B (either may be nil)."
  (cond ((null a) b) ((null b) a)
        (t (let ((x0 (min (aref a 0) (aref b 0))) (y0 (min (aref a 1) (aref b 1)))
                 (x1 (max (+ (aref a 0) (aref a 2)) (+ (aref b 0) (aref b 2))))
                 (y1 (max (+ (aref a 1) (aref a 3)) (+ (aref b 1) (aref b 3)))))
             (vector x0 y0 (- x1 x0) (- y1 y0))))))

(defun eas-action-callback--axis-size (scene axis key default)
  "AXIS's font size KEY in SCENE: its own style, the axis config, DEFAULT."
  (or (plist-get (plist-get axis :style) key)
      (plist-get (plist-get (plist-get scene :config) :axis) key)
      default))

(defun eas-action-callback--axis-areas (scene view axis)
  "Areas of AXIS in VIEW of SCENE: its title, then each tick and label.
Retained per axis (`eas-svg-retain-part'): they depend on the axis and
on SCENE's target, size and axis config only, so a frame whose axes did
not move measures no label again."
  (eas-svg-retain-part (list 'axis-areas (plist-get view :id) (plist-get axis :channel) (plist-get axis :orient))
                       (list axis (plist-get scene :target) (plist-get scene :size)
                             (plist-get (plist-get scene :config) :axis))
                       (lambda () (eas-action-callback--axis-areas-1 scene view axis))))

(defun eas-action-callback--axis-areas-1 (scene view axis)
  "Areas of AXIS in VIEW of SCENE, as `eas-action-callback--axis-areas'."
  (let* ((id (plist-get view :id)) (channel (plist-get axis :channel))
         (horizontal (member (plist-get axis :orient) '("bottom" "top")))
         (size (eas-action-callback--axis-size scene axis :labelFontSize 10))
         (areas nil))
    (when-let* ((tm (plist-get axis :title-mark)))
      (push (list :box (eas-action-callback--text-box
                        scene (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) (plist-get tm :align)
                        (plist-get tm :baseline) (eas-action-callback--axis-size scene axis :titleFontSize 11)
                        (plist-get tm :angle))
                  :id (format "eas-axis:%s|%s|title" id channel)
                  :target (list :area "axis" :view id :axis channel :part "title" :title (plist-get tm :text)))
            areas))
    (seq-do-indexed
     (lambda (tk i)
       (let* ((label (plist-get tk :label))
              (lbox (and (stringp label) (not (string-empty-p label))
                         (eas-action-callback--text-box scene label (plist-get tk :lx) (plist-get tk :ly)
                                                        (plist-get tk :align) (plist-get tk :baseline) size
                                                        (if horizontal (plist-get axis :labelAngle) 0))))
              (seg (plist-get tk :tick))
              (tbox (and (vectorp seg)
                         (let ((pad eas-action-callback--area-pad))
                           (vector (- (min (aref seg 0) (aref seg 2)) pad) (- (min (aref seg 1) (aref seg 3)) pad)
                                   (+ (abs (- (aref seg 2) (aref seg 0))) pad pad)
                                   (+ (abs (- (aref seg 3) (aref seg 1))) pad pad)))))
              (box (eas-action-callback--union lbox tbox)))
         (when box
           (push (list :box box :id (format "eas-axis:%s|%s|%d" id channel i) :help (and (stringp label) label)
                       :target (list :area "axis" :view id :axis channel :part "label"
                                     :value (plist-get tk :value) :label (or label :null)))
                 areas))))
     (plist-get axis :ticks))
    (nreverse areas)))

(defun eas-action-callback--title-areas (scene)
  "Areas of SCENE's title and subtitle, and of each view's header."
  (let ((title (plist-get scene :title)) areas)
    (dolist (part (and title (list (cons "title" title) (cons "subtitle" (plist-get title :subtitle)))))
      (when-let* ((tt (cdr part)) ((stringp (plist-get tt :text))))
        (push (list :box (eas-action-callback--text-box scene (plist-get tt :text) (plist-get tt :x) (plist-get tt :y)
                                                        (plist-get tt :align) (plist-get tt :baseline)
                                                        (or (plist-get tt :fontSize) 13) (plist-get tt :angle)
                                                        "center")
                    :id (format "eas-title:%s" (car part)) :help (plist-get tt :text)
                    :target (list :area "title" :part (car part) :title (plist-get tt :text)))
              areas)))
    (seq-doseq (view (plist-get scene :views))
      (when-let* ((h (plist-get view :header)) ((stringp (plist-get h :text))))
        (push (list :box (eas-action-callback--text-box scene (plist-get h :text) (plist-get h :x) (plist-get h :y)
                                                        (plist-get h :align) (plist-get h :baseline)
                                                        (or (plist-get h :fontSize) 11) (plist-get h :angle)
                                                        "left")
                    :id (format "eas-title:%s|header" (plist-get view :id)) :help (plist-get h :text)
                    :target (list :area "title" :part "header" :view (plist-get view :id)
                                  :title (plist-get h :text)))
              areas)))
    (nreverse areas)))

(defun eas-action-callback-areas (scene)
  "SCENE's click areas that hold no datum, most specific first.
Each is (:box [x y w h] :id STRING :help TEXT :target PLIST): titles,
then each view's axis titles and labels, then each view's plot
background."
  (append (eas-action-callback--title-areas scene)
          (seq-mapcat (lambda (view)
                        (seq-mapcat (lambda (axis) (eas-action-callback--axis-areas scene view axis))
                                    (plist-get view :axes)))
                      (plist-get scene :views))
          (seq-map (lambda (view)
                     (list :box (plist-get view :bounds) :id (format "eas-background:%s" (plist-get view :id))
                           :target (list :area "background" :view (plist-get view :id))))
                   (plist-get scene :views))))

;;; Targets

(defun eas-action-callback--scene-view (scene id)
  "The view of SCENE with ID."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get scene :views)))

(defun eas-action-callback-data-xy (scene view-id px)
  "Data-space (:x X :y Y :fields (:x F :y F)) of PX in SCENE's view VIEW-ID.
A channel without a scale, or whose scale cannot invert, is left out."
  (when-let* ((view (eas-action-callback--scene-view scene view-id)))
    (let ((scales (plist-get view :scales)) out fields)
      (dolist (ch '((:x . 0) (:y . 1)))
        (when-let* ((scale (plist-get scales (car ch)))
                    (v (ignore-errors (eas-scale-invert scale (aref px (cdr ch))))))
          (setq out (append out (list (car ch) v)))
          (when (stringp (plist-get scale :field))
            (setq fields (append fields (list (car ch) (plist-get scale :field)))))))
      (append out (when fields (list :fields fields))))))

(defun eas-action-callback--extend (_view target scene)
  "Data-space :x and :y of TARGET's click when it lies in its plot of SCENE."
  (let* ((px (plist-get target :px))
         (view (and (vectorp px) (eas-action-callback--scene-view scene (plist-get target :view)))))
    (when (and view (or (plist-get target :mark) (equal (plist-get target :area) "background"))
               (eas-hit--contains (plist-get view :bounds) (aref px 0) (aref px 1)))
      (eas-action-callback-data-xy scene (plist-get target :view) px))))

(defun eas-action-callback-area-at (scene px)
  "The first of SCENE's click areas holding PX, or nil."
  (seq-find (lambda (a) (eas-hit--contains (plist-get a :box) (aref px 0) (aref px 1)))
            (eas-action-callback-areas scene)))

(defun eas-action-callback--area-target (view px scene)
  "The area target a click at PX in SCENE made, when VIEW binds it."
  (when-let* ((area (eas-action-callback-area-at scene px)))
    (let* ((target (append (plist-get area :target) (list :px px)))
           (probe (append target (eas-action-callback--extend view target scene))))
      (and (eas-action-binding-for view probe) target))))

;;; GUI hot spots

(defun eas-action-callback-hot-spots (scene)
  "Image :map areas for SCENE's axis, title and background click areas.
Retained while the areas stay the same (`eas-svg-retain-part')."
  (let ((areas (eas-action-callback-areas scene)))
    (eas-svg-retain-part '(action-hot-spots) areas
                         (lambda () (eas-action-callback--hot-spots-1 areas)))))

(defun eas-action-callback--hot-spots-1 (areas)
  "Image :map areas for click AREAS (`eas-action-callback-areas')."
  (mapcar (lambda (a)
            (let ((b (plist-get a :box)))
              (list (cons 'rect (cons (cons (round (aref b 0)) (round (aref b 1)))
                                      (cons (round (+ (aref b 0) (aref b 2))) (round (+ (aref b 1) (aref b 3))))))
                    (intern (plist-get a :id))
                    (append (when (plist-get a :help) (list 'help-echo (plist-get a :help)))
                            (list 'pointer (if (equal (plist-get (plist-get a :target) :area) "background")
                                               'arrow 'hand))))))
          areas))

(setq eas-action-pick-function #'eas-action-callback-pick
      eas-action-default-tables-function #'eas-action-callback--tables)
(add-hook 'eas-action-area-target-functions #'eas-action-callback--area-target)
(add-hook 'eas-action-target-extend-functions #'eas-action-callback--extend)
(add-hook 'eas-svg-hot-spot-functions #'eas-action-callback-hot-spots)

(provide 'eas-action-callback)
;;; eas-action-callback.el ends here
