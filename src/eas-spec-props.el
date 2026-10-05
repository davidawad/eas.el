;;; eas-spec-props.el --- which Vega-Lite style properties eas honors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L2.  `eas-spec-features' names the marks, channels, scales and
;; transforms a spec uses; this file covers the properties that only
;; style them: mark properties per mark type, axis, legend, scale and
;; title properties, config.view and top-level config (fc-qx1.43).
;; The vocabulary (eas-spec-props-vocab.el) lists each scope's Vega-Lite
;; 6.4.1 properties with a non-default probe value;
;; `eas-spec-props-honored' lists those the native engine draws.
;;
;; `eas-spec-props-findings' walks a spec and returns an
;; UNSUPPORTED_FEATURE finding (with :property t and its JSON path) for
;; each property outside the honored list, so `check' names it.  These
;; findings do not stop compile: the chart is drawn without that
;; property.
;;
;; `eas-spec-props-audit' keeps the honored list honest: for every
;; property of a scope it compiles small base charts with and without
;; the probe value and counts the property honored when the SVG
;; changes.  ERT re-runs it, so a property stays listed only while
;; it has an effect.  Properties that never change Vega's static picture
;; are `eas-spec-props-inert' instead.

;;; Code:

(require 'eas-core)
(require 'eas-spec-props-vocab)
(require 'eas-spec-props-lower)

(declare-function eas-compile "eas-compile")
(declare-function eas-svg-render "eas-svg")
(defvar eas-spec-supported-function)

;;; What eas honors (each entry proven by `eas-spec-props-audit')

(defconst eas-spec-props-honored
  '(("bar" cornerRadius cornerRadiusEnd cornerRadiusTopLeft cornerRadiusTopRight cornerRadiusBottomLeft cornerRadiusBottomRight orient width height size color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeCap strokeJoin clip filled xOffset yOffset x2Offset y2Offset)
    ("line" interpolate tension point orient size color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeDashOffset strokeCap strokeJoin strokeMiterLimit blend clip)
    ("area" interpolate tension point line orient color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeDashOffset strokeCap strokeJoin strokeMiterLimit blend clip filled)
    ("point" shape size angle color fill stroke opacity fillOpacity strokeOpacity strokeWidth clip filled xOffset yOffset)
    ("rule" size color stroke opacity strokeWidth strokeDash strokeCap clip xOffset yOffset x2Offset)
    ("tick" thickness (orient "vertical") size color stroke opacity strokeWidth strokeDash strokeCap clip xOffset yOffset)
    ("text" align baseline dx dy angle font fontSize fontStyle fontWeight limit lineHeight ellipsis text radius color fill opacity clip xOffset yOffset)
    (axis bandPosition domain domainColor domainDash domainOpacity domainWidth format grid gridColor gridDash gridOpacity gridWidth labelAlign labelAngle labelBaseline labelColor labelExpr labelFlush labelFont labelFontSize labelFontStyle labelFontWeight labelLimit labelOffset labelOpacity labelPadding labels minExtent offset orient tickBand tickColor tickCount tickDash tickOpacity tickSize tickWidth ticks title titleAlign titleAngle titleBaseline titleColor titleFont titleFontSize titleFontStyle titleFontWeight titleLimit titleOpacity titlePadding titleX titleY values)
    (legend clipHeight columns direction format gradientLength gradientThickness labelColor labelExpr labelFont labelFontSize labelFontStyle labelFontWeight labelOffset labelOpacity legendX legendY offset orient rowPadding symbolOpacity symbolSize symbolStrokeColor symbolStrokeWidth symbolType title titleColor titleFont titleFontSize titleFontStyle titleFontWeight titleOpacity titlePadding values)
    (scale type domain domainMax domainMin domainMid range rangeMax rangeMin scheme reverse nice zero padding paddingInner paddingOuter)
    (title text subtitle anchor color dx dy fontSize fontWeight frame offset (orient "top") subtitleColor subtitleFontSize subtitleFontWeight subtitlePadding)
    (view stroke strokeWidth strokeDash strokeOpacity fill fillOpacity continuousWidth continuousHeight step)
    (config background padding font countTitle)
    (config-axis bandPosition domain domainColor domainDash domainOpacity domainWidth grid gridColor gridDash gridOpacity gridWidth labelAngle labelColor labelFlush labelFont labelFontSize labelFontStyle labelFontWeight labelLimit labelOpacity labelPadding labels minExtent offset tickBand tickColor tickDash tickOpacity tickSize tickWidth ticks title titleAlign titleAngle titleBaseline titleColor titleFont titleFontSize titleFontStyle titleFontWeight titleLimit titleOpacity titlePadding titleX titleY)
    (config-legend (columns 1) direction gradientThickness labelColor labelFont labelFontSize labelFontStyle labelFontWeight labelOffset labelOpacity offset (orient "right" "left" "top" "bottom" "top-left" "top-right" "bottom-left" "bottom-right") rowPadding symbolSize symbolStrokeWidth symbolType titleColor titleFont titleFontSize titleFontStyle titleFontWeight titleOpacity titlePadding)
    (config-title anchor color dx dy fontSize fontWeight offset (orient "top"))
    ("config.bar" cornerRadius cornerRadiusEnd cornerRadiusTopLeft cornerRadiusTopRight cornerRadiusBottomLeft cornerRadiusBottomRight orient width size color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeCap strokeJoin clip filled xOffset yOffset y2Offset)
    ("config.line" interpolate tension orient size color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeDashOffset strokeCap strokeJoin strokeMiterLimit blend clip)
    ("config.area" interpolate tension orient color fill stroke opacity fillOpacity strokeOpacity strokeWidth strokeDash strokeDashOffset strokeCap strokeJoin strokeMiterLimit blend clip filled)
    ("config.point" shape size angle color fill stroke opacity fillOpacity strokeOpacity strokeWidth clip filled xOffset yOffset)
    ("config.rule" size color stroke opacity strokeWidth strokeDash strokeCap clip xOffset yOffset x2Offset)
    ("config.tick" thickness (orient "vertical") size color stroke opacity strokeWidth strokeDash strokeCap clip xOffset yOffset)
    ("config.text" align baseline dx dy angle font fontSize fontStyle fontWeight limit ellipsis color fill opacity clip xOffset yOffset))
  "Alist (SCOPE . KEYS) of the properties eas draws, per scope.")

(defun eas-spec-props-honored-p (scope key &optional value)
  "Non-nil when property KEY (a symbol) of SCOPE is drawn or inert.
An enumerated property is honored for the VALUEs its entry lists."
  (or (memq key eas-spec-props-inert)
      (seq-some (lambda (e) (if (consp e) (and (eq (car e) key) (member value (cdr e))) (eq e key)))
                (cdr (assoc scope eas-spec-props-honored)))))

;;; Findings

(defconst eas-spec-props--marks '("bar" "line" "area" "point" "rule" "tick" "text")
  "Mark types whose properties are audited (the rest are not judged here).")

(defconst eas-spec-props--config-blocks
  '((:axis . config-axis) (:axisX . config-axis) (:axisY . config-axis) (:legend . config-legend)
    (:title . config-title) (:view . view))
  "Config blocks and the scope their properties belong to.")

(defun eas-spec-props--finding (scope key path)
  "An UNSUPPORTED_FEATURE finding for property KEY of SCOPE at PATH."
  (let ((id (format "property/%s/%s" (if (stringp scope) (if (string-prefix-p "config." scope) scope (concat "mark." scope))
                                       (replace-regexp-in-string "-" "." (symbol-name scope)))
                    (eas-key-name key))))
    (list :code "UNSUPPORTED_FEATURE" :property t :feature id :path (concat path "/" (eas-key-name key))
          :message (format "%s is not honored natively; the chart is drawn without it" id))))

(defun eas-spec-props--object (scope obj path)
  "Findings for the properties of OBJ (in SCOPE, at PATH) eas does not draw."
  (when (eas-object-p obj)
    (let ((vocab (eas-spec-props-vocabulary scope)))
      (cl-loop for key in (eas-plist-keys obj)
               for sym = (intern (eas-key-name key))
               when (and (assq sym vocab)
                         (not (eas-spec-props-honored-p scope sym (plist-get obj key))))
               collect (eas-spec-props--finding scope key path)))))

(defun eas-spec-props--mark-type (mark)
  "Type of MARK (a string or a mark object), or nil."
  (if (stringp mark) mark (and (eas-object-p mark) (plist-get mark :type))))

(defun eas-spec-props--view (view path)
  "Findings for VIEW at PATH and the views nested in it."
  (when (eas-object-p view)
    (append
     (let ((mark (plist-get view :mark)))
       (when (and (eas-object-p mark) (member (eas-spec-props--mark-type mark) eas-spec-props--marks))
         (eas-spec-props--object (plist-get mark :type) (eas--plist-without mark :type) (concat path "/mark"))))
     (when (eas-object-p (plist-get view :title))
       (eas-spec-props--object 'title (plist-get view :title) (concat path "/title")))
     (let ((enc (plist-get view :encoding)))
       (when (eas-object-p enc)
         (cl-loop for ch in (eas-plist-keys enc)
                  for def = (plist-get enc ch)
                  for cpath = (concat path "/encoding/" (eas-key-name ch))
                  when (eas-object-p def)
                  append (append (eas-spec-props--object 'axis (plist-get def :axis) (concat cpath "/axis"))
                                 (eas-spec-props--object 'legend (plist-get def :legend) (concat cpath "/legend"))
                                 (eas-spec-props--object 'scale (plist-get def :scale) (concat cpath "/scale"))))))
     (cl-loop for key in '(:layer :vconcat :hconcat)
              for children = (plist-get view key)
              when (vectorp children)
              append (cl-loop for c across children for i from 0
                              append (eas-spec-props--view c (format "%s/%s/%d" path (eas-key-name key) i)))))))

(defun eas-spec-props--config (config)
  "Findings for the properties of CONFIG eas does not draw."
  (when (eas-object-p config)
    (cl-loop for key in (eas-plist-keys config)
             for value = (plist-get config key)
             for block = (cdr (assq key eas-spec-props--config-blocks))
             append (cond
                     (block (eas-spec-props--object block value (concat "/config/" (eas-key-name key))))
                     ((member (eas-key-name key) eas-spec-props--marks)
                      (eas-spec-props--object (concat "config." (eas-key-name key)) value
                                              (concat "/config/" (eas-key-name key))))
                     ((not (eas-object-p value))
                      (eas-spec-props--object 'config (list key value) "/config"))))))

(defun eas-spec-props-findings (spec)
  "UNSUPPORTED_FEATURE findings for the style properties of parsed SPEC
that eas does not draw, each with :property t and its JSON path."
  (append (eas-spec-props--view spec "") (eas-spec-props--config (plist-get spec :config))))

;;; The audit

(defconst eas-spec-props--rows
  [(:a "alpha" :b 3.3 :i 1 :j 2 :q 1) (:a "bravo" :b 7.1 :i 2 :j 4 :q 5)
   (:a "charlie" :b 5.2 :i 3 :j 5 :q 9) (:a "delta" :b 9.4 :i 4 :j 7 :q 13)]
  "Rows of the audit's base charts.")

(defconst eas-spec-props--null-row '(:a "echo" :b :null :i 5 :j 8 :q 2)
  "A row with an invalid measure, for the mark charts (invalid).")

(defun eas-spec-props--data (&optional nulls)
  "The audit's data, with the invalid row when NULLS."
  (list :values (if nulls (vconcat eas-spec-props--rows (list eas-spec-props--null-row)) eas-spec-props--rows)))

(defun eas-spec-props--encoding (type)
  "The base chart encoding for mark TYPE."
  (copy-tree
   (pcase type
     ((or "bar" "tick") '(:x (:field "a" :type "nominal") :y (:field "b" :type "quantitative")))
     ("rule" '(:x (:field "i" :type "quantitative") :x2 (:field "j") :y (:field "b" :type "quantitative")))
     (_ '(:x (:field "i" :type "quantitative") :y (:field "b" :type "quantitative"))))))

(defun eas-spec-props--mark (type)
  "The base mark object of TYPE (text marks draw a constant label)."
  (if (equal type "text") (list :type type :text "label") (list :type type)))

(defun eas-spec-props--with (plist entry &optional probe)
  "PLIST (an object or nil) with ENTRY's context and, with PROBE ((VALUE)),
ENTRY's key set to VALUE."
  (let ((out (copy-sequence (and (eas-object-p plist) plist))))
    (cl-loop for (k v) on (nth 2 entry) by #'cddr do (setq out (plist-put out (eas-key (symbol-name k)) v)))
    (when probe
      (setq out (plist-put out (eas-key (symbol-name (car entry))) (car probe))))
    out))

(defun eas-spec-props--in-channel (type channel key obj &optional config)
  "A TYPE chart whose CHANNEL definition carries KEY: OBJ, under CONFIG."
  (let ((enc (eas-spec-props--encoding type)))
    (append (list :data (eas-spec-props--data) :mark type
                  :encoding (if key (plist-put enc channel (plist-put (copy-sequence (plist-get enc channel)) key obj))
                              enc))
            (when config (list :config config)))))

(defun eas-spec-props--colored (field type key obj &optional config)
  "A point chart colored by FIELD of TYPE whose color carries KEY: OBJ."
  (append (list :data (eas-spec-props--data) :mark "point"
                :encoding (append (eas-spec-props--encoding "point")
                                  (list :color (append (list :field field :type type) (when key (list key obj))))))
          (when config (list :config config))))

(defun eas-spec-props--axis-charts (obj config)
  "Axis base charts: OBJ as a y, a nominal x and a continuous x axis, or CONFIG."
  (list (eas-spec-props--in-channel "bar" :y (and obj :axis) obj config)
        (eas-spec-props--in-channel "bar" :x (and obj :axis) obj config)
        (eas-spec-props--in-channel "point" :x (and obj :axis) obj config)))

(defun eas-spec-props--block (key obj)
  "A config holding OBJ as block KEY, or nil when OBJ is empty (an empty
block would replace the theme's)."
  (and obj (list key obj)))

(defun eas-spec-props--charts (scope entry probe)
  "Base charts for ENTRY of SCOPE (with PROBE, a (VALUE), when non-nil)."
  (let ((put (lambda (obj) (eas-spec-props--with obj entry probe))))
    (pcase scope
      ((pred (lambda (s) (and (stringp s) (string-prefix-p "config." s))))
       (let ((type (substring scope 7)))
         (list (list :data (eas-spec-props--data t) :mark (eas-spec-props--mark type)
                     :config (eas-spec-props--block (eas-key type) (funcall put nil)) :encoding (eas-spec-props--encoding type)))))
      ((pred stringp)
       (append (list (list :data (eas-spec-props--data t) :mark (funcall put (eas-spec-props--mark scope))
                           :encoding (eas-spec-props--encoding scope)))
               ;; A bar's height (and yOffset) matter on a horizontal bar, over a band y.
               (when (equal scope "bar")
                 (list (list :data (eas-spec-props--data) :mark (funcall put (eas-spec-props--mark scope))
                             :encoding '(:y (:field "a" :type "nominal") :x (:field "b" :type "quantitative")))))
               ;; A text mark's polar properties (radius, theta) only place it beside an arc.
               (when (equal scope "text")
                 (list (list :data (eas-spec-props--data)
                             :encoding '(:theta (:field "b" :type "quantitative" :stack t))
                             :layer (vector (list :mark (list :type "arc" :outerRadius 60))
                                            (list :mark (funcall put (eas-spec-props--mark scope)) :encoding
                                                  '(:text (:field "a" :type "nominal")))))))))
      ('axis (eas-spec-props--axis-charts (funcall put nil) nil))
      ('config-axis (eas-spec-props--axis-charts nil (eas-spec-props--block :axis (funcall put nil))))
      ('legend (list (eas-spec-props--colored "a" "nominal" :legend (funcall put nil))
                     (eas-spec-props--colored "q" "quantitative" :legend (funcall put nil))))
      ('config-legend (list (eas-spec-props--colored "a" "nominal" nil nil (eas-spec-props--block :legend (funcall put nil)))
                            (eas-spec-props--colored "q" "quantitative" nil nil (eas-spec-props--block :legend (funcall put nil)))))
      ('scale (list (list :data (eas-spec-props--data) :mark "arc"
                          :encoding (list :theta (list :field "b" :type "quantitative" :stack t :scale (funcall put nil))
                                          :color '(:field "a" :type "nominal")))
                    (eas-spec-props--in-channel "bar" :y :scale (funcall put nil))
                    (eas-spec-props--in-channel "bar" :x :scale (funcall put nil))
                    (eas-spec-props--in-channel "point" :x :scale (funcall put nil))
                    (eas-spec-props--colored "q" "quantitative" :scale (funcall put nil))
                    (eas-spec-props--colored "a" "nominal" :scale (funcall put nil))))
      ('title (list (list :data (eas-spec-props--data) :mark "bar" :title (funcall put (list :text "Title"))
                          :encoding (eas-spec-props--encoding "bar"))))
      ('config-title (list (list :data (eas-spec-props--data) :mark "bar" :title "Title"
                                 :config (eas-spec-props--block :title (funcall put nil)) :encoding (eas-spec-props--encoding "bar"))))
      ('view (mapcar (lambda (type) (list :data (eas-spec-props--data) :mark type
                                          :config (eas-spec-props--block :view (funcall put nil))
                                          :encoding (eas-spec-props--encoding type)))
                     '("bar" "point")))
      ('config (list (list :data (eas-spec-props--data) :mark "bar" :config (funcall put nil) :title "Title"
                           :encoding (eas-spec-props--encoding "bar"))
                     ;; config.countTitle names a count aggregate's axis.
                     (list :data (eas-spec-props--data) :mark "bar" :config (funcall put nil)
                           :encoding '(:x (:field "a" :type "nominal") :y (:aggregate "count" :type "quantitative"))))))))

(defvar eas-spec-props--pictures nil
  "Hash table of pictures by spec while an audit runs, else nil.")

(defun eas-spec-props--picture (spec)
  "What SPEC draws, as SVG, or (:error MESSAGE)."
  (let ((hit (and eas-spec-props--pictures (gethash spec eas-spec-props--pictures))))
    (or hit
        (let ((pic (condition-case err (eas-svg-render (eas-compile spec))
                     (error (list :error (error-message-string err))))))
          (when eas-spec-props--pictures (puthash spec pic eas-spec-props--pictures))
          pic))))

(defun eas-spec-props--effect-p (scope entry probe)
  "Non-nil when PROBE ((VALUE)) of ENTRY changes what a SCOPE base chart draws."
  (let ((eas-spec-supported-function nil))
    (cl-loop for base in (eas-spec-props--charts scope entry nil)
             for probed in (eas-spec-props--charts scope entry probe)
             thereis (let ((a (eas-spec-props--picture base)) (b (eas-spec-props--picture probed)))
                       (and (stringp b) (not (equal a b)))))))

(defun eas-spec-props--judge (scope entry)
  "ENTRY of SCOPE as it belongs in the honored list: its key, (KEY VALUES...)
for an enumeration honored in part, or nil."
  (let ((probe (nth 1 entry)))
    (pcase probe
      (`(:any . ,values) (and (seq-some (lambda (v) (eas-spec-props--effect-p scope entry (list v))) values)
                              (car entry)))
      (`(:one-of ,default . ,values)
       (let ((ok (seq-filter (lambda (v) (eas-spec-props--effect-p scope entry (list v))) values)))
         (if (= (length ok) (length values)) (car entry) (cons (car entry) (cons default ok)))))
      (_ (and (eas-spec-props--effect-p scope entry (list probe)) (car entry))))))

(defun eas-spec-props-audit (&optional scopes)
  "The honored list as the engine stands: alist (SCOPE . KEYS) of the
properties whose probe changes the picture.  SCOPES defaults to
`eas-spec-props-scopes'.  Inert properties are skipped."
  (let ((eas-spec-props--pictures (make-hash-table :test 'equal)))
    (mapcar (lambda (scope)
              (cons scope (delq nil (cl-loop for entry in (eas-spec-props-vocabulary scope)
                                             unless (memq (car entry) eas-spec-props-inert)
                                             collect (eas-spec-props--judge scope entry)))))
            (or scopes (eas-spec-props-scopes)))))

(provide 'eas-spec-props)
;;; eas-spec-props.el ends here
