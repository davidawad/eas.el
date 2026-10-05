;;; eas-spec.el --- chart/v1: the Vega-Lite subset eas reads -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L2.  chart/v1 is Vega-Lite 6.4.1 plus the "x-eas" key and
;; "x-eas:transform" transforms.  `eas-spec-parse' reads a spec
;; (string, file or value) and normalizes mark shorthand;
;; `eas-spec-features' walks it and names every feature it uses with
;; its JSON pointer; `eas-spec-check' turns that walk into findings
;; with stable reason codes instead of signalling, so check can report
;; them all at once.
;;
;; The vocabulary tables below are what the walker recognises.  Whether
;; the native engine can draw a recognised feature is decided by
;; `eas-spec-supported-features' (the conformance-backed list).

;;; Code:

(require 'eas-core)
(require 'seq)

(defconst eas-spec-vega-lite-version "6.4.1"
  "The Vega-Lite version chart/v1 follows (the version bin/chart pins).")

(defconst eas-spec-schema-url "https://vega.github.io/schema/vega-lite/v6.json"
  "The $schema resolve writes into specs that lack one.")

(defconst eas-spec--view-keys
  '(:$schema :data :mark :encoding :transform :layer :vconcat :hconcat
    :width :height :title :description :name :params :config :autosize
    :padding :background :resolve :usermeta :x-eas :spacing)
  "Keys a chart/v1 view or composition may carry.")

(defconst eas-spec--marks
  '("point" "circle" "square" "line" "area" "bar" "rect" "rule" "tick" "text"
    "arc"
    ;; composite marks, expanded by eas-composite.el (fc-qx1.26)
    "errorbar" "errorband"
    "trail")
  "Mark types chart/v1 recognises.")

(defconst eas-spec--mark-keys
  '(:type :color :fill :stroke :opacity :fillOpacity :strokeOpacity
    :strokeWidth :strokeDash :size :filled :interpolate :point :line :tooltip
    :clip :orient :width :height :cornerRadius :align :baseline :dx :dy
    :fontSize :fontWeight :text :thickness :binSpacing :invalid
    :innerRadius :outerRadius :padAngle :radius :radius2 :theta :theta2
    :radiusOffset :thetaOffset
    ;; distributions (fc-qx1.26)
    :style :cornerRadiusEnd :extent :ticks :rule :median :outliers :box
    :aria :description :strokeCap :strokeJoin)
  "Mark properties chart/v1 recognises.")

(defconst eas-spec--channels
  '(:x :y :x2 :y2 :color :fill :stroke :opacity :size :tooltip :href :text
    :detail :order
    :theta :radius
    ;; distributions (fc-qx1.26)
    :shape :row
    :strokeDash)
  "Encoding channels chart/v1 recognises.")

(defconst eas-spec--channel-def-keys
  '(:field :type :aggregate :bin :timeUnit :title :scale :axis :legend :sort
    :stack :format :value :datum :condition :param :empty
    ;; distributions (fc-qx1.26)
    :header)
  "Field/value definition keys chart/v1 recognises.")

(defconst eas-spec--types '("quantitative" "temporal" "ordinal" "nominal")
  "Vega-Lite measurement types.")

(defconst eas-spec--scale-types
  '("linear" "log" "time" "utc" "band" "point" "ordinal"
    "sqrt")
  "Scale types chart/v1 recognises.")

(defconst eas-spec--scale-keys
  '(:type :domain :range :zero :nice :padding :paddingInner :paddingOuter
    :reverse :scheme :clamp :base :domainMin :domainMax
    :rangeMin :rangeMax :exponent)
  "Scale properties chart/v1 recognises.")

(defconst eas-spec--transforms
  '(:filter :calculate :aggregate :window :fold :timeUnit :bin :joinaggregate
    :x-eas:transform
    ;; distributions (fc-qx1.26)
    :flatten :density
    :pivot)
  "Transform keys chart/v1 recognises; the first key present names it.")

(defconst eas-spec--select-keys
  '(:type :on :nearest :fields :encodings :clear :toggle :resolve :mark)
  "Selection definition keys chart/v1 recognises.")

;;; Parse

(defvar eas-spec-rewrite-functions nil
  "Functions (SPEC) -> SPEC that `eas-spec-parse' applies in order.
Each lowers Vega-Lite sugar the native compiler does not read (data
URLs, repeat, mark overlays) into the subset it does, and must be
idempotent: parse runs again on its own output.")

(defvar eas-spec-source-directory nil
  "Directory of the spec file being parsed, for relative data URLs.")

(defun eas-spec-parse (input)
  "Parse chart/v1 INPUT into its normalized internal form.
INPUT is a JSON string, a file name ending in .json, or an already
parsed value.  Mark shorthand (\"bar\") becomes (:type \"bar\").
Signals PARSE_ERROR or INVALID_INPUT."
  (let* ((file (and (stringp input) (string-suffix-p ".json" input)
                    (not (string-prefix-p "{" (string-trim-left input)))
                    input))
         (spec (cond (file (eas-json-read-file input))
                     ((stringp input) (eas-json-parse input))
                     (t input))))
    (unless (and (eas-object-p spec) spec)
      (eas-signal "INVALID_INPUT"
                    "A chart/v1 spec must be a JSON object with a mark, layer, vconcat or hconcat"
                    :path ""))
    (let ((eas-spec-source-directory (if file (file-name-directory (expand-file-name file))
                                       eas-spec-source-directory)))
      (dolist (f eas-spec-rewrite-functions) (setq spec (funcall f spec))))
    (eas-spec--normalize spec)))

(defun eas-spec--normalize (spec)
  "Return SPEC with mark shorthand expanded, recursively."
  (let ((out spec))
    (when (stringp (plist-get spec :mark))
      (setq out (eas-plist-put out :mark (list :type (plist-get spec :mark)))))
    (dolist (key '(:layer :vconcat :hconcat))
      (when (vectorp (plist-get spec key))
        (setq out (eas-plist-put out key (vconcat (mapcar #'eas-spec--normalize
                                                            (plist-get spec key)))))))
    out))

(defun eas-spec-mark-type (spec)
  "Return the mark type string of unit SPEC, or nil."
  (let ((mark (plist-get spec :mark)))
    (if (stringp mark) mark (plist-get mark :type))))

(defconst eas-spec--transform-params
  '(:as :field :groupby :sort :frame :ignorePeers :extent :maxbins :param :empty
    :value :op :limit)
  "Keys that parameterize a transform rather than name it.")

(defun eas-spec--transform-key (tr)
  "The key naming transform TR.
A known operation key wins; else the first key that is not a parameter."
  (let ((keys (eas-plist-keys tr)))
    (or (seq-find (lambda (k) (memq k eas-spec--transforms)) keys)
        (seq-find (lambda (k) (not (memq k eas-spec--transform-params))) keys)
        (car keys))))

;;; Feature walk

(defun eas-spec-features (spec)
  "Return every feature SPEC uses, as plists (:feature ID :path POINTER).
IDs look like \"mark/bar\", \"encoding/color\", \"scale/log\",
\"transform/aggregate\", \"param/interval\", \"bind/scales\" and
\"composition/layer\".  Keys outside the chart/v1 vocabulary come back
as (:feature \"key/NAME\" :path P :unknown t).  Structural problems
come back as (:invalid MESSAGE :path P)."
  (let (found)
    (cl-labels
        ((add (feature path &rest props)
           (push (append (list :feature feature :path path) props) found))
         (bad (message path) (push (list :invalid message :path path) found))
         (unknown (key path) (add (concat "key/" (eas-key-name key))
                                  (concat path "/" (eas-key-name key)) :unknown t))
         (check-keys (plist allowed path)
           (dolist (key (eas-plist-keys plist))
             (unless (memq key allowed) (unknown key path))))
         (walk-view (view path)
           (cond
            ((not (and view (eas-object-p view)))
             (bad "Each view must be a JSON object" path))
            ((plist-get view :x-eas:when)
             (walk-view (plist-get view :spec) (concat path "/spec")))
            (t
             (check-keys view eas-spec--view-keys path)
             (let ((composite nil))
               (dolist (key '(:layer :vconcat :hconcat))
                 (when-let* ((children (plist-get view key)))
                   (setq composite t)
                   (add (concat "composition/" (eas-key-name key))
                        (concat path "/" (eas-key-name key)))
                   (if (not (vectorp children))
                       (bad (format "%s must be an array of views" (eas-key-name key))
                            (concat path "/" (eas-key-name key)))
                     (cl-loop for child across children for i from 0
                              do (walk-view child (format "%s/%s/%d" path
                                                          (eas-key-name key) i))))))
               (when (plist-get view :mark) (walk-mark (plist-get view :mark) path))
               (unless (or composite (plist-get view :mark))
                 (bad "A view needs a mark, or a layer, vconcat or hconcat" path)))
             (walk-encoding (plist-get view :encoding) (concat path "/encoding"))
             (walk-transforms (plist-get view :transform) (concat path "/transform"))
             (walk-params (plist-get view :params) (concat path "/params")))))
         (walk-mark (mark path)
           (let ((type (if (stringp mark) mark (plist-get mark :type)))
                 (mpath (concat path "/mark")))
             (cond ((plist-get type :x-eas:slot) (add "mark/slot" mpath))
                   ((not (stringp type)) (bad "mark needs a type string" mpath))
                   ((member type eas-spec--marks) (add (concat "mark/" type) mpath))
                   (t (add (concat "mark/" type) mpath :unknown t)))
             (unless (stringp mark) (check-keys mark eas-spec--mark-keys mpath))))
         (walk-encoding (encoding path)
           (when encoding
             (if (not (eas-object-p encoding))
                 (bad "encoding must be an object" path)
               (dolist (channel (eas-plist-keys encoding))
                 (let ((cpath (concat path "/" (eas-key-name channel)))
                       (def (plist-get encoding channel)))
                   (if (not (memq channel eas-spec--channels))
                       (add (concat "encoding/" (eas-key-name channel)) cpath :unknown t)
                     (add (concat "encoding/" (eas-key-name channel)) cpath)
                     (if (vectorp def)
                         (cl-loop for d across def for i from 0
                                  do (walk-def d (format "%s/%d" cpath i)))
                       (walk-def def cpath))))))))
         (walk-def (def path)
           (if (not (eas-object-p def))
               (bad "An encoding channel must be an object (or an array for tooltip)" path)
             (check-keys def eas-spec--channel-def-keys path)
             (when-let* ((type (plist-get def :type)))
               (unless (member type eas-spec--types)
                 (bad (format "type must be one of %s" (string-join eas-spec--types ", "))
                      (concat path "/type"))))
             (when (plist-get def :condition)
               (add "encoding/condition" (concat path "/condition")))
             (when (eas-true-p (plist-get def :bin)) (add "encoding/bin" (concat path "/bin")))
             (when (plist-get def :aggregate)
               (add "encoding/aggregate" (concat path "/aggregate")))
             (when (plist-get def :timeUnit)
               (add "encoding/timeUnit" (concat path "/timeUnit")))
             (let ((scale (plist-get def :scale)))
               (when (and scale (eas-object-p scale))
                 (check-keys scale eas-spec--scale-keys (concat path "/scale"))
                 (when-let* ((type (plist-get scale :type)))
                   (add (concat "scale/" type) (concat path "/scale/type")
                        :unknown (not (member type eas-spec--scale-types))))))))
         (walk-transforms (transforms path)
           (when transforms
             (if (not (vectorp transforms))
                 (bad "transform must be an array" path)
               (cl-loop for tr across transforms for i from 0
                        for tpath = (format "%s/%d" path i)
                        for key = (and (eas-object-p tr) (eas-spec--transform-key tr))
                        do (cond
                            ((not (and tr (eas-object-p tr)))
                             (bad "Each transform must be an object" tpath))
                            ((plist-get tr :x-eas:transform)
                             (add "transform/x-eas" tpath
                                  :transform (plist-get tr :x-eas:transform)))
                            ((memq key eas-spec--transforms)
                             (add (concat "transform/" (eas-key-name key)) tpath))
                            (t (add (concat "transform/" (eas-key-name key)) tpath
                                    :unknown t)))))))
         (walk-params (params path)
           (when params
             (if (not (vectorp params))
                 (bad "params must be an array" path)
               (cl-loop for param across params for i from 0
                        for ppath = (format "%s/%d" path i)
                        do (walk-param param ppath)))))
         (walk-param (param ppath)
           (unless (stringp (plist-get param :name))
             (bad "Each param needs a name" ppath))
           (let ((select (plist-get param :select))
                 (bind (plist-get param :bind)))
             (cond
              ((plist-get param :expr) (add "param/expr" ppath :unknown t))
              ((null select) (add "param/value" ppath))
              (t (let ((type (if (stringp select) select (plist-get select :type))))
                   (add (concat "param/" type) (concat ppath "/select")
                        :unknown (not (member type '("point" "interval"))))
                   (when (eas-object-p select)
                     (check-keys select eas-spec--select-keys
                                 (concat ppath "/select"))))))
             (cond ((null bind))
                   ((member bind '("scales" "legend"))
                    (add (concat "bind/" bind) (concat ppath "/bind")))
                   ((and (eas-object-p bind) (plist-get bind :input))
                    (add "bind/input" (concat ppath "/bind") :unknown t))
                   (t (add "bind/other" (concat ppath "/bind") :unknown t))))))
      (walk-view spec ""))
    (nreverse found)))

;;; Check

(defvar eas-spec-supported-function nil
  "Function of no arguments returning the supported feature IDs, or nil.
When nil every recognised feature counts as supported.  Conformance
\(`eas-conformance') sets it to read supported.json.")

(defun eas-spec-supported-features ()
  "Return the list of supported feature IDs, or t when unrestricted."
  (if eas-spec-supported-function (funcall eas-spec-supported-function) t))

(defun eas-spec-check (spec)
  "Return findings for chart/v1 SPEC; nil when it is fully supported.
SPEC may be any input `eas-spec-parse' accepts.  Each finding is a
plist (:code CODE :message M :path P [:feature ID]).  Parse failures
are returned as a single finding rather than signalled."
  (condition-case err
      (let ((supported (eas-spec-supported-features))
            findings)
        (dolist (f (eas-spec-features (eas-spec-parse spec)))
          (cond
           ((plist-get f :invalid)
            (push (list :code "INVALID_INPUT" :message (plist-get f :invalid)
                        :path (plist-get f :path))
                  findings))
           ((or (plist-get f :unknown)
                (and (listp supported)
                     (not (member (plist-get f :feature) supported))))
            (push (list :code "UNSUPPORTED_FEATURE"
                        :message (format "%s is not in the native subset; the chart falls back to a static image"
                                         (plist-get f :feature))
                        :path (plist-get f :path) :feature (plist-get f :feature))
                  findings))))
        (nreverse findings))
    (eas-error (list (eas-error-plist err)))))

(defun eas-spec-validate (spec)
  "Parse SPEC and signal the first INVALID_INPUT finding; return the spec.
Unsupported features are not errors here: they decide the backend."
  (let ((parsed (eas-spec-parse spec)))
    (dolist (f (eas-spec-features parsed))
      (when (plist-get f :invalid)
        (eas-signal "INVALID_INPUT" (plist-get f :invalid) :path (plist-get f :path))))
    parsed))

(defun eas-spec-unsupported (spec)
  "Return the UNSUPPORTED_FEATURE findings of SPEC."
  (seq-filter (lambda (f) (equal (plist-get f :code) "UNSUPPORTED_FEATURE"))
              (eas-spec-check spec)))

(provide 'eas-spec)
;;; eas-spec.el ends here
