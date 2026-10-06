;;; eas-view.el --- view/v1: live views, dispatch, replay, inspect -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A view is one chart someone (or something) is looking
;; at: {id, spec-hash, bindings, state, log}.  Ids are stable and
;; readable ("line:daily", "ohlc:TSM").  `eas-dispatch' applies an
;; event/v1 through the pure reducer, re-compiles only when the visible
;; output changes, appends to a bounded log and returns `eas-inspect'.
;; `eas-replay' re-applies a log.  None of this needs a display, so
;; ERT and agents drive every interaction headlessly.  Buffer glue
;; (eas-mode.el) only translates native events and redraws.

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-resolve)
(require 'eas-compile)
(require 'eas-compile-patch)
(require 'eas-event)
(require 'eas-reduce)
(require 'eas-params)
(require 'eas-adapters)
(require 'eas-chart)
(require 'eas-tip)
(require 'eas-strip)

(defvar eas-views (make-hash-table :test 'equal)
  "Live views by id.")

(defvar eas-view-log-size 200
  "Events kept in each view's log.")

(defvar eas-view-changed-functions nil
  "Hook run with a VIEW after dispatch changed its visible output.")

(defvar eas-view-dispatch-functions nil
  "Hook run with VIEW, EVENT, OLD-STATE and OLD-SCENE after every dispatch.
It runs once the reducer has updated VIEW and before the inspect is
taken, so a function may record data in VIEW's state (clicks, fc-qx1.1).")

(defvar eas-view-replaying nil
  "Non-nil while `eas-replay' re-applies a log.
Hook functions with side effects (actions, echo) skip them then.")

(defvar eas-push-function nil
  "Function that takes over `eas-push' with VIEW and ROWS, or nil.
When non-nil, `eas-push' calls it instead of dispatching a push now.
eas-stream sets it to coalesce live data.")

(cl-defstruct (eas-view (:constructor eas-view--make) (:copier nil))
  id template subject spec spec-hash bindings data size target cell
  state log (seq 0) scene plan buffer (interactive t) fallback warnings)

(defun eas-view--unique-id (base)
  "BASE, or BASE<N> when BASE is taken."
  (if (not (gethash base eas-views)) base
    (cl-loop for n from 2 for id = (format "%s<%d>" base n)
             unless (gethash id eas-views) return id)))

(defun eas-view-get (view)
  "Return VIEW (an id or view) or signal VIEW_NOT_FOUND."
  (cond ((eas-view-p view) view)
        ((gethash view eas-views))
        (t (eas-signal "VIEW_NOT_FOUND"
                         (format "No view %S; live views: %s" view
                                 (let (ids) (maphash (lambda (k _) (push k ids)) eas-views)
                                      (if ids (string-join (sort ids #'string<) ", ") "none")))
                         :view view))))

(defun eas-view--compile (view &optional patch old-state)
  "Compile VIEW's scene from its spec, data and state.
With PATCH (data and size unchanged since OLD-STATE), try patching the
cached plan for a selection-only change before compiling from scratch."
  (let ((state (eas-view-state view)))
    (eas-params-with-state state
      (let ((plan (or (and patch (eas-view-plan view)
                           (eas-compile-patch (eas-view-plan view) old-state state))
                      (eas-compile-plan (eas-view-spec view)
                                          :rows (plist-get (eas-view-data view) :rows)
                                          :size (eas-view-size view) :target (eas-view-target view)
                                          :cell (eas-view-cell view) :state state))))
        (setf (eas-view-plan view) plan)
        (eas-compile-scene plan state)))))

(defun eas-view--root-rows (spec)
  "Inline root rows of resolved SPEC, or nil."
  (let ((values (plist-get (plist-get spec :data) :values)))
    (and (vectorp values) values)))

(cl-defun eas-view-open (source &key id subject bindings rows size target cell)
  "Open a live view of SOURCE and register it; return the view.
SOURCE is a template name (resolved with BINDINGS) or a chart/v1 spec.
SUBJECT names what is shown (\"TSM\") for a readable id
TEMPLATE:SUBJECT; ID overrides.  ROWS replaces the root data.  SIZE,
TARGET and CELL are as in `eas-compile'."
  (let* ((template (and (stringp source) (not (string-prefix-p "{" (string-trim-left source)))
                        (not (string-suffix-p ".json" source))
                        source))
         (spec (if template (eas-resolve template bindings) (eas-resolve-spec source)))
         (data (eas-data-make (or rows (eas-view--root-rows spec) []) ))
         (view (eas-view--make
                :id (eas-view--unique-id (or id (if subject (format "%s:%s" (or template "chart") subject)
                                                    (or template "chart"))))
                :template template :subject subject :spec spec :spec-hash (eas-resolve-hash spec)
                :bindings bindings :data data :size size :target (or target 'svg) :cell cell
                :state nil :log nil)))
    (condition-case err
        (progn
          (setf (eas-view-scene view) (eas-view--compile view))
          ;; Selections with an initial value start non-empty.
          (when-let* ((init (eas-params-initial-state (eas-view-scene view))))
            (setf (eas-view-state view) init
                  (eas-view-scene view) (eas-view--compile view))))
      (eas-unsupported-feature (eas-view--fallback view err)))
    (puthash (eas-view-id view) view eas-views)
    view))

(defun eas-view--fallback (view err)
  "Make VIEW a static, non-interactive view after unsupported-feature ERR.
Every unsupported path becomes a warning.  The picture comes from
bin/chart only when `eas-static-fallback' is non-nil; otherwise the
fallback is the UNSUPPORTED_FEATURE error itself."
  (let ((spec (eas-view-spec view)))
    (setf (eas-view-interactive view) nil
          (eas-view-warnings view)
          (vconcat (or (let ((eas-spec-supported-function eas-spec-supported-function))
                         (mapcar (lambda (f) (list :code "UNSUPPORTED_FEATURE" :path (plist-get f :path)
                                                   :feature (plist-get f :feature) :message (plist-get f :message)))
                                 (eas-spec-unsupported spec)))
                       (list (eas-error-plist err))))
          (eas-view-fallback view)
          (if (not eas-static-fallback) (list :error (eas-static-fallback-error err))
            (condition-case e (list :type "svg" :data (eas-chart-build spec "svg"))
              (eas-error (list :error (eas-error-plist e))))))))

(defun eas-view-close (view)
  "Forget VIEW."
  (remhash (eas-view-id (eas-view-get view)) eas-views))

(defun eas-view-ids ()
  "Ids of live views, sorted."
  (let (ids) (maphash (lambda (k _) (push k ids)) eas-views) (sort ids #'string<)))

(defun eas-view--visible (view)
  "The parts of VIEW's state that change what is drawn."
  (let ((state (eas-view-state view)))
    (list (plist-get state :domains) (plist-get state :params)
          (length (plist-get (eas-view-data view) :rows)))))

(defun eas-view--log (view event)
  "Append EVENT to VIEW's bounded log."
  (cl-incf (eas-view-seq view))
  (setf (eas-view-log view)
        (seq-take (cons (list :seq (eas-view-seq view) :type (plist-get event :type)
                              :summary (eas-event-describe event) :event event)
                        (eas-view-log view))
                  eas-view-log-size)))

(defun eas-view-resize (view size &optional target)
  "Recompile VIEW at SIZE (and TARGET); return its scene (nil when static)."
  (let ((view (eas-view-get view)))
    (setf (eas-view-size view) size)
    (when target (setf (eas-view-target view) target))
    (when (eas-view-interactive view)
      (setf (eas-view-scene view) (eas-view--compile view)))))

(defun eas-dispatch (view event)
  "Apply EVENT (event/v1) to VIEW and return the new `eas-inspect'.
Events are validated (EVENT_INVALID); push rows are schema-checked
\(SHAPE_INVALID).  The scene is recompiled only when domains, selections
or data changed."
  (if (not (eas-view-interactive (eas-view-get view)))
      (let ((view (eas-view-get view)))
        ;; Static fallback: record what was asked, change nothing.
        (eas-view--log view (eas-event-parse event))
        (eas-inspect view))
    (eas-view--dispatch (eas-view-get view) event)))

(defun eas-view--dispatch (view event)
  "Apply EVENT to interactive VIEW and return the new inspect."
  (let* ((event (eas-event-parse event))
         (before (eas-view--visible view))
         (old-state (eas-view-state view))
         (old-scene (eas-view-scene view))
         (push (equal (plist-get event :type) "push")))
    (when push
      (setf (eas-view-data view)
            (eas-view--window (eas-data-append (eas-view-data view) (vconcat (plist-get event :rows)))
                                (plist-get event :window))))
    (setf (eas-view-state view) (eas-reduce old-state event (eas-view-scene view)))
    (eas-view--log view event)
    ;; A windowed push can keep the row count while changing the rows.
    (unless (and (equal before (eas-view--visible view))
                 (not (and push (> (length (plist-get event :rows)) 0))))
      (setf (eas-view-scene view) (eas-view--compile view (not push) old-state))
      (run-hook-with-args 'eas-view-changed-functions view))
    (run-hook-with-args 'eas-view-dispatch-functions view event old-state old-scene)
    (eas-inspect view)))

(defun eas-view--window (data window)
  "DATA keeping only its last WINDOW rows (all when WINDOW is nil)."
  (let ((rows (plist-get data :rows)))
    (if (and window (> (length rows) window))
        (eas-plist-put data :rows (seq-subseq rows (- (length rows) window)))
      data)))

(defun eas-replay (view log)
  "Re-apply LOG (events, or log entries with :event) to VIEW, oldest first.
LOG may be a vector or list, in order, or a view's own `eas-view-log'
\(newest first, detected by descending :seq).  Returns the final inspect."
  (let* ((entries (append log nil))
         (entries (if (and (cdr entries) (plist-get (car entries) :seq)
                           (> (plist-get (car entries) :seq) (plist-get (cadr entries) :seq)))
                      (reverse entries) entries))
         (result (eas-inspect view))
         (eas-view-replaying t))
    (dolist (entry entries result)
      (setq result (eas-dispatch view (or (plist-get entry :event) entry))))))

(defun eas-push (view rows)
  "Append ROWS to VIEW's data (schema-checked) and redraw; return inspect.
With `eas-push-function' set (eas-stream), the push may be coalesced."
  (if eas-push-function (funcall eas-push-function view rows)
    (eas-dispatch view (list :type "push" :rows (vconcat rows)))))

;;; Inspect and selection

(defun eas-view--fmt (scale v)
  "Data value V formatted for SCALE (ISO for time)."
  (if (and (member (plist-get scale :type) '("time" "utc")) (numberp v)) (eas-time-iso v) v))

(defvar eas-view--summaries (make-hash-table :test 'eq :weakness 'key)
  "Mark rows vector -> (KEY . SUMMARY), so a hover does not rescan rows.")

(defun eas-view--visible-summary (scene-view)
  "Summarize the y field over SCENE-VIEW's x domain.
Return n/min/max/first/last/change.
Cached per mark rows and domain: hover keeps both (fc-qx1.9)."
  (let* ((scales (plist-get scene-view :scales))
         (xs (plist-get scales :x)) (ys (plist-get scales :y))
         (mark (seq-find (lambda (m) (> (length (plist-get m :rows)) 0)) (plist-get scene-view :marks)))
         (key (list (plist-get xs :field) (plist-get xs :type) (plist-get xs :domain) (plist-get ys :field)))
         (cached (and mark (gethash (plist-get mark :rows) eas-view--summaries))))
    (if (and cached (equal (car cached) key)) (cdr cached)
      (let ((summary (eas-view--summarize xs ys mark)))
        (when mark (puthash (plist-get mark :rows) (cons key summary) eas-view--summaries))
        summary))))

(defun eas-view--summarize (xs ys mark)
  "The visible summary of MARK's rows under x scale XS and y scale YS."
  (let* ((xf (and (plist-get xs :field) (eas-key (plist-get xs :field))))
         (yf (and (plist-get ys :field) (eas-key (plist-get ys :field))))
         (d (plist-get xs :domain))
         (rows (and mark yf
                    (seq-filter (lambda (r)
                                  (or (not (and xf (member (plist-get xs :type) '("linear" "time" "utc"))))
                                      (let ((x (eas-params--number (plist-get r xf))))
                                        (and x (<= (min (aref d 0) (aref d 1)) x (max (aref d 0) (aref d 1)))))))
                                (plist-get mark :rows))))
         (values (seq-filter #'numberp (mapcar (lambda (r) (plist-get r yf)) rows))))
    (when values
      (let ((first (car values)) (last (car (last values))))
        (list :field (plist-get ys :field) :n (length values)
              :min (apply #'min values) :max (apply #'max values) :first first :last last
              :change (- last first)
              :change-pct (if (zerop first) :null (/ (round (* 10000.0 (/ (- last first) (float first)))) 100.0)))))))

(defun eas-inspect (view)
  "Describe what VIEW draws right now, as JSON-ready data."

  (let* ((view (eas-view-get view)) (scene (eas-view-scene view)) (state (eas-view-state view))
         (hover (plist-get state :hover)))
    (list :id (eas-view-id view) :template (or (eas-view-template view) :null)
          :spec-hash (eas-view-spec-hash view)
          :interactive (if (eas-view-interactive view) t :false)
          :target (symbol-name (eas-view-target view))
          :size (or (plist-get scene :size) :null)
          :static (if (eas-view-interactive view) :null
                    (let ((fb (eas-view-fallback view)))
                      (if (plist-get fb :data) (list :source "bin/chart" :type (plist-get fb :type))
                        (list :source :null :error (plist-get fb :error)))))
          :rows (length (plist-get (eas-view-data view) :rows))
          :views (vconcat
                  (seq-map (lambda (v)
                             (let ((scales (plist-get v :scales)))
                               (append
                                (list :id (plist-get v :id)
                                      :domains (cl-loop for ch in '(:x :y)
                                                        for s = (plist-get scales ch)
                                                        when s append
                                                        (list ch (vconcat (seq-map (lambda (x) (eas-view--fmt s x))
                                                                                   (plist-get s :domain)))))
                                      :zoomed (if (plist-get (plist-get state :domains) (eas-key (plist-get v :id))) t :false))
                                (when-let* ((sum (eas-view--visible-summary v))) (list :visible sum)))))
                           (plist-get scene :views)))
          :params (vconcat (seq-map (lambda (p)
                                      (let ((store (plist-get (plist-get state :params) (eas-key (plist-get p :name)))))
                                        (list :name (plist-get p :name) :view (plist-get p :view)
                                              :type (plist-get (plist-get p :def) :type)
                                              :bind (or (plist-get p :bind) :null)
                                              :summary (eas-params-summary store)
                                              :value (or store :null))))
                                    (eas-params-of scene)))
          :hover (if hover (list :view (plist-get hover :view) :mark (plist-get hover :mark)
                                 :datum (plist-get hover :datum)
                                 :row (eas--plist-without (plist-get hover :row) eas-params-row-key)
                                 :tooltip (or (eas-tip-tooltip scene (eas-view-plan view) hover) :null))
                   :null)
          :strip (or (eas-strip scene (eas-view-plan view) state) :null)
          :click (or (plist-get state :click) :null)
          :warnings (vconcat (eas-view-warnings view))
          :last-event (let ((e (car (eas-view-log view)))) (if e (plist-get e :summary) :null)))))

(defun eas-view-log-entries (view &optional n)
  "VIEW's last N (default all) log entries, oldest first."
  (let ((log (eas-view-log (eas-view-get view))))
    (vconcat (reverse (if n (seq-take log n) log)))))

(defun eas-selection (view &optional name as)
  "Rows selected in VIEW by selection NAME (default: first non-empty).
AS is rows (default, a vector of row plists), json or org."
  (let* ((view (eas-view-get view)) (scene (eas-view-scene view))
         (state (eas-view-state view))
         (param (if name (seq-find (lambda (p) (equal (plist-get p :name) name)) (eas-params-of scene))
                  (seq-find (lambda (p) (plist-get (plist-get state :params) (eas-key (plist-get p :name))))
                            (eas-params-of scene))))
         (store (and param (plist-get (plist-get state :params) (eas-key (plist-get param :name)))))
         (scene-view (and param (seq-find (lambda (v) (equal (plist-get v :id) (plist-get param :view)))
                                          (plist-get scene :views))))
         (mark (and scene-view (seq-find (lambda (m) (> (length (plist-get m :rows)) 0)) (plist-get scene-view :marks))))
         (rows (and store mark
                    (vconcat (mapcar (lambda (r) (eas--plist-without r eas-params-row-key))
                                     (seq-filter (lambda (r) (eas-params-contains store r)) (plist-get mark :rows)))))))
    (when (and name (null param))
      (eas-signal "NOT_FOUND" (format "View %s has no param %s" (eas-view-id view) name) :param name))
    (pcase (or as 'rows)
      ((or 'rows "rows") (or rows []))
      ((or 'json "json") (eas-json-encode (or rows [])))
      ((or 'org "org") (eas-view--org-table (or rows [])))
      (_ (eas-signal "INVALID_INPUT" "selection AS is rows, json or org" :as as)))))

(defun eas-view--org-table (rows)
  "ROWS as an org table string."
  (if (zerop (length rows)) ""
    (let ((keys (eas-plist-keys (aref rows 0))))
      (concat "| " (mapconcat #'eas-key-name keys " | ") " |\n|"
              (mapconcat (lambda (_) "---") keys "+") "|\n"
              (mapconcat (lambda (r) (concat "| " (mapconcat (lambda (k) (format "%s" (plist-get r k))) keys " | ") " |"))
                         rows "\n")
              "\n"))))

(provide 'eas-view)
;;; eas-view.el ends here
