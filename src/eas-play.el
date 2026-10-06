;;; eas-play.el --- timer and key event streams that update params -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A template (or spec) animates, or answers keys, with
;;
;;   "x-eas": {"timer": {"interval": 1000},
;;             "on": [{"events": "timer", "param": "tick",
;;                     "update": "tick + 1"},
;;                    {"events": ["key:down", "key:j"], "param": "row",
;;                     "update": "min(row + 1, event.rows)"}]}
;;
;; the way a Vega signal's "on" handlers update it.  Each handler names
;; a top-level param of the chart and a Vega expression (eas-expr.el)
;; computing its next value.  The expression sees every param's
;; current value by name and `event': {type, key, n, time, local, rows},
;; where n counts the view's play events, time is epoch milliseconds,
;; local is time shifted so date functions (hours(), minutes()) read
;; the system's wall clock, and rows is the number of rows of the
;; view's data.  random() is deterministic per handler and play event.
;; Handlers matching an event run in order, each one seeing what the
;; ones before it set.
;;
;; A handler does not change the view itself: the values an event's
;; handlers computed are dispatched as one {"type": "param"} (or, for
;; several params, {"type": "params"}) event/v1, so the reducers stay
;; pure, the view redraws once, the log says what changed ("param tick
;; = 3") and `eas-replay' redraws the animation without a timer.  Expressions may
;; use {{SLOT}} references ({"x-eas:expr": ...}); they are resolved
;; with the view's bindings when the play attaches.
;;
;; `eas-play-tick' fires the "timer" handlers and `eas-play-key' the
;; "key:NAME" ones, so ERT and agents drive both headlessly.  Shown in
;; a buffer (`eas-show'), a view whose template declares x-eas.timer
;; ticks every interval milliseconds while the buffer is visible in some
;; window, and skips (pauses) while it is not; the keys its handlers
;; name are bound in that buffer (`eas-play-keys-mode'), ahead of the
;; view mode's own (the arrows, which otherwise pan).  Key names are
;; Emacs key descriptions (`kbd'), plus up, down, left, right, home,
;; end, pageup, pagedown and space.

;;; Code:

(require 'eas-core)
(require 'eas-expr)
(require 'eas-template)
(require 'eas-resolve)
(require 'eas-compile)
(require 'eas-view)
(require 'eas-describe)

(defvar eas-play-min-interval 50
  "Shortest timer interval accepted, in milliseconds.
Each tick may recompile and redraw the chart; see engine-spikes.md
section 8 for frame costs.")

(defvar eas-play-clock #'float-time
  "Function returning the current time in seconds.")

(defvar eas-play-visible-function #'eas-play--visible-p
  "Function of a view returning non-nil when its timer may tick.")

(cl-defstruct (eas-play (:constructor eas-play--make) (:copier nil))
  "VIEW's handlers (plists :events :param :update) and timer state."
  view object handlers interval timer (n 0) (ticks 0) (skipped 0))

(defvar eas-plays (make-hash-table :test 'equal)
  "Live plays by view id.")

;;; Configuration

(defun eas-play--invalid (path message)
  "Signal INVALID_INPUT at x-eas PATH with MESSAGE."
  (eas-signal "INVALID_INPUT" message :path (concat "/x-eas" path)))

(defun eas-play-config (source)
  "The x-eas timer and on SOURCE declares, as (:timer T :on ON), or nil.
SOURCE is a template name, a template plist or a chart/v1 spec (plist
or JSON string)."
  (let ((meta (cond ((and (stringp source) (not (string-prefix-p "{" (string-trim-left source))))
                     (plist-get (eas-template-get source) :meta))
                    ((and (eas-object-p source) (plist-get source :meta)) (plist-get source :meta))
                    (t (plist-get (if (stringp source) (eas-json-parse source) source) :x-eas)))))
    (when (or (plist-get meta :timer) (plist-get meta :on))
      (list :timer (plist-get meta :timer) :on (plist-get meta :on)))))

(defun eas-play--events (events path)
  "EVENTS of a handler (a string or array of them) as a list; check them.
PATH locates the handler."
  (let ((list (if (stringp events) (list events) (append events nil))))
    (unless (and list (seq-every-p #'stringp list))
      (eas-play--invalid path "A handler needs events: \"timer\", \"key:NAME\" or an array of them"))
    (dolist (e list list)
      (unless (or (equal e "timer") (and (string-prefix-p "key:" e) (> (length e) 4)))
        (eas-play--invalid path (format "Unknown event %S; events are \"timer\" and \"key:NAME\"" e))))))

(defun eas-play-check (config)
  "Validate CONFIG, a (:timer T :on ON) plist; return it normalized.
The result is (:interval MS-or-nil :handlers ((:events E :param P
:update U) ...))."
  (let* ((timer (plist-get config :timer))
         (interval (and timer (plist-get timer :interval)))
         (on (plist-get config :on)))
    (when timer
      (unless (eas-object-p timer)
        (eas-play--invalid "/timer" "x-eas.timer is an object: {\"interval\": MS}"))
      (dolist (key (eas-plist-keys timer))
        (unless (eq key :interval)
          (eas-play--invalid (concat "/timer/" (eas-key-name key))
                             (format "Unknown x-eas.timer key %s; keys: interval" (eas-key-name key)))))
      (unless (and (numberp interval) (>= interval eas-play-min-interval))
        (eas-play--invalid "/timer/interval"
                           (format "interval is milliseconds, at least %d" eas-play-min-interval))))
    (unless (or (null on) (vectorp on) (listp on))
      (eas-play--invalid "/on" "x-eas.on is an array of handlers"))
    (list :interval interval
          :handlers
          (cl-loop for h in (append on nil) for i from 0
                   for path = (format "/on/%d" i)
                   collect (progn
                             (unless (eas-object-p h)
                               (eas-play--invalid path "A handler is an object: {events, param, update}"))
                             (dolist (key (eas-plist-keys h))
                               (unless (memq key '(:events :param :update))
                                 (eas-play--invalid (concat path "/" (eas-key-name key))
                                                    (format "Unknown handler key %s; keys: events, param, update"
                                                            (eas-key-name key)))))
                             (unless (stringp (plist-get h :param))
                               (eas-play--invalid (concat path "/param") "A handler needs the param it updates"))
                             (unless (stringp (plist-get h :update))
                               (eas-play--invalid (concat path "/update") "A handler's update is an expression string"))
                             ;; Parse now, so a typo fails the caller and not a timer.
                             (eas-expr-parse (plist-get h :update))
                             (list :events (eas-play--events (plist-get h :events) (concat path "/events"))
                                   :param (plist-get h :param) :update (plist-get h :update)))))))

(defun eas-play--substitute (view on)
  "Handlers ON with {{SLOT}} references resolved by VIEW's bindings."
  (if-let* ((name (eas-view-template view))
            (template (eas-template-get name)))
      (eas-resolve--substitute on (eas-template-bind template (eas-view-bindings view)) "/x-eas/on")
    on))

;;; Attaching

(defun eas-play-get (view)
  "The play attached to VIEW (an id or view), or nil.
A play left by a closed view whose id was reused is dropped."
  (let* ((view (eas-view-get view)) (play (gethash (eas-view-id view) eas-plays)))
    (if (and play (not (eq (eas-play-object play) view)))
        (progn (eas-play--cancel play) (remhash (eas-view-id view) eas-plays) nil)
      play)))

(defun eas-play-attach (view &optional config)
  "Drive VIEW's params with CONFIG's handlers (default: its template's).
CONFIG is (:timer T :on ON) as `eas-play-config' returns.  Signals
INVALID_INPUT when there is none or a handler names a param VIEW does
not have.  Returns the play; attaching again replaces the handlers."
  (let* ((view (eas-view-get view))
         (config (or config (and (eas-view-template view) (eas-play-config (eas-view-template view)))
                     (eas-play--invalid "" (format "View %s declares no x-eas.timer or x-eas.on"
                                                   (eas-view-id view)))))
         (checked (eas-play-check (list :timer (plist-get config :timer)
                                        :on (eas-play--substitute view (plist-get config :on)))))
         (params (mapcar (lambda (p) (plist-get p :name))
                         (plist-get (eas-view-scene view) :params)))
         (play (or (eas-play-get view) (eas-play--make :view (eas-view-id view) :object view))))
    (when (eas-view-interactive view)
      (dolist (h (plist-get checked :handlers))
        (unless (member (plist-get h :param) params)
          (eas-play--invalid "/on" (format "Handler param %s is not a param of view %s; params: %s"
                                           (plist-get h :param) (eas-view-id view)
                                           (string-join params ", "))))))
    (setf (eas-play-handlers play) (plist-get checked :handlers)
          (eas-play-interval play) (plist-get checked :interval))
    (puthash (eas-view-id view) play eas-plays)))

(defun eas-play-detach (view)
  "Stop VIEW's timer and forget its handlers."
  (when-let* ((play (gethash (if (eas-view-p view) (eas-view-id view) view) eas-plays)))
    (eas-play--cancel play)
    (remhash (eas-play-view play) eas-plays)))

(cl-defun eas-play-open (source &rest args &key play &allow-other-keys)
  "Open a view of SOURCE (as `eas-view-open' with ARGS) and attach a play.
PLAY overrides the (:timer T :on ON) config SOURCE declares."
  (let ((view (apply #'eas-view-open source (eas--plist-without args :play))))
    (condition-case err
        (eas-play-attach view (or play (unless (eas-view-template view) (eas-play-config source))))
      (error (eas-view-close view) (signal (car err) (cdr err))))
    view))

;;; Events

(defun eas-play--env (view event)
  "Param values of VIEW now, plus EVENT as `event'."
  (eas-plist-put (eas-compile--env (eas-view-spec view) (eas-view-state view)) :event event))

(defun eas-play--fire (play name event)
  "Run PLAY's handlers listening to NAME with EVENT; return how many ran.
Each handler sees the values the ones before it computed; the changed
params reach the view as one param or params event, so one redraw."
  (let* ((view (eas-view-get (eas-play-view play)))
         (handlers (seq-filter (lambda (h) (member name (plist-get h :events))) (eas-play-handlers play)))
         (env (and handlers (eas-play--env view event)))
         (start env)
         (changed nil)
         (i 0))
    (dolist (h handlers)
      (let ((key (eas-key (plist-get h :param)))
            ;; random() hashes the row index: one row per handler and play event.
            (datum (list :_eas_row (+ (* 1024 (eas-play-n play)) i))))
        (setq env (eas-plist-put env key (eas-expr-evaluate (plist-get h :update) datum env))
              i (1+ i))
        (unless (memq key changed) (push key changed))))
    (let ((values (cl-loop for key in (nreverse changed)
                           unless (equal (plist-get env key) (plist-get start key))
                           append (list key (plist-get env key)))))
      (cond ((null values))
            ((null (cddr values))
             (eas-dispatch view (list :type "param" :param (eas-key-name (car values)) :value (cadr values))))
            (t (eas-dispatch view (list :type "params" :values values)))))
    (length handlers)))

(defun eas-play--local (seconds)
  "Epoch ms of SECONDS shifted so date functions read the local wall clock.
Date functions read `eas-time-zone' (UTC when nil); the shift is the
system zone's offset less that zone's."
  (let ((time (seconds-to-time seconds)))
    (* 1000.0 (+ seconds (car (current-time-zone time))
                 (- (if eas-time-zone (car (current-time-zone time eas-time-zone)) 0))))))

(defun eas-play--event (play type &optional key now)
  "The `event' object of a TYPE play event of PLAY with KEY at NOW."
  (let ((view (eas-view-get (eas-play-view play)))
        (now (or now (funcall eas-play-clock))))
    (append (list :type type)
            (and key (list :key key))
            (list :n (eas-play-n play)
                  :time (* 1000.0 now)
                  :local (eas-play--local now)
                  :rows (length (plist-get (eas-view-data view) :rows))))))

(defun eas-play-tick (view &optional now)
  "Fire VIEW's timer handlers once, as at time NOW (seconds); return inspect.
VIEW must have a play (`eas-play-attach'); it is attached from its
template on first use."
  (let ((play (eas-play--ensure view)))
    (cl-incf (eas-play-n play))
    (cl-incf (eas-play-ticks play))
    (eas-play--fire play "timer" (eas-play--event play "timer" nil now))
    (eas-inspect view)))

(defun eas-play-key (view key &optional now)
  "Fire VIEW's handlers for KEY (a name such as \"left\") at NOW.
Return the new inspect, or nil when no handler listens to KEY."
  (let ((play (eas-play--ensure view)))
    (when (seq-some (lambda (h) (member (concat "key:" key) (plist-get h :events)))
                    (eas-play-handlers play))
      (cl-incf (eas-play-n play))
      (eas-play--fire play (concat "key:" key) (eas-play--event play "key" key now))
      (eas-inspect view))))

(defun eas-play--ensure (view)
  "VIEW's play, attached from its template when it has none."
  (or (eas-play-get view) (eas-play-attach view)))

;;; Timer

(defun eas-play--visible-p (view)
  "Non-nil when a window of a visible frame displays VIEW's buffer."
  (let ((buffer (eas-view-buffer (eas-view-get view))))
    (and (buffer-live-p buffer) (get-buffer-window buffer 'visible) t)))

(defun eas-play--cancel (play)
  "Cancel PLAY's timer."
  (when (eas-play-timer play)
    (cancel-timer (eas-play-timer play))
    (setf (eas-play-timer play) nil)))

(defun eas-play--timer-fire (id)
  "The timer callback of view ID: tick when visible, else skip."
  (let ((play (gethash id eas-plays)))
    (cond ((null play))
          ((not (gethash id eas-views)) (eas-play-detach id))
          ((not (funcall eas-play-visible-function id)) (cl-incf (eas-play-skipped play)))
          (t (condition-case err (eas-play-tick id)
               (eas-error (eas-play--cancel play)
                          (message "eas: %s stopped: %s" id (plist-get (eas-error-plist err) :message))))))))

(defun eas-play-start (view)
  "Run VIEW's timer handlers every interval while it is visible.
The first tick comes at once.  Returns the play, or nil when VIEW
declares no timer."
  (let ((play (eas-play--ensure view)))
    (when (eas-play-interval play)
      (eas-play--cancel play)
      (setf (eas-play-timer play)
            (run-at-time 0 (/ (eas-play-interval play) 1000.0) #'eas-play--timer-fire (eas-play-view play)))
      play)))

(defun eas-play-stop (view)
  "Stop VIEW's timer; its handlers still answer keys."
  (when-let* ((play (eas-play-get view))) (eas-play--cancel play)))

(defun eas-play-inspect (view)
  "VIEW's play as JSON-ready data, or :null when it has none."
  (if-let* ((play (eas-play-get view)))
      (list :view (eas-play-view play)
            :interval (or (eas-play-interval play) :null)
            :running (if (eas-play-timer play) t :false)
            :visible (if (funcall eas-play-visible-function (eas-play-view play)) t :false)
            :events (eas-play-n play) :ticks (eas-play-ticks play) :skipped (eas-play-skipped play)
            :keys (vconcat (eas-play-keys play))
            :params (vconcat (delete-dups (mapcar (lambda (h) (plist-get h :param)) (eas-play-handlers play)))))
    :null))

;;; Keys in eas-view-mode buffers

(defun eas-play-keys (play)
  "The key names PLAY's handlers listen to."
  (delete-dups (cl-loop for h in (eas-play-handlers play)
                        append (cl-loop for e in (plist-get h :events)
                                        when (string-prefix-p "key:" e) collect (substring e 4)))))

(defconst eas-play--key-names
  '(("up" . [up]) ("down" . [down]) ("left" . [left]) ("right" . [right])
    ("home" . [home]) ("end" . [end]) ("pageup" . [prior]) ("pagedown" . [next])
    ("space" . " ") ("escape" . [escape]) ("return" . [return]) ("tab" . [tab]))
  "Key names with their Emacs keys; any other name is read by `kbd'.")

(defun eas-play--emacs-key (name)
  "The Emacs key sequence of key NAME, as a vector."
  (vconcat (or (cdr (assoc name eas-play--key-names)) (kbd name))))

(defvar-local eas-play--view nil "The view whose keys this buffer sends.")

(defvar-local eas-play--key-map nil "This buffer's play key map.")

(define-minor-mode eas-play-keys-mode
  "Send the keys of this buffer's view's x-eas.on handlers to them.
The keys take precedence over `eas-view-mode-map' (the arrows pan
there)."
  :lighter " Play"
  (if eas-play-keys-mode
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'eas-play-keys-mode eas-play--key-map)
                        (assq-delete-all 'eas-play-keys-mode
                                         (copy-sequence minor-mode-overriding-map-alist))))
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'eas-play-keys-mode (copy-sequence minor-mode-overriding-map-alist)))))

(defun eas-play-key-command ()
  "Send the key that invoked this command to the buffer's view handlers."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (name (car (seq-find (lambda (entry) (equal (eas-play--emacs-key (car entry)) keys))
                              (mapcar #'list (eas-play-keys (eas-play-get eas-play--view)))))))
    (when name
      (condition-case err (eas-play-key eas-play--view name)
        (eas-error (message "eas: %s" (plist-get (eas-error-plist err) :message)))))))

(defun eas-play--bind-keys (view buffer)
  "Bind VIEW's handler keys in BUFFER and turn `eas-play-keys-mode' on."
  (let ((map (make-sparse-keymap)) (play (eas-play-get view)))
    (dolist (name (eas-play-keys play))
      (define-key map (eas-play--emacs-key name) #'eas-play-key-command))
    (with-current-buffer buffer
      (setq eas-play--view (eas-view-id view) eas-play--key-map map)
      (eas-play-keys-mode (if (eas-play-keys play) 1 -1)))))

(defun eas-play--on-show (view buffer)
  "Attach and start VIEW's play when its template declares one in BUFFER."
  (when (and (eas-view-interactive view)
             (or (eas-play-get view)
                 (and (eas-view-template view) (eas-play-config (eas-view-template view)))))
    (eas-play--ensure view)
    (eas-play--bind-keys view buffer)
    (eas-play-start view)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook (let ((id (eas-view-id view))) (lambda () (eas-play-stop id))) nil t))))

(defvar eas-show-functions)
(add-hook 'eas-show-functions #'eas-play--on-show)

;;; Describe

(defun eas-play--describe ()
  "The describe section for timer and key handlers."
  (list :play (list :keys (list :timer "x-eas.timer: {\"interval\": MS}, ticking while the view's buffer is visible"
                                :on "x-eas.on: [{\"events\": \"timer\"|\"key:NAME\"|[...], \"param\": P, \"update\": EXPR}]")
                    :expression "params by name; event: {type, key, n, time, local, rows}"
                    :min-interval eas-play-min-interval
                    :key-names (vconcat (mapcar #'car eas-play--key-names))
                    :verbs ["eas-play-tick" "eas-play-key" "eas-play-attach" "eas-play-start" "eas-play-stop"
                            "eas-play-inspect" "eas-play-open"])))

(add-hook 'eas-describe-functions #'eas-play--describe)

(provide 'eas-play)
;;; eas-play.el ends here
