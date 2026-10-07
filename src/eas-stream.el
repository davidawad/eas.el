;;; eas-stream.el --- live data: frame-capped, interaction-aware pushes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A template (or spec) opts into live data with
;;
;;   "x-eas": {"stream": {"max-fps": 5, "window": 500}}
;;
;; Every `eas-push' to such a view is schema-checked at once (so a
;; bad row fails the caller, not a timer) and queued.  Queued rows reach
;; the view as one windowed push event, at most max-fps times a second,
;; and never while the user is interacting: a drag (brush or pan) is in
;; progress, or a pointer event arrived within `eas-stream-hover-hold'
;; seconds and no pointerleave followed.  When that ends the queue
;; catches up in a single frame.  The frame is an ordinary push event,
;; so the view log replays exactly what was drawn.
;;
;; A keyed push (`eas-push' with :key, eas-keyed.el) queues into a
;; batch that keeps only the latest row per key, so a frame carries one
;; row per changed key however many deltas arrived, and a long pause
;; holds a bounded queue.  Consecutive pushes with the same key (or
;; none) share a batch; a frame dispatches one push event per batch.
;;
;; The redraw itself stays idle-coalesced (eas-mode); the cap bounds
;; how often the scene is recompiled.  The default cap comes from the
;; measured cost of a push frame (docs/design/engine-spikes.md, section
;; 8).  Time is `eas-stream-clock' and timers can be turned off, so
;; ERT drives every case headlessly with `eas-stream-tick'.

;;; Code:

(require 'eas-core)
(require 'eas-template)
(require 'eas-adapters)
(require 'eas-view)
(require 'eas-keyed)
(require 'eas-describe)
(require 'eas-live)

(defvar eas-stream-default-max-fps 5
  "Frame cap for streams that do not set max-fps.
See engine-spikes.md section 8 for the measured frame costs behind it.")

(defconst eas-stream-max-fps-limit 30
  "Highest accepted max-fps.")

(defvar eas-stream-hover-hold 2.0
  "Seconds after a pointer event that a stream stays paused.
Neither GUI nor terminal glue reliably reports the pointer leaving (in
a terminal, point always hovers), so the pause lapses.  nil holds until
pointerleave.")

(defvar eas-stream-clock #'float-time
  "Function returning the current time in seconds.")

(defvar eas-stream-use-timers t
  "Non-nil schedules deferred frames with timers.
Tests bind it to nil and call `eas-stream-tick' themselves.")

(defconst eas-stream--pointer-events
  '("pointermove" "pointerdown" "pointerup" "click" "dblclick" "wheel")
  "Event types that mean the pointer is over the chart.")

(cl-defstruct (eas-stream--batch (:constructor eas-stream--batch-make) (:copier nil))
  "Queued rows of one kind: KEY nil appends PARTS (row vectors, newest
first); a KEY column merges rows into TABLE, keys in ORDER newest first."
  key parts table order)

(defun eas-stream--batch-add (batch rows)
  "Queue ROWS into BATCH."
  (if-let* ((key (eas-stream--batch-key batch)))
      (setf (eas-stream--batch-order batch)
            (eas-keyed-merge (eas-stream--batch-table batch) (eas-stream--batch-order batch) rows key))
    (push rows (eas-stream--batch-parts batch))))

(defun eas-stream--compact (batch window)
  "Trim an append BATCH to its last WINDOW rows.
A frame draws no more, so a long pause (a hidden view) keeps a bounded
queue."
  (when (and window (not (eas-stream--batch-key batch))
             (cdr (eas-stream--batch-parts batch))
             (> (eas-stream--batch-count batch) window))
    (let ((rows (eas-stream--batch-rows batch)))
      (setf (eas-stream--batch-parts batch)
            (list (seq-subseq rows (max 0 (- (length rows) window))))))))

(defun eas-stream--batch-rows (batch)
  "BATCH's queued rows as a vector, oldest first."
  (if (eas-stream--batch-key batch)
      (let ((table (eas-stream--batch-table batch)))
        (vconcat (mapcar (lambda (k) (gethash k table)) (reverse (eas-stream--batch-order batch)))))
    (apply #'vconcat (reverse (eas-stream--batch-parts batch)))))

(defun eas-stream--batch-count (batch)
  "How many rows BATCH would draw."
  (if (eas-stream--batch-key batch) (hash-table-count (eas-stream--batch-table batch))
    (apply #'+ (mapcar #'length (eas-stream--batch-parts batch)))))

(cl-defstruct (eas-stream (:constructor eas-stream--make) (:copier nil))
  view object max-fps window pending (queued 0) last-frame pointer-at timer
  (frames 0) (pushes 0))

(defvar eas-streams (make-hash-table :test 'equal)
  "Live streams by view id.")

;;; Configuration

(defun eas-stream--invalid (key message)
  "Signal INVALID_INPUT for x-eas.stream KEY with MESSAGE."
  (eas-signal "INVALID_INPUT" message :path (concat "/x-eas/stream" (and key (concat "/" key)))))

(defun eas-stream-check (config)
  "Validate CONFIG (an x-eas.stream object); return it with defaults.
The result is (:max-fps N :window N-or-nil)."
  (unless (and config (eas-object-p config))
    (eas-stream--invalid nil "x-eas.stream is an object: {\"max-fps\": N, \"window\": N}"))
  (dolist (key (eas-plist-keys config))
    (unless (memq key '(:max-fps :window))
      (eas-stream--invalid (eas-key-name key)
                             (format "Unknown x-eas.stream key %s; keys: max-fps, window"
                                     (eas-key-name key)))))
  (let ((fps (or (plist-get config :max-fps) eas-stream-default-max-fps))
        (window (plist-get config :window)))
    (unless (and (numberp fps) (> fps 0) (<= fps eas-stream-max-fps-limit))
      (eas-stream--invalid "max-fps" (format "max-fps is a number in (0, %d]" eas-stream-max-fps-limit)))
    (unless (or (null window) (and (natnump window) (> window 0)))
      (eas-stream--invalid "window" "window is a positive integer: the rows to keep, oldest dropped"))
    (list :max-fps fps :window window)))

(defun eas-stream-config (source)
  "The x-eas.stream object SOURCE declares, or nil.
SOURCE is a template name, a template plist or a chart/v1 spec (plist
or JSON string).  The object is returned as written; see
`eas-stream-check' for defaults."
  (cond ((and (stringp source) (not (string-prefix-p "{" (string-trim-left source))))
         (eas-stream-config (eas-template-get source)))
        ((and (eas-object-p source) (plist-get source :meta))
         (plist-get (plist-get source :meta) :stream))
        (t (plist-get (plist-get (if (stringp source) (eas-json-parse source) source) :x-eas) :stream))))

;;; Attaching

(defun eas-stream-get (view)
  "The stream attached to VIEW (an id or view), or nil.
A stream left by a closed view whose id was reused is dropped."
  (let* ((view (eas-view-get view)) (stream (gethash (eas-view-id view) eas-streams)))
    (if (and stream (not (eq (eas-stream-object stream) view)))
        (progn (eas-stream--cancel stream) (remhash (eas-view-id view) eas-streams) nil)
      stream)))

(defun eas-stream-attach (view &optional config)
  "Stream live data into VIEW with CONFIG (default: its template's).
Signals INVALID_INPUT when neither declares x-eas.stream.  Returns
the stream; attaching again replaces the configuration."
  (let* ((view (eas-view-get view))
         (config (or config (and (eas-view-template view) (eas-stream-config (eas-view-template view)))))
         (checked (if config (eas-stream-check config)
                    (eas-stream--invalid nil (format "View %s declares no x-eas.stream; pass a config"
                                                       (eas-view-id view)))))
         (stream (or (eas-stream-get view)
                     (eas-stream--make :view (eas-view-id view) :object view))))
    (setf (eas-stream-max-fps stream) (plist-get checked :max-fps)
          (eas-stream-window stream) (plist-get checked :window))
    (puthash (eas-view-id view) stream eas-streams)))

(defun eas-stream-detach (view)
  "Stop streaming into VIEW (an id or view), drawing queued rows first."
  (when-let* ((stream (gethash (if (eas-view-p view) (eas-view-id view) view) eas-streams)))
    (when (and (eas-stream-pending stream) (gethash (eas-stream-view stream) eas-views))
      (eas-stream--flush stream (funcall eas-stream-clock)))
    (eas-stream--cancel stream)
    (remhash (eas-stream-view stream) eas-streams)))

(cl-defun eas-stream-open (source &rest args &key stream &allow-other-keys)
  "Open a view of SOURCE (as `eas-view-open' with ARGS) and attach a stream.
STREAM overrides the config SOURCE declares; one of them must exist."
  (let ((view (apply #'eas-view-open source (eas--plist-without args :stream))))
    (condition-case err
        (eas-stream-attach view (or stream (unless (eas-view-template view)
                                               (eas-stream-config source))))
      (error (eas-view-close view) (signal (car err) (cdr err))))
    view))

;;; Frames

(defun eas-stream--held (stream view now)
  "Why STREAM's VIEW must not take a frame at NOW (drag or pointer), or nil."
  (cond ((plist-get (eas-view-state view) :drag) "drag")
        ((and (eas-stream-pointer-at stream)
              (or (null eas-stream-hover-hold)
                  (< (- now (eas-stream-pointer-at stream)) eas-stream-hover-hold)))
         "pointer")))

(defun eas-stream--next-frame (stream)
  "Earliest time STREAM may take its next frame.
A frame slower than the cap delays the next (`eas-live-interval')."
  (if (eas-stream-last-frame stream)
      (+ (eas-stream-last-frame stream)
         (eas-live-interval (eas-stream-view stream) (/ 1.0 (eas-stream-max-fps stream))))
    0))

(defun eas-stream--flush (stream now)
  "Dispatch STREAM's queued rows as one windowed push per batch at NOW."
  (let ((batches (reverse (eas-stream-pending stream)))
        (window (eas-stream-window stream)))
    (setf (eas-stream-pending stream) nil (eas-stream-queued stream) 0
          (eas-stream-last-frame stream) now)
    (cl-incf (eas-stream-frames stream))
    (dolist (batch batches)
      (let* ((key (eas-stream--batch-key batch))
             (rows (eas-stream--batch-rows batch))
             (rows (if (and window (not key) (> (length rows) window))
                       (seq-subseq rows (- (length rows) window))
                     rows)))
        (eas-live-timed (eas-stream-view stream)
          (eas-dispatch (eas-stream-view stream)
                        (append (list :type "push" :rows rows) (and key (list :key key))
                                (and window (list :window window)))))))))

(defun eas-stream--cancel (stream)
  "Cancel STREAM's pending timer."
  (when (eas-stream-timer stream)
    (cancel-timer (eas-stream-timer stream))
    (setf (eas-stream-timer stream) nil)))

(defun eas-stream--schedule (stream at now)
  "Arrange for STREAM to be ticked at time AT (seen from NOW)."
  (when (and eas-stream-use-timers (not (eas-stream-timer stream)))
    (setf (eas-stream-timer stream)
          (run-at-time (max 0 (- at now)) nil
                       (lambda (id)
                         (when-let* ((s (gethash id eas-streams)))
                           (setf (eas-stream-timer s) nil)
                           (if (gethash id eas-views) (eas-stream-tick id)
                             (remhash id eas-streams))))
                       (eas-stream-view stream)))))

(defun eas-stream-tick (view &optional now)
  "Draw VIEW's queued rows at NOW (default: the clock) if allowed.
A frame is taken when rows are queued, 1/max-fps seconds have passed
since the last one and no interaction holds it; otherwise the next try
is scheduled.  Returns non-nil when a frame was taken."
  (let* ((stream (eas-stream-get view)) (now (or now (funcall eas-stream-clock))))
    (when (and stream (eas-stream-pending stream)
               ;; Nobody sees the view: keep the queue for when someone does.
               (or (eas-live-visible-p view)
                   (progn (eas-stream--cancel stream) (eas-live-defer view) nil)))
      (let ((held (eas-stream--held stream (eas-view-get view) now))
            (next (eas-stream--next-frame stream)))
        (cond ((and (equal held "pointer") eas-stream-hover-hold)
               (eas-stream--schedule stream (max next (+ (eas-stream-pointer-at stream) eas-stream-hover-hold)) now)
               nil)
              (held (eas-stream--schedule stream (max next (+ now (/ 1.0 (eas-stream-max-fps stream)))) now)
                    nil)
              ((< now next) (eas-stream--schedule stream next now) nil)
              (t (eas-stream--cancel stream) (eas-stream--flush stream now) t))))))

(defun eas-stream-push (view rows &optional key)
  "Queue ROWS for VIEW's stream and draw when the cap and interaction allow.
ROWS are schema-checked now (SHAPE_INVALID, :index within ROWS).  KEY
makes the push keyed (`eas-push'); queued keyed rows keep the latest
row per key.  A view whose template declares x-eas.stream is attached
on first push; any other view gets the rows at once.  Returns
`eas-inspect'."
  (let* ((view (eas-view-get view))
         (rows (vconcat rows))
         (stream (or (eas-stream-get view)
                     (and (eas-view-interactive view) (eas-view-template view)
                          (eas-stream-config (eas-view-template view))
                          (eas-stream-attach view)))))
    (if (not stream)
        (eas-dispatch view (append (list :type "push" :rows rows) (and key (list :key key))))
      (let ((rows (eas-keyed-check (eas-view-data view) rows key)))
        (when (> (length rows) 0)
          (let ((batch (car (eas-stream-pending stream))))
            (unless (and batch (equal (eas-stream--batch-key batch) key))
              (setq batch (eas-stream--batch-make
                           :key key :table (and key (make-hash-table :test 'equal))))
              (push batch (eas-stream-pending stream)))
            (eas-stream--batch-add batch rows)
            (eas-stream--compact batch (eas-stream-window stream)))
          (setf (eas-stream-queued stream)
                (apply #'+ (mapcar #'eas-stream--batch-count (eas-stream-pending stream))))
          (cl-incf (eas-stream-pushes stream))
          (eas-stream-tick view)))
      (eas-inspect view))))

(defun eas-stream-flush (view)
  "Draw VIEW's queued rows now, ignoring the cap and interaction; return inspect."
  (let ((stream (eas-stream-get view)))
    (when (and stream (eas-stream-pending stream))
      (eas-stream--cancel stream)
      (eas-stream--flush stream (funcall eas-stream-clock)))
    (eas-inspect view)))

(defun eas-stream--observe (view event &rest _)
  "Track pointer presence for VIEW's stream from dispatched EVENT.
The hook's OLD-STATE and OLD-SCENE arguments are ignored."
  (when-let* ((stream (eas-stream-get view)))
    (let ((type (plist-get event :type)))
      (cond ((member type eas-stream--pointer-events)
             (setf (eas-stream-pointer-at stream) (funcall eas-stream-clock)))
            ((equal type "pointerleave")
             (setf (eas-stream-pointer-at stream) nil)))
      ;; Leaving the chart or ending a drag may end a pause: catch up.
      (when (and (member type '("pointerleave" "pointerup")) (eas-stream-pending stream)
                 (not (eas-stream--held stream view (funcall eas-stream-clock))))
        (eas-stream--cancel stream)
        (eas-stream-tick view)))))

(add-hook 'eas-view-dispatch-functions #'eas-stream--observe)

(defun eas-stream--wake (view)
  "Draw the queue VIEW's stream kept while VIEW was hidden."
  (when (eas-stream-get view) (eas-stream-tick view)))

(add-hook 'eas-live-wake-functions #'eas-stream--wake)
(setq eas-push-function #'eas-stream-push)

;;; Inspect

(defun eas-stream-inspect (view)
  "VIEW's stream as JSON-ready data, or :null when it has none."
  (if-let* ((stream (eas-stream-get view)))
      (list :view (eas-stream-view stream)
            :max-fps (eas-stream-max-fps stream)
            :window (or (eas-stream-window stream) :null)
            :queued (eas-stream-queued stream)
            :held (or (eas-stream--held stream (eas-view-get view) (funcall eas-stream-clock)) :null)
            :frames (eas-stream-frames stream)
            :pushes (eas-stream-pushes stream))
    :null))

(defun eas-stream--describe ()
  "The describe section for live data."
  (list :stream (list :key "x-eas.stream"
                      :keys (list :max-fps (format "frames per second; default %s, at most %d"
                                                   eas-stream-default-max-fps eas-stream-max-fps-limit)
                                  :window "rows kept, oldest dropped; default all")
                      :keyed "eas-push :key FIELD replaces rows by FIELD; _eas_delete: true removes"
                      :hover-hold (or eas-stream-hover-hold :null)
                      :verbs ["eas-push" "eas-push-delete" "eas-stream-open" "eas-stream-attach"
                              "eas-stream-flush" "eas-stream-inspect" "eas-stream-detach"])))

(add-hook 'eas-describe-functions #'eas-stream--describe)

(provide 'eas-stream)
;;; eas-stream.el ends here
