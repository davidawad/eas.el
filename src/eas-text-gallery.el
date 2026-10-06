;;; eas-text-gallery.el --- every gallery chart and template, in a terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fc-qx1.49: the gallery's verdicts (status.json) were measured on the
;; SVG backend.  This file is the text backend's gallery: every
;; official example except the topojson maps, and every template with
;; its example bindings, is opened as a live view the way a terminal
;; user opens it -- `eas-view-open' for the text target, then `eas-show'
;; in a text window -- at each of `eas-text-gallery-sizes', and judged
;; by `eas-text-check' (structural invariants computed from the scene).
;;
;;   `eas-text-gallery-entries'  what is checked: (:group :name :open FN)
;;   `eas-text-gallery-run'      one entry's problems, each "COLSxROWS: ..."
;;   `eas-text-gallery-report'   per-group counts and every problem
;;
;; test/vl-examples/text-status.json records the verdict per entry
;; ("pass", or "partial" with the problems as its reason); the :gallery
;; test holds each entry to it, so a pass cannot quietly regress and a
;; fixed partial must be promoted.

;;; Code:

(require 'eas-core)
(require 'eas-view)
(require 'eas-mode)
(require 'eas-template)
(require 'eas-vl-gallery)
(require 'eas-text-check)

(defconst eas-text-gallery-sizes
  '((:cols 60 :rows 16) (:cols 94 :rows 27) (:cols 100 :rows 30) (:cols 160 :rows 45) (:cols 189 :rows 56))
  "Text sizes every gallery chart and template is checked at.
94x27 is a quadrant and 189x56 the whole of a 189x56 terminal frame.")

(defconst eas-text-gallery-group-templates "templates"
  "The pseudo-group the templates are reported under.")

(defun eas-text-gallery-status-file ()
  "The committed text verdicts."
  (expand-file-name "text-status.json" eas-vl-gallery-directory))

(defconst eas-text-gallery--datasets "https://cdn.jsdelivr.net/npm/vega-datasets@"
  "Prefix of the vega-datasets CDN URLs some official examples load.")

(defun eas-text-gallery--local-urls (spec)
  "SPEC with vega-datasets CDN URLs pointing at the committed copies."
  (cond
   ((vectorp spec) (vconcat (mapcar #'eas-text-gallery--local-urls spec)))
   ((eas-object-p spec)
    (cl-loop for (k v) on spec by #'cddr
             append (list k (if (and (eq k :url) (stringp v) (string-prefix-p eas-text-gallery--datasets v)
                                     (string-match "/data/\\(.*\\)\\'" v))
                                (concat "../data/" (match-string 1 v))
                              (eas-text-gallery--local-urls v)))))
   (t spec)))

(defun eas-text-gallery-spec (group name)
  "Example NAME of GROUP with its data inlined (CDN datasets read locally)."
  (let ((dir (eas-vl-gallery-group-directory group)))
    (eas-vl-gallery-inline
     (eas-text-gallery--local-urls (eas-json-read-file (expand-file-name (concat name ".vl.json") dir)))
     dir)))

(defun eas-text-gallery--map-p (group name)
  "Non-nil when NAME of GROUP is a topojson map (out of scope)."
  (let ((entry (plist-get (eas-vl-gallery-status group) (eas-key name))))
    (and (equal (plist-get entry :status) "unsupported")
         (string-match-p "topojson" (or (plist-get entry :reason) "")))))

(defun eas-text-gallery-entries (&optional groups)
  "Entries to check: examples of GROUPS, then the templates.
GROUPS defaults to all, maps left out; the templates come when GROUPS
is nil or names `eas-text-gallery-group-templates'.
Each is (:group G :name N :open FN), FN taking a SIZE and returning a view."
  (append
   (cl-loop for group in (eas-vl-gallery-groups)
            when (or (null groups) (member group groups))
            append (cl-loop for name in (eas-vl-gallery-names group)
                            unless (eas-text-gallery--map-p group name)
                            collect (let ((group group) (name name) (spec nil))
                                      (list :group group :name name
                                            :open (lambda (size)
                                                    (eas-view-open (or spec (setq spec (eas-text-gallery-spec group name)))
                                                                   :id (concat "text-gallery:" name)
                                                                   :target 'text :size size))))))
   (when (or (null groups) (member eas-text-gallery-group-templates groups))
     (mapcar (lambda (name)
               (list :group eas-text-gallery-group-templates :name name
                     :open (lambda (size)
                             (eas-view-open name :bindings (eas-template-example name)
                                            :id (concat "text-gallery:" name) :target 'text :size size))))
             (eas-template-names)))))

(defun eas-text-gallery--show (view size)
  "Problems showing VIEW with `eas-show' in a text window of SIZE, in batch.
The window is faked: its size is SIZE whatever the batch frame's is."
  (let ((buffer nil))
    (unwind-protect
        (condition-case err
            (cl-letf (((symbol-function 'eas-mode--window-size) (lambda (_window _target) size)))
              (setq buffer (eas-show view 'text))
              (with-current-buffer buffer
                (cond ((not (eq (eas-view-target view) 'text)) (list "show: the view is not drawn as text"))
                      ((string-empty-p (string-trim (buffer-string))) (list "show: eas-show left an empty buffer"))
                      ;; The strip under the chart too fits the window.
                      ((seq-some (lambda (l) (> (string-width l) (plist-get size :cols))) (split-string (buffer-string) "\n"))
                       (list (format "show: a buffer line is over %d columns" (plist-get size :cols)))))))
          (error (list (format "show: eas-show fails: %s" (error-message-string err)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun eas-text-gallery-run (entry &optional sizes)
  "Problems of ENTRY (from `eas-text-gallery-entries') at SIZES.
SIZES defaults to `eas-text-gallery-sizes'.  Each problem is prefixed
with its size; nil when ENTRY passes."
  (eas-vl-gallery--native
   (let (out)
     (dolist (size (or sizes eas-text-gallery-sizes))
       (let ((tag (format "%dx%d" (plist-get size :cols) (plist-get size :rows))) (view nil))
         (unwind-protect
             (condition-case err
                 (progn
                   (setq view (funcall (plist-get entry :open) size))
                   (if (not (eas-view-interactive view))
                       (push (format "%s: static: %S" tag (eas-view-warnings view)) out)
                     (dolist (p (append (eas-text-check (eas-view-scene view) (plist-get size :cols)) (eas-text-gallery--show view size)))
                       (push (format "%s: %s" tag p) out))))
               (error (push (format "%s: open: %s" tag (error-message-string err)) out)))
           (when view (remhash (eas-view-id view) eas-views)))))
     (nreverse out))))

(defun eas-text-gallery-status ()
  "Parsed text-status.json: (GROUP-KEY (NAME-KEY (:status :reason)))."
  (let ((file (eas-text-gallery-status-file)))
    (and (file-exists-p file) (eas-json-read-file file))))

(defun eas-text-gallery-report (&optional groups)
  "Run every entry of GROUPS (default all); return (:groups [...] :problems [...]).
Each group is (:group G :pass N :partial N :total N)."
  (let ((counts nil) (problems nil))
    (dolist (entry (eas-text-gallery-entries groups))
      (let* ((g (plist-get entry :group)) (ps (eas-text-gallery-run entry))
             (c (or (assoc g counts) (car (push (list g 0 0) counts)))))
        (if ps (cl-incf (nth 2 c)) (cl-incf (nth 1 c)))
        (when ps (push (list :group g :name (plist-get entry :name) :problems (vconcat ps)) problems))))
    (list :groups (vconcat (mapcar (lambda (c) (list :group (nth 0 c) :pass (nth 1 c) :partial (nth 2 c)
                                                     :total (+ (nth 1 c) (nth 2 c))))
                                   (reverse counts)))
          :problems (vconcat (nreverse problems)))))

(defun eas-text-gallery-write-status (report)
  "Write REPORT as text-status.json and return it.
REPORT comes from `eas-text-gallery-report' over every group."
  (let ((bad (make-hash-table :test 'equal)) (status nil))
    (seq-doseq (p (plist-get report :problems))
      (puthash (cons (plist-get p :group) (plist-get p :name)) (plist-get p :problems) bad))
    (dolist (entry (eas-text-gallery-entries))
      (let* ((g (plist-get entry :group)) (n (plist-get entry :name))
             (ps (gethash (cons g n) bad))
             (cell (or (assoc g status) (car (push (list g) status)))))
        (setcdr cell (append (cdr cell)
                             (list (eas-key n)
                                   (if ps (list :status "partial" :reason (string-join (append ps nil) "; "))
                                     (list :status "pass")))))))
    (let ((value (cl-loop for (g . entries) in (reverse status) append (list (eas-key g) entries))))
      (with-temp-file (eas-text-gallery-status-file)
        (set-buffer-file-coding-system 'utf-8-unix)
        (insert (eas-json-pretty value)))
      value)))

(provide 'eas-text-gallery)
;;; eas-text-gallery.el ends here
