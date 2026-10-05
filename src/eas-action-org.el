;;; eas-action-org.el --- actions that jump into org: source rows, notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.5).  Two built-in actions for charts whose data
;; lives in org:
;;
;;   goto-source  jump to where the clicked datum came from:
;;                1. the row's link column (args "field", default
;;                   "source"): an org link or URL, opened as open-href
;;                2. the view's source (`eas-action-set-source', or
;;                   args "file"/"buffer" with "table"/"heading"): the
;;                   datum's line in the named org table, or the
;;                   heading whose title holds the row's HEADING field
;;   open-notes   open the notes for the datum's date: DIRECTORY/DATE.org
;;                (`eas-action-notes-directory', args "directory"),
;;                or the heading naming the date in one file
;;                (`eas-action-notes-file', args "file").  The date
;;                comes from args "field", default the view's x field.
;;
;; Both show the buffer with `eas-action-display-function' and
;; answer "FILE-OR-BUFFER:LINE"; what they cannot find is NOT_FOUND,
;; recorded on the click like any action failure.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-params)
(require 'eas-view)
(require 'eas-action)

(declare-function org-at-table-p "org-table" (&optional table-type))

(defvar eas-action-display-function #'pop-to-buffer
  "Function the org actions call with the buffer they jumped in.")

(defvar eas-action-notes-directory nil
  "Directory of daily notes files, named by `eas-action-notes-file-format'.")

(defvar eas-action-notes-file nil
  "One org file whose headings name the dates they hold notes for.")

(defvar eas-action-notes-file-format "%Y-%m-%d.org"
  "`format-time-string' format (UTC) of a day's notes file name.")

(defvar eas-action-notes-date-format "%Y-%m-%d"
  "`format-time-string' format (UTC) of the date a notes heading contains.")

(defvar eas-action--sources (make-hash-table :test 'eq :weakness 'key)
  "Per-view source of the rows: view -> (:file F | :buffer B ...).")

(defun eas-action-set-source (view source)
  "Record where VIEW's rows came from, for goto-source; return SOURCE.
SOURCE is a plist: :file F or :buffer B, then :table NAME (the
#+name of the org table the rows were read from, row N being its Nth
data line; nil means the first table) or :heading FIELD (each row
names its heading in FIELD).  nil forgets it."
  (let ((view (eas-view-get view)))
    (if source (puthash view source eas-action--sources) (remhash view eas-action--sources))
    source))

(defun eas-action--arg (target key)
  "TARGET's binding argument KEY (a keyword), or nil."
  (plist-get (plist-get target :args) key))

(defun eas-action--show (buffer pos)
  "Show BUFFER with point at POS; return \"FILE-OR-BUFFER:LINE\"."
  (with-current-buffer buffer
    (goto-char pos)
    (funcall eas-action-display-function buffer)
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-point window pos))
    (format "%s:%d" (or (buffer-file-name buffer) (buffer-name buffer))
            (with-current-buffer buffer (line-number-at-pos pos)))))

(defun eas-action--source-buffer (source)
  "The buffer SOURCE's :file or :buffer names."
  (let ((file (plist-get source :file)) (buffer (plist-get source :buffer)))
    (cond (file (find-file-noselect (expand-file-name file)))
          ((and buffer (buffer-live-p (get-buffer buffer))) (get-buffer buffer))
          (t (eas-signal "NOT_FOUND" (format "Source %s is neither a file nor a live buffer; pass :file or :buffer"
                                               (or file buffer))
                           :action "goto-source")))))

(defun eas-action--table-row (name n)
  "Position of data row N of the org table named NAME (nil: the first table).
The first non-hline line is the header, as the org-table adapter reads it."
  (require 'org-table)
  (save-excursion
    (goto-char (point-min))
    (unless (if name
                (re-search-forward (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
              (re-search-forward "^[ \t]*|" nil t))
      (eas-signal "NOT_FOUND" (format "No org table %s in %s" (or name "at all") (buffer-name))
                    :action "goto-source" :table name))
    (forward-line (if name 1 0))
    (let ((k -2))                       ; the header line is row -1
      (while (and (< k n) (org-at-table-p))
        (unless (looking-at-p "^[ \t]*|-") (setq k (1+ k)))
        (when (< k n) (forward-line 1)))
      (unless (and (= k n) (org-at-table-p))
        (eas-signal "NOT_FOUND" (format "Table %s has no data row %d; was it edited since the chart opened?"
                                          (or name "") n)
                      :action "goto-source" :index n))
      (skip-chars-forward " \t|")
      (point))))

(defun eas-action--heading (title action)
  "Position of the first heading whose title contains TITLE, for ACTION."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*+[ \t]+.*%s" (regexp-quote title)) nil t)
        (line-beginning-position)
      (eas-signal "NOT_FOUND" (format "No heading mentions %s in %s; add one" title (buffer-name))
                    :action action :heading title))))

(defun eas-action-goto-source (target view)
  "Jump to the org row, heading or link TARGET's datum in VIEW came from."
  (let* ((row (plist-get target :row))
         (link (plist-get row (eas-key (or (eas-action--arg target :field) "source"))))
         (args (plist-get target :args))
         (source (if (or (plist-get args :file) (plist-get args :buffer)) args
                   (gethash view eas-action--sources))))
    (cond
     ((and (stringp link) (not (string-empty-p link))) (eas-action-open-link link))
     (source
      (let ((buffer (eas-action--source-buffer source)))
        (eas-action--show
         buffer
         (with-current-buffer buffer
           (if-let* ((field (plist-get source :heading)))
               (eas-action--heading (format "%s" (plist-get row (eas-key field))) "goto-source")
             (let ((n (plist-get target :source-row)))
               (unless (integerp n)
                 (eas-signal "NOT_FOUND" "This datum has no source row (an aggregate?); use :heading or a link column"
                               :action "goto-source"))
               (eas-action--table-row (plist-get source :table) n)))))))
     (t (eas-signal "NOT_FOUND"
                      "This datum has no source: give rows a source column (an org link) or call `eas-action-set-source'"
                      :action "goto-source")))))

(defun eas-action--date (target view)
  "TARGET's date in VIEW as epoch ms, from args field or the x field."
  (let* ((given (eas-action--arg target :field))
         (scene (eas-view-scene view))
         (field (or given (eas-params-channel-field scene (plist-get target :view) "x")))
         (value (and field (plist-get (plist-get target :row) (eas-key field))))
         (x (plist-get (plist-get (seq-find (lambda (v) (equal (plist-get v :id) (plist-get target :view)))
                                            (plist-get scene :views))
                                  :scales)
                       :x)))
    (or (and (stringp value) (eas-time-parse value))
        (and (numberp value) (or given (member (plist-get x :type) '("time" "utc"))) value)
        (eas-signal "NOT_FOUND" (format "Datum has no date in field %s; bind open-notes with a \"field\"" field)
                      :action "open-notes" :field field))))

(defun eas-action-open-notes (target view)
  "Open the notes for TARGET's date in VIEW."
  (let* ((time (/ (eas-action--date target view) 1000.0))
         (directory (or (eas-action--arg target :directory) eas-action-notes-directory))
         (file (or (eas-action--arg target :file) eas-action-notes-file)))
    (cond
     (directory
      (let ((buffer (find-file-noselect
                     (expand-file-name (format-time-string (or (eas-action--arg target :format)
                                                               eas-action-notes-file-format)
                                                           time t)
                                       directory))))
        (eas-action--show buffer (with-current-buffer buffer (point-min)))))
     (file
      (let ((buffer (find-file-noselect (expand-file-name file))))
        (eas-action--show buffer (with-current-buffer buffer
                                     (eas-action--heading (format-time-string eas-action-notes-date-format time t)
                                                            "open-notes")))))
     (t (eas-signal "NOT_FOUND"
                      "No notes location: set `eas-action-notes-directory' or `eas-action-notes-file', or bind args"
                      :action "open-notes")))))

(eas-register-action
 "goto-source" :fn #'eas-action-goto-source
 :doc "Jump to the datum's source: its link column, or its org table row or heading.")

(eas-register-action
 "open-notes" :fn #'eas-action-open-notes
 :doc "Open the notes for the datum's date (a file per day, or a heading in one file).")

(provide 'eas-action-org)
;;; eas-action-org.el ends here
