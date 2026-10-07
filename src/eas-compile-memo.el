;;; eas-compile-memo.el --- transforms shared by layers and frames run once -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (fc-qx1.43, eas-b2s.3).  Each unit of a layered chart runs
;; its ancestors' transforms on their data before its own, so a parent's
;; fold, window or calculate ran once per layer: seven times for the
;; gallery's parallel coordinates.  Vega-Lite runs a shared dataflow
;; once.  While `eas-compile-memo' is in effect, transforms run one
;; step at a time and every prefix of a transform array run on the same
;; rows vector (by identity) with the same params is remembered: a
;; layer whose transforms extend a sibling's starts from the longest
;; prefix already run.  Each row handed out is a fresh copy, so the
;; unit that gets them may change them freely.
;;
;; Prefixes also outlive a compile.  Rows tagged from a source vector
;; (`eas-compile-memo-source': the view's data, a spec's values) are
;; remembered against that source, keyed by the prefix, the values of
;; the params its text names and `eas-compile-memo-globals'.  A slider
;; that filters 5000 random points then reruns the filter, not the
;; random calculates before it (the table drops the least recently used
;; run first).  A prefix with a registered transform,
;; a lookup into a selection or a clock read (now) is never kept across
;; compiles.  A push makes a new source vector, so its entries go with
;; the old one (the table is weak).

;;; Code:

(require 'eas-core)
(require 'eas-transform)

(defvar eas-compile-memo--runs nil
  "Hash table of rows vector -> ((PREFIX . ENV) . ROWS) runs.
Set while a compile is in progress; nil otherwise.")

(defvar eas-compile-memo--sources (make-hash-table :test 'eq :weakness 'key)
  "Tagged rows vector -> the source vector it was tagged from.")

(defvar eas-compile-memo--frames (make-hash-table :test 'eq :weakness 'key)
  "Source vector -> ((PREFIX PARAMS GLOBALS) . ROWS) runs kept across compiles.")

(defvar eas-compile-memo-frame-limit 24
  "Prefix runs kept per source vector across compiles.")

(defvar eas-compile-memo-globals '(eas-time-zone)
  "Variables whose values transforms may read; part of every kept run's key.")

(defvar eas-compile-memo-stats (list :hits 0 :frame-hits 0 :steps 0)
  "Counters of `eas-compile-memo-transform-run'.
Prefixes found, those found from an earlier compile, and transform
steps run.")

(defmacro eas-compile-memo (&rest body)
  "Evaluate BODY with each transform run memoized."
  `(let ((eas-compile-memo--runs (make-hash-table :test 'eq))) ,@body))

(defun eas-compile-memo-source (tagged source)
  "Record that TAGGED rows were made from SOURCE (a vector); return TAGGED."
  (when (vectorp source) (puthash tagged source eas-compile-memo--sources))
  tagged)

(defun eas-compile-memo--count (key)
  "Increment `eas-compile-memo-stats' KEY."
  (plist-put eas-compile-memo-stats key (1+ (or (plist-get eas-compile-memo-stats key) 0))))

(defun eas-compile-memo--copy (rows)
  "A fresh vector of fresh copies of ROWS."
  (let* ((n (length rows)) (out (make-vector n nil)))
    (dotimes (i n) (aset out i (copy-sequence (aref rows i))))
    out))

(defvar eas-compile-memo--texts (make-hash-table :test 'eq :weakness 'key)
  "Transform object -> its printed text, for the checks below.")

(defvar eas-compile-memo--names (make-hash-table :test 'eq :weakness 'key)
  "Transform object -> ((PARAM-KEYS . MENTIONED) ...): the keys it names.")

(defun eas-compile-memo--text (transform)
  "TRANSFORM printed, cached by identity."
  (or (gethash transform eas-compile-memo--texts)
      (puthash transform (format "%S" transform) eas-compile-memo--texts)))

(defun eas-compile-memo--keepable-p (transform)
  "Non-nil when TRANSFORM's output depends only on rows, params and globals."
  (not (or (plist-get transform :x-eas:transform)
           (plist-get (plist-get transform :from) :param)
           (string-match-p "\\_<now\\_>" (eas-compile-memo--text transform)))))

(defun eas-compile-memo--mentions (transform keys)
  "Members of param KEYS whose names occur as words in TRANSFORM."
  (let ((seen (gethash transform eas-compile-memo--names)))
    (if-let* ((hit (assoc keys seen))) (cdr hit)
      (let ((m (seq-filter (lambda (k) (string-match-p (concat "\\_<" (regexp-quote (eas-key-name k)) "\\_>")
                                                       (eas-compile-memo--text transform)))
                           keys)))
        (puthash transform (cons (cons keys m) (seq-take seen 7)) eas-compile-memo--names)
        m))))

(defun eas-compile-memo--run (cells prefix env)
  "Find the rows of PREFIX under ENV in CELLS, ((PREFIX . ENV) . ROWS) pairs."
  (cl-loop for cell in cells
           when (and (equal (caar cell) prefix) (equal (cdar cell) env)) return (cdr cell)))

(defun eas-compile-memo--trim (cells)
  "Cut CELLS to `eas-compile-memo-frame-limit' entries in place; return them."
  (let ((tail (nthcdr (1- eas-compile-memo-frame-limit) cells)))
    (when tail (setcdr tail nil))
    cells))

(defun eas-compile-memo-transform-run (transforms rows env path)
  "Return `eas-transform-run' of TRANSFORMS on ROWS under ENV.
PATH is for errors.  Within `eas-compile-memo', start from the longest
prefix of TRANSFORMS already run on the same ROWS."
  (if (or (null eas-compile-memo--runs) (zerop (length transforms)) (not (vectorp rows)))
      (eas-transform-run transforms rows env path)
    (let* ((all (append transforms nil)) (n (length all))
           (source (gethash rows eas-compile-memo--sources))
           ;; Steps 0..KEEP-1 may be kept across compiles.
           (keep (if source (or (cl-position-if-not #'eas-compile-memo--keepable-p all) n) 0))
           ;; PREFIXES[K] is the first K transforms; FRAME-KEYS[K] its key
           ;; across compiles (PREFIX PARAMS GLOBALS), made when needed.
           (prefixes (let ((v (make-vector (1+ n) nil)))
                       (dotimes (k n) (aset v (1+ k) (seq-take all (1+ k))))
                       v))
           (frame-keys (make-vector (1+ n) nil))
           (env-keys nil) (globals nil) (made nil)
           (frame-key
            (lambda (k)
              (or (aref frame-keys k)
                  (progn
                    (unless made
                      (setq made t
                            env-keys (cl-loop for (key _) on env by #'cddr collect key)
                            globals (mapcar (lambda (s) (and (boundp s) (symbol-value s)))
                                            eas-compile-memo-globals)))
                    (let ((named (cl-loop for tr in (aref prefixes k)
                                          append (eas-compile-memo--mentions tr env-keys))))
                      (aset frame-keys k
                            (list (aref prefixes k)
                                  (cl-loop for (key v) on env by #'cddr
                                           when (memq key named) append (list key v))
                                  globals)))))))
           (k n) (out nil))
      (while (and (> k 0) (not out))
        (let ((prefix (aref prefixes k)))
          (setq out (or (eas-compile-memo--run (gethash rows eas-compile-memo--runs) prefix env)
                        (when-let* (((<= k keep))
                                    (runs (gethash source eas-compile-memo--frames))
                                    (cell (assoc (funcall frame-key k) runs))
                                    (hit (cdr cell)))
                          ;; Most recently used first: a slider's one-off
                          ;; runs must not push out the prefix every step needs.
                          (unless (eq cell (car runs))
                            (puthash source (cons cell (delq cell runs)) eas-compile-memo--frames))
                          (eas-compile-memo--count :frame-hits)
                          (push (cons (cons prefix env) hit) (gethash rows eas-compile-memo--runs))
                          hit)))
          (if out (eas-compile-memo--count :hits) (setq k (1- k)))))
      (cl-loop for j from k below n
               do (eas-compile-memo--count :steps)
               ;; Built-in transforms never change their input rows
               ;; (`eas-plist-put' copies), so a step reads the previous
               ;; step's run as is; a registered one gets copies.
               (setq out (eas-transform-run (vector (nth j all))
                                            (if (and out (plist-get (nth j all) :x-eas:transform))
                                                (eas-compile-memo--copy out)
                                              (or out rows))
                                            env path j))
               (push (cons (cons (aref prefixes (1+ j)) env) out) (gethash rows eas-compile-memo--runs))
               (when (< j keep)
                 (puthash source (eas-compile-memo--trim
                                  (cons (cons (funcall frame-key (1+ j)) out)
                                        (gethash source eas-compile-memo--frames)))
                          eas-compile-memo--frames)))
      (eas-compile-memo--copy out))))

(provide 'eas-compile-memo)
;;; eas-compile-memo.el ends here
