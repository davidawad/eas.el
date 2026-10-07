;;; eas-svg-retain.el --- retained SVG fragments for live frames -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5 helper, GUI half (eas-b2s.2).  A live frame redraws a scene that
;; differs from the last one in a few items: ten levels of an order
;; book, a clock's hands, the routes of the hovered airport.  Profiled
;; byte-compiled at 0.2.3, a 25-level ladder push spent its SVG time
;; rebuilding the axes (37%), in `svg-print' (28%) and on hot spots
;; (17%), all for parts that had not changed.  This file keeps the
;; printed SVG of each part of a frame, retained between frames and
;; looked up by value:
;;
;;   item   the printed node of one mark item, keyed on the mark kind
;;          and the item plist, so a push re-prints only changed items;
;;   mark   the printed SVG of one mark, made of its items;
;;   part   the printed nodes of an axis or a legend, keyed on it and
;;          the theme, and the :map hot spots of one mark.
;;
;; Marks and parts are each kept in a slot of their own (the view and
;; the part's id), last version only: a hit is one `equal' with it, and
;; a stream's versions do not pile up in a hash bucket.
;;
;; Keys hold every input of the work they stand for, and scenes are
;; values, so a hit is the output a fresh render would make: the SVG
;; stays byte-identical.  An item whose fill is a gradient records a
;; definition as it prints, so it is never retained.  Items are found by
;; content through a hash that sees every point of a series
;; (`eas-svg-retain--hash'), in two generations of bounded size that
;; keep the working set.
;; `eas-svg-retain-print' prints a DOM as `svg-print' does, faster.
;;
;; Everything here is off while `eas-render-cache-enabled' is nil.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'eas-render-cache)

(defvar eas-svg-retain-max-bytes (* 8 1024 1024)
  "Bytes of retained fragments one generation holds before it ages.")

(defvar eas-svg-retain-stats nil
  "Counters of retained fragments, a plist.
Its keys are :item-hits :item-misses :item-reused (items reused by
position in their mark) :part-hits :part-misses.
`eas-svg-retain-clear' resets it.")

(defvar eas-svg-retain-max-parts 4096
  "Slots of retained parts kept before they are all dropped.")

(defun eas-svg-retain--vector-hash (v)
  "A hash of every element of vector V."
  (let ((h (length v)))
    (dotimes (i (length v))
      (setq h (logxor (* 31 (logand h #xFFFFFFF)) (sxhash-equal (aref v i)))))
    h))

(defconst eas-svg-retain--series '("line" "area" "trail" "geoshape")
  "Mark kinds whose item holds a long vector of points.")

(defun eas-svg-retain--hash (key)
  "A hash of KEY, (KIND . ITEM), that sees every point of a series.
`sxhash-equal' looks 3 levels down and at the first 7 elements of a
vector: every version of a series whose first points stay would share
a bucket, and a long stream would walk a growing chain of them on each
lookup."
  (let ((h (sxhash-equal key)))
    (when (member (car key) eas-svg-retain--series)
      (dolist (v (cdr key))
        (when (and (vectorp v) (> (length v) 7))
          (setq h (logxor (* 31 (logand h #xFFFFFFF)) (eas-svg-retain--vector-hash v))))))
    h))

(define-hash-table-test 'eas-svg-retain #'equal #'eas-svg-retain--hash)

(defvar eas-svg-retain--parts nil
  "Retained parts by slot: SLOT -> (KEY . VALUE).")

(defvar eas-svg-retain--new nil "Fragments of this generation.")
(defvar eas-svg-retain--old nil "Fragments of the last generation.")
(defvar eas-svg-retain--bytes 0 "Bytes held by `eas-svg-retain--new'.")

(defun eas-svg-retain-clear ()
  "Forget every retained fragment and reset `eas-svg-retain-stats'."
  (interactive)
  (setq eas-svg-retain--new (make-hash-table :test 'eas-svg-retain)
        eas-svg-retain--old (make-hash-table :test 'eas-svg-retain)
        eas-svg-retain--parts (make-hash-table :test 'equal)
        eas-svg-retain--bytes 0
        eas-svg-retain-stats (list :item-hits 0 :item-misses 0 :item-reused 0 :part-hits 0 :part-misses 0)))

(eas-svg-retain-clear)

(defun eas-svg-retain--count (key)
  "Add 1 to counter KEY of `eas-svg-retain-stats'."
  (setq eas-svg-retain-stats (plist-put eas-svg-retain-stats key (1+ (plist-get eas-svg-retain-stats key)))))

(defun eas-svg-retain--size (value)
  "Return the bytes VALUE takes up, about: its strings and list cells."
  (if (stringp value) (length value)
    ;; A list of strings, or of lists led by one; anything else counts 64.
    (let ((n 8))
      (while (consp value)
        (let ((e (car value)))
          (setq n (+ n 16 (cond ((stringp e) (length e))
                                ((and (consp e) (stringp (car e))) (length (car e)))
                                (t 64)))
                value (cdr value))))
      n)))

(defun eas-svg-retain--get (key)
  "Return the value retained under KEY, or nil.
A value of the last generation is moved to this one."
  (or (gethash key eas-svg-retain--new)
      (when-let* ((old (gethash key eas-svg-retain--old)))
        (eas-svg-retain--put key old))))

(defun eas-svg-retain--put (key value)
  "Retain VALUE under KEY; return VALUE."
  (let ((size (+ 64 (eas-svg-retain--size value))))
    (when (> (+ eas-svg-retain--bytes size) eas-svg-retain-max-bytes)
      (setq eas-svg-retain--old eas-svg-retain--new
            eas-svg-retain--new (make-hash-table :test 'eas-svg-retain)
            eas-svg-retain--bytes 0))
    (cl-incf eas-svg-retain--bytes size)
    (puthash key value eas-svg-retain--new)))

(defun eas-svg-retain-memo (key fn)
  "The value of FN (no arguments), retained under KEY, a list.
FN's value must be non-nil and depend only on what KEY holds.  Values
are found by content, so a state seen again (a hover going back) hits."
  (if (not eas-render-cache-enabled) (funcall fn)
    (let ((hit (eas-svg-retain--get key)))
      (if hit
          (progn (eas-svg-retain--count :item-hits) hit)
        (eas-svg-retain--count :item-misses)
        (eas-svg-retain--put key (funcall fn))))))

(defun eas-svg-retain-part (slot key fn)
  "The value of FN (no arguments) for KEY, retained in SLOT.
SLOT names a part of a scene (an axis of a view, say); it keeps the
value of its last KEY only, so a hit is one `equal' against it.  FN's
value must depend only on what KEY holds."
  (if (not eas-render-cache-enabled) (funcall fn)
    (let ((entry (gethash slot eas-svg-retain--parts)))
      (if (and entry (equal (car entry) key))
          (progn (eas-svg-retain--count :part-hits) (cdr entry))
        (eas-svg-retain--count :part-misses)
        (when (>= (hash-table-count eas-svg-retain--parts) eas-svg-retain-max-parts)
          (clrhash eas-svg-retain--parts))
        (let ((value (funcall fn)))
          (puthash slot (cons key value) eas-svg-retain--parts)
          value)))))

;;; Printing

(defun eas-svg-retain--pieces (dom acc)
  "Push the strings of DOM's XML onto ACC, last first; return ACC.
This is `svg-print''s output: a string goes in as it is, and attributes
whose name starts with a colon are dropped."
  (if (stringp dom) (cons dom acc)
    (let ((tag (format "%s" (car dom))))
      (push "<" acc) (push tag acc)
      (dolist (attr (nth 1 dom))
        (let ((name (format "%s" (car attr))) (v (cdr attr)))
          (unless (eq (aref name 0) ?:)
            (setq acc (cl-list* "\"" (if (stringp v) v (format "%s" v)) "=\"" name " " acc)))))
      (push ">" acc)
      (dolist (elem (nthcdr 2 dom)) (setq acc (eas-svg-retain--pieces elem acc)))
      (cl-list* ">" tag "</" acc))))

(defun eas-svg-retain-string (nodes)
  "NODES, a list of DOM nodes and strings, printed to one string."
  (if (and nodes (null (cdr nodes)) (stringp (car nodes))) (car nodes)
    (let ((acc nil))
      (dolist (node nodes) (setq acc (eas-svg-retain--pieces node acc)))
      (apply #'concat (nreverse acc)))))

(defun eas-svg-retain-print (dom)
  "Insert DOM's XML at point, as `svg-print' does."
  (insert (eas-svg-retain-to-string dom)))

(defun eas-svg-retain-to-string (dom)
  "DOM's XML as a multibyte string, what `svg-print' would insert.
One `concat' of the pieces: inserting them one by one grows a buffer's
gap a little at a time, about 25 bytes allocated per byte printed."
  (string-to-multibyte (apply #'concat (nreverse (eas-svg-retain--pieces dom nil)))))

(defvar eas-paint--svg-defs)

(defun eas-svg-retain-item (kind item node-fn)
  "Return the printed node of ITEM of a mark of KIND, made by NODE-FN.
Nil when NODE-FN gives no node.  An item that records a gradient
definition is printed afresh each time, so the definition is recorded."
  (if (or (not eas-render-cache-enabled) (plist-get item :gradient))
      (let ((node (funcall node-fn))) (and node (eas-svg-retain-string (list node))))
    (let ((s (eas-svg-retain-memo (cons kind item)
                                  (lambda () (let ((node (funcall node-fn)))
                                               (if node (eas-svg-retain-string (list node)) 'none))))))
      (and (stringp s) s))))


(defun eas-svg-retain--merge-defs (defs)
  "Append each of DEFS whose id is new to `eas-paint--svg-defs'.
This is the order and dedup `eas-paint' records gradients in."
  (dolist (d defs)
    (unless (seq-some (lambda (o) (equal (dom-attr o 'id) (dom-attr d 'id))) eas-paint--svg-defs)
      (setq eas-paint--svg-defs (append eas-paint--svg-defs (list d))))))

(defun eas-svg-retain-count-reused (n)
  "Add N items reused by position to :item-reused of `eas-svg-retain-stats'."
  (unless (zerop n)
    (setq eas-svg-retain-stats (plist-put eas-svg-retain-stats :item-reused
                                          (+ n (or (plist-get eas-svg-retain-stats :item-reused) 0))))))

(defun eas-svg-retain-mark (slot mark strings-fn)
  "Return the SVG text of MARK's items, from STRINGS-FN, retained in SLOT.
STRINGS-FN is called with the items and per-item texts of the slot's
last version of the mark (nil, nil when there is none) and returns
\(STRINGS . PER-ITEM) as `eas-svg--mark-strings' does.  The gradient
definitions it records in `eas-paint--svg-defs' are recorded again on
a hit.  Hits and misses count in `eas-render-cache-stats' as :svg-hits
and :svg-misses.  A slot keeps the last version of its mark only:
`eas-render-cache-svg-fragment', keyed on the mark, kept every version
of a pushed mark in one bucket."
  ;; An entry is (MARK TEXT DEFS ITEMS PER-ITEM).
  (let* ((entry (gethash slot eas-svg-retain--parts))
         (hit (and entry (equal (car entry) mark) entry))
         (value (or hit
                    (let* ((eas-paint--svg-defs nil)
                           (printed (funcall strings-fn (plist-get (car entry) :items) (nth 4 entry))))
                      (list mark (eas-svg-retain-string (car printed)) eas-paint--svg-defs
                            (plist-get mark :items) (cdr printed))))))
    (eas-render-cache--count (if hit :svg-hits :svg-misses))
    (unless hit
      (when (>= (hash-table-count eas-svg-retain--parts) eas-svg-retain-max-parts)
        (clrhash eas-svg-retain--parts))
      (puthash slot value eas-svg-retain--parts))
    (eas-svg-retain--merge-defs (nth 2 value))
    (nth 1 value)))

(provide 'eas-svg-retain)
;;; eas-svg-retain.el ends here
