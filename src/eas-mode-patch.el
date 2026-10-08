;;; eas-mode-patch.el --- rewrite only the changed cells of a text chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (fc-qx1.2).  In a terminal, moving point moves the
;; crosshair column, so a hover redraw changes a column or two of the
;; grid.  The fc-qx1.14 tty spike measured a full rewrite of a 100x30
;; grid plus redisplay at 47.3 ms and a one-column change at 5.5 ms, and
;; decided that terminal hover patches only the changed cells.
;; `eas-mode-patch-text' makes the buffer equal (text and properties)
;; to a freshly rendered grid while rewriting only the runs of cells
;; that differ.  A different line count (first draw, resize, a GUI
;; image before) rewrites everything.
;;
;; eas-b2s.1: the buffer reads above cost 2 to 5 ms a frame
;; (byte-compiled), most of a text frame once rendering is cached.
;; `eas-mode-patch-lines' keeps the lines it wrote last (a retained
;; model) and diffs the new lines against them, not against the
;; buffer: a line the renderer reused is `eq' to the old one and costs
;; nothing, a changed line is diffed run by run on strings.  The model
;; holds while the buffer's text is what it wrote (its chars-modified
;; tick and size); otherwise the buffer-reading diff runs.
;;
;; eas-b2s.9: a line may also be an `eas-text-row' record, a repainted
;; row the renderer did not compose into a string.  Its cells are
;; compared with the buffer's and the runs that differ are written in
;; place: the characters inserted one by one, then one
;; `set-text-properties' per run of equal properties.  No substring is
;; made on the way, and eas buffers keep no undo list (`eas-view-mode').
;; Modification hooks still run: they are how others see the frame.

;;; Code:

(require 'cl-lib)
(require 'eas-text)

(defconst eas-mode-patch-ignored '(fontified)
  "Text properties redisplay adds that do not count as a change.")

(defun eas-mode-patch--subset-p (a b)
  "Non-nil when every property of plist A but the ignored ones is equal in B."
  (cl-loop for (k v) on a by #'cddr
           always (or (memq k eas-mode-patch-ignored)
                      (and (plist-member b k) (equal v (plist-get b k))))))

(defun eas-mode-patch--line-count ()
  "Lines in the buffer: one more than its newlines."
  (save-excursion
    (goto-char (point-min))
    (let ((n 1)) (while (search-forward "\n" nil t) (setq n (1+ n))) n)))

(defun eas-mode-patch--buffer (lines)
  "Make the current buffer LINES (a list), diffing against its text.
Returns the number of characters inserted."
  (if (/= (length lines) (eas-mode-patch--line-count))
      (let ((text (mapconcat #'eas-text-row-string lines "\n"))) (erase-buffer) (insert text) (length text))
    (save-excursion
      (goto-char (point-min))
      (let ((written 0))
        (dolist (line lines written)
          (let ((bol (point)) (eol (line-end-position)))
            (setq written (+ written (eas-mode-patch--cells bol eol line)))
            (goto-char bol)
            (forward-line 1)))))))

;;; Cells written in place (eas-b2s.9)

(defvar eas-mode-patch--chars (make-vector 128 nil)
  "Scratch: the characters of the line being written.")

(defvar eas-mode-patch--props (make-vector 128 nil)
  "Scratch: the text properties of each character of the line being written.")

(defun eas-mode-patch--scratch (n)
  "Make the scratch vectors hold at least N cells."
  (when (< (length eas-mode-patch--chars) n)
    (setq eas-mode-patch--chars (make-vector (* 2 n) nil) eas-mode-patch--props (make-vector (* 2 n) nil))))

(defun eas-mode-patch--fill (line)
  "Put the cells of LINE (a string or `eas-text-row') in the scratch.
Return its length."
  (if (stringp line)
      (let ((n (length line)))
        (eas-mode-patch--scratch n)
        (let ((chars eas-mode-patch--chars) (props eas-mode-patch--props) (j 0))
          (while (< j n)
            (let ((p (text-properties-at j line)) (e (next-property-change j line n)))
              (while (< j e) (aset chars j (aref line j)) (aset props j p) (setq j (1+ j))))))
        n)
    (eas-mode-patch--scratch (eas-text-row-cols line))
    (eas-text-row-scan line (lambda (j char props)
                              (aset eas-mode-patch--chars j char)
                              (aset eas-mode-patch--props j props)))))

(defun eas-mode-patch--same-props (a b)
  "Non-nil when plists A and B hold the same properties, ignored ones aside."
  (or (eq a b) (equal a b)
      (and (eas-mode-patch--subset-p a b) (eas-mode-patch--subset-p b a))))

(defun eas-mode-patch--props-write (pos s e)
  "Set the properties of the buffer cells from POS to scratch cells S to E.
One `set-text-properties' per run of `eq' plists."
  (let ((props eas-mode-patch--props) (j s))
    (while (< j e)
      (let ((p (aref props j)) (k (1+ j)))
        (while (and (< k e) (eq (aref props k) p)) (setq k (1+ k)))
        (set-text-properties (+ pos (- j s)) (+ pos (- k s)) p)
        (setq j k)))))

(defun eas-mode-patch--write (pos s e &optional fresh)
  "Make the buffer cells from POS the scratch cells S to E; return E - S.
FRESH means the cells are not in the buffer yet: they are inserted at
POS.  Characters are replaced only when one differs; properties are
set once per run of `eq' plists.  Modification hooks see one change
per run, as for a `delete-region' and `insert'."
  (let ((chars eas-mode-patch--chars))
    (if (or fresh (let ((k s)) (while (and (< k e) (eq (char-after (+ pos (- k s))) (aref chars k))) (setq k (1+ k)))
                    (< k e)))
        (combine-change-calls pos (if fresh pos (+ pos (- e s)))
          (goto-char pos)
          (unless fresh (delete-region pos (+ pos (- e s))))
          (let ((j s)) (while (< j e) (insert-char (aref chars j) 1) (setq j (1+ j))))
          (eas-mode-patch--props-write pos s e))
      (eas-mode-patch--props-write pos s e))
    (- e s)))

(defun eas-mode-patch--cells (bol eol line)
  "Rewrite the buffer between BOL and EOL to LINE, reading the buffer.
LINE is a string or an `eas-text-row'.  Only the runs of cells that
differ are written.  Return the number of cells written."
  (let* ((n (eas-mode-patch--fill line)) (len (- eol bol)) (m (min n len)) (j 0) (written 0)
         (chars eas-mode-patch--chars) (props eas-mode-patch--props))
    ;; The tail first: the common part keeps its positions.
    (cond ((> len n) (delete-region (+ bol n) eol))
          ((> n len) (setq written (eas-mode-patch--write eol len n t))))
    (while (< j m)
      (let ((pos (+ bol j)))
        (if (and (eq (char-after pos) (aref chars j))
                 (eas-mode-patch--same-props (text-properties-at pos) (aref props j)))
            (setq j (1+ j))
          (let ((k (1+ j)))
            (while (and (< k m)
                        (not (and (eq (char-after (+ bol k)) (aref chars k))
                                  (eas-mode-patch--same-props (text-properties-at (+ bol k)) (aref props k)))))
              (setq k (1+ k)))
            (setq written (+ written (eas-mode-patch--write pos j k)) j k)))))
    written))

(defvar-local eas-mode-patch--model nil
  "What `eas-mode-patch-lines' wrote last: [LINES CHARS-TICK SIZE].
LINES is a vector of strings; the model is good while the buffer's
`buffer-chars-modified-tick' and size are still CHARS-TICK and SIZE.")

(defun eas-mode-patch--string-runs (old new)
  "Find where strings OLD and NEW differ, as a list of (START . END).
Only their common length counts.  A cell differs in its character or
its properties.  Runs come last first."
  (let ((n (min (length old) (length new))) (j 0) (runs nil) (start nil))
    (while (< j n)
      (let* ((e (min (next-property-change j old n) (next-property-change j new n)))
             (same-props (let ((a (text-properties-at j old)) (b (text-properties-at j new)))
                           (or (eq a b) (equal a b)))))
        (if (not same-props)
            (progn (unless start (setq start j)) (setq j e))
          (while (< j e)
            (if (eq (aref old j) (aref new j))
                (when start (push (cons start j) runs) (setq start nil))
              (unless start (setq start j)))
            (setq j (1+ j))))))
    (when start (push (cons start n) runs))
    runs))

(defun eas-mode-patch--retained (old new)
  "Rewrite the buffer, showing line vector OLD, to line vector NEW.
They have the same length.  Two strings are diffed with each other; a
line that is an `eas-text-row' (either) is diffed with the buffer.
The cells that differ are written in place.  Returns cells written."
  (let ((n (length new)) (written 0))
    (save-excursion
      (goto-char (point-min))
      (dotimes (i n)
        (let ((a (aref old i)) (b (aref new i)) (bol (point)))
          (unless (eq a b)
            (if (and (stringp a) (stringp b))
                (unless (equal-including-properties a b)
                  (let ((la (length a)) (lb (length b)))
                    (eas-mode-patch--fill b)
                    (cond ((> la lb) (delete-region (+ bol lb) (+ bol la)))
                          ((> lb la) (setq written (+ written (eas-mode-patch--write (+ bol la) la lb t)))))
                    (pcase-dolist (`(,s . ,e) (eas-mode-patch--string-runs a b))
                      (setq written (+ written (eas-mode-patch--write (+ bol s) s e))))))
              (setq written (+ written (eas-mode-patch--cells bol (line-end-position) b)))))
          (goto-char bol)
          (forward-line 1))))
    written))

(defun eas-mode-patch-lines (lines)
  "Make the current buffer LINES joined by newlines, rewriting only what differs.
LINES is a list of strings without newlines, or `eas-text-row' records
\(`eas-text-render-rows').  Returns the number of cells written."
  (let* ((new (vconcat lines)) (old (and (eas-mode-patch--model-p) (aref eas-mode-patch--model 0))))
    (prog1 (if (and old (= (length old) (length new)))
               (eas-mode-patch--retained old new)
             (eas-mode-patch--buffer lines))
      (setq eas-mode-patch--model (vector new (buffer-chars-modified-tick) (buffer-size))))))

(defun eas-mode-patch--model-p ()
  "Non-nil when the retained model still describes the current buffer."
  (let ((model eas-mode-patch--model))
    (and model (eql (aref model 1) (buffer-chars-modified-tick)) (eql (aref model 2) (buffer-size)))))

(defun eas-mode-patch-last-line (line)
  "Make the last line of the current buffer LINE, when the model knows it.
Returns nil, changing nothing, when the retained model is stale; else
the number of characters inserted."
  (when (eas-mode-patch--model-p)
    (let ((old (aref eas-mode-patch--model 0)))
      ;; The redraw wrote this very line a moment ago (eas-b2s.9).
      (if (and (> (length old) 0) (eq line (aref old (1- (length old))))) 0
        (let ((lines (append old nil)))
          (eas-mode-patch-lines (nconc (butlast lines) (list line))))))))

(defun eas-mode-patch-text (text)
  "Make the current buffer TEXT, rewriting only the cells that differ.
Returns the number of characters inserted."
  (eas-mode-patch-lines (split-string text "\n")))

(provide 'eas-mode-patch)
;;; eas-mode-patch.el ends here
