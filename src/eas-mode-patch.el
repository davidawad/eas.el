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

;;; Code:

(require 'cl-lib)

(defconst eas-mode-patch-ignored '(fontified)
  "Text properties redisplay adds that do not count as a change.")

(defun eas-mode-patch--subset-p (a b)
  "Non-nil when every property of plist A but the ignored ones is equal in B."
  (cl-loop for (k v) on a by #'cddr
           always (or (memq k eas-mode-patch-ignored)
                      (and (plist-member b k) (equal v (plist-get b k))))))

(defun eas-mode-patch--same (pos line j)
  "Non-nil when the buffer cell at POS equals cell J of string LINE.
Properties compare as sets: insertion does not keep their order."
  (and (eq (char-after pos) (aref line j))
       (let ((a (text-properties-at pos)) (b (text-properties-at j line)))
         (and (eas-mode-patch--subset-p a b) (eas-mode-patch--subset-p b a)))))

(defun eas-mode-patch--replace (beg end string)
  "Replace the buffer between BEG and END with STRING; return its length."
  (goto-char beg)
  (delete-region beg end)
  (insert string)
  (length string))

(defun eas-mode-patch--line (bol eol line)
  "Rewrite the buffer between BOL and EOL to LINE; return cells rewritten.
Cells are rewritten run by run, so a crosshair that jumps across the
chart touches two columns, not the cells between.  Rendered lines are
right-trimmed, so the part past the shorter length is replaced whole."
  (let* ((new (length line)) (n (min new (- eol bol))) (written 0) (j 0))
    (while (< j n)
      (if (eas-mode-patch--same (+ bol j) line j)
          (setq j (1+ j))
        (let ((k j))
          (while (and (< k n) (not (eas-mode-patch--same (+ bol k) line k))) (setq k (1+ k)))
          (setq written (+ written (eas-mode-patch--replace (+ bol j) (+ bol k) (substring line j k)))
                j k))))
    (if (= (- eol bol) new) written
      (+ written (eas-mode-patch--replace (+ bol n) eol (substring line n))))))

(defun eas-mode-patch--line-count ()
  "Lines in the buffer: one more than its newlines."
  (save-excursion
    (goto-char (point-min))
    (let ((n 1)) (while (search-forward "\n" nil t) (setq n (1+ n))) n)))

(defun eas-mode-patch--buffer (lines)
  "Make the current buffer LINES (a list), diffing against its text.
Returns the number of characters inserted."
  (if (/= (length lines) (eas-mode-patch--line-count))
      (let ((text (mapconcat #'identity lines "\n"))) (erase-buffer) (insert text) (length text))
    (save-excursion
      (goto-char (point-min))
      (let ((written 0))
        (dolist (line lines written)
          (let ((bol (point)) (eol (line-end-position)))
            (setq written (+ written (eas-mode-patch--line bol eol line)))
            (forward-line 1)))))))

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
They have the same length.  Lines are patched last first, so the
starts of the lines above stay put.  Returns characters inserted."
  (let* ((n (length new)) (bols (make-vector n 0)) (pos (point-min)) (written 0))
    (dotimes (i n)
      (aset bols i pos)
      (setq pos (+ pos (length (aref old i)) 1)))
    (save-excursion
      (cl-loop for i downfrom (1- n) to 0
               for a = (aref old i) for b = (aref new i)
               unless (or (eq a b) (equal-including-properties a b))
               do (let ((bol (aref bols i)) (la (length a)) (lb (length b)))
                    (unless (= la lb)
                      (let ((m (min la lb)))
                        (setq written (+ written (eas-mode-patch--replace (+ bol m) (+ bol la) (substring b m))))))
                    (pcase-dolist (`(,s . ,e) (eas-mode-patch--string-runs a b))
                      (setq written (+ written (eas-mode-patch--replace (+ bol s) (+ bol e) (substring b s e))))))))
    written))

(defun eas-mode-patch-lines (lines)
  "Make the current buffer LINES joined by newlines, rewriting only what differs.
LINES is a list of strings without newlines.  Returns the number of
characters inserted."
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
    (let ((lines (append (aref eas-mode-patch--model 0) nil)))
      (eas-mode-patch-lines (nconc (butlast lines) (list line))))))

(defun eas-mode-patch-text (text)
  "Make the current buffer TEXT, rewriting only the cells that differ.
Returns the number of characters inserted."
  (eas-mode-patch-lines (split-string text "\n")))

(provide 'eas-mode-patch)
;;; eas-mode-patch.el ends here
