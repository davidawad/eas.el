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

;;; Code:

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

(defun eas-mode-patch-text (text)
  "Make the current buffer TEXT, rewriting only the cells that differ.
Returns the number of characters inserted."
  (let ((lines (split-string text "\n")))
    (if (/= (length lines) (eas-mode-patch--line-count))
        (progn (erase-buffer) (insert text) (length text))
      (save-excursion
        (goto-char (point-min))
        (let ((written 0))
          (dolist (line lines written)
            (let ((bol (point)) (eol (line-end-position)))
              (setq written (+ written (eas-mode-patch--line bol eol line)))
              (forward-line 1))))))))

(provide 'eas-mode-patch)
;;; eas-mode-patch.el ends here
