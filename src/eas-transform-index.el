;;; eas-transform-index.el --- range filters answered from a sorted index -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1 (eas-b2s.3).  A slider filter such as
;;
;;   {"filter": "datum.data <= num_points"}
;;
;; runs on every step over the same rows: the transforms before it are
;; kept across compiles (eas-compile-memo.el), so its input is the same
;; vector each time.  `eas-transform-index-filter' answers a comparison
;; of one field with one param (< <= > >= == ===, either side) from an
;; index of that vector built once: a slice when the field never
;; decreases in row order (sequences, timestamps), else the sorted
;; positions.  The rows come back in their order, as the plain filter
;; returns them.  Anything else, or a field or param that is not a
;; number on every row, answers nil and the filter tests row by row.

;;; Code:

(require 'cl-lib)
(require 'eas-core)
(require 'eas-expr)

(defvar eas-transform-index-min-rows 256
  "Fewer rows than this are filtered row by row.")

(defvar eas-transform-index--indexes (make-hash-table :test 'eq :weakness 'key)
  "Rows vector -> ((FIELD-KEY . INDEX) ...).
INDEX is what `eas-transform-index--build' made.")

(defvar eas-transform-index--shapes (make-hash-table :test 'equal)
  "Filter expression -> (OP FIELD-KEY PARAM-KEY), or `none'.")

(defconst eas-transform-index--flip '(("<" . ">") ("<=" . ">=") (">" . "<") (">=" . "<=")
                                      ("==" . "==") ("===" . "==="))
  "Each operator with its sides swapped.")

(defun eas-transform-index--field (ast)
  "The field name of AST when it is datum.FIELD or datum[\"FIELD\"]."
  (pcase ast
    (`(:member (:var "datum") (:lit ,(and f (pred stringp)))) (and (not (equal f "length")) f))))

(defun eas-transform-index--shape (expr)
  "(OP FIELD-KEY PARAM-KEY) when EXPR compares datum.FIELD with a param."
  (let ((shape (gethash expr eas-transform-index--shapes)))
    (unless shape
      (setq shape
            (or (ignore-errors
                  (pcase (eas-expr-parse expr)
                    (`(:binary ,op ,a ,b)
                     (when (assoc op eas-transform-index--flip)
                       (cond ((and (eas-transform-index--field a) (eq (car-safe b) :var)
                                   (not (member (cadr b) '("datum" "PI" "E"))))
                              (list op (eas-key (eas-transform-index--field a)) (eas-key (cadr b))))
                             ((and (eas-transform-index--field b) (eq (car-safe a) :var)
                                   (not (member (cadr a) '("datum" "PI" "E"))))
                              (list (cdr (assoc op eas-transform-index--flip))
                                    (eas-key (eas-transform-index--field b)) (eas-key (cadr a)))))))))
                'none))
      (puthash expr shape eas-transform-index--shapes))
    (and (consp shape) shape)))

(defun eas-transform-index--build (rows key)
  "Index ROWS by KEY: `monotone', a vector of positions sorted by value, or nil.
Nil when a row's KEY is not a number (or NaN)."
  (catch 'none
    (let ((n (length rows)) (monotone t) (prev nil))
      (dotimes (i n)
        (let ((v (plist-get (aref rows i) key)))
          (unless (and (numberp v) (not (and (floatp v) (isnan v)))) (throw 'none nil))
          (when (and prev (< v prev)) (setq monotone nil))
          (setq prev v)))
      (if monotone 'monotone
        (let ((order (vconcat (number-sequence 0 (1- n)))))
          (sort order (lambda (a b) (< (plist-get (aref rows a) key) (plist-get (aref rows b) key)))))))))

(defun eas-transform-index--of (rows key)
  "ROWS' index by KEY (cached), or nil."
  (let* ((all (gethash rows eas-transform-index--indexes))
         (hit (assq key all)))
    (if hit (cdr hit)
      (let ((index (eas-transform-index--build rows key)))
        (puthash rows (cons (cons key index) all) eas-transform-index--indexes)
        index))))

(defun eas-transform-index--bound (n value-at x strict)
  "The first of N sorted positions whose value is > X (STRICT) or >= X.
VALUE-AT maps a position in the order to its value."
  (let ((lo 0) (hi n))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (if strict (<= (funcall value-at mid) x) (< (funcall value-at mid) x))
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun eas-transform-index-filter (expr rows env)
  "ROWS passing filter expression EXPR under ENV, from an index; or nil.
Nil means EXPR has another shape, ROWS are few, or a value is not a
number: test row by row."
  (when-let* (((and (vectorp rows) (>= (length rows) eas-transform-index-min-rows)))
              (shape (eas-transform-index--shape expr))
              (cell (plist-member env (nth 2 shape)))
              (x (cadr cell))
              ((and (numberp x) (not (and (floatp x) (isnan x)))))
              (index (eas-transform-index--of rows (nth 1 shape))))
    (let* ((key (nth 1 shape)) (n (length rows))
           (value-at (if (eq index 'monotone) (lambda (i) (plist-get (aref rows i) key))
                       (lambda (i) (plist-get (aref rows (aref index i)) key))))
           ;; [FROM, TO) of the sorted order passes.
           (span (pcase (car shape)
                   ("<" (cons 0 (eas-transform-index--bound n value-at x nil)))
                   ("<=" (cons 0 (eas-transform-index--bound n value-at x t)))
                   (">" (cons (eas-transform-index--bound n value-at x t) n))
                   (">=" (cons (eas-transform-index--bound n value-at x nil) n))
                   (_ (cons (eas-transform-index--bound n value-at x nil)
                            (eas-transform-index--bound n value-at x t))))))
      (if (eq index 'monotone)
          (substring rows (car span) (max (car span) (cdr span)))
        (let ((picked (sort (append (substring index (car span) (max (car span) (cdr span))) nil) #'<)))
          (vconcat (mapcar (lambda (i) (aref rows i)) picked)))))))

(provide 'eas-transform-index)
;;; eas-transform-index.el ends here
