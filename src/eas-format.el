;;; eas-format.el --- d3-format number specifiers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega formats numbers with d3-format, in axis `format', tooltips and
;; the expression function format(value, specifier).  This is that
;; formatter for the common specifier grammar
;;
;;   [[fill]align][sign][symbol][0][width][,][.precision][~][type]
;;
;; with types f d % e g r s and none, the "−" minus sign d3 uses,
;; grouping by thousands, trimming (~) and padding.  JavaScript's
;; renderings of non-finite values are kept: NaN, and Infinity, which
;; "d" prints with toLocaleString as "∞".

;;; Code:

(require 'eas-core)

(defconst eas-format--spec-regexp
  (concat "\\`\\(?:\\(.\\)?\\([<>=^]\\)\\)?\\([-+( ]\\)?\\([$#]\\)?\\(0\\)?\\([0-9]+\\)?"
          "\\(,\\)?\\(?:\\.\\([0-9]+\\)\\)?\\(~\\)?\\([a-z%]\\)?\\'")
  "A d3-format specifier.")

(defun eas-format--group (digits)
  "DIGITS (a string of integer digits) grouped by thousands with commas."
  (let ((n (length digits)) (out ""))
    (dotimes (i n)
      (when (and (> i 0) (zerop (% (- n i) 3))) (setq out (concat out ",")))
      (setq out (concat out (string (aref digits i)))))
    out))

(defun eas-format--trim (s)
  "S without insignificant trailing zeros in its fraction (d3's ~)."
  (if (string-match "\\`\\([^.e]*\\)\\(\\.[0-9]*?\\)0*\\([^0-9].*\\)?\\'" s)
      (let ((frac (match-string 2 s)))
        (concat (match-string 1 s) (if (equal frac ".") "" frac) (or (match-string 3 s) "")))
    s))

(defun eas-format--exponential (x p)
  "X in JavaScript's toExponential(P) form."
  (let ((s (format (format "%%.%de" p) x)))
    (if (string-match "e\\([-+]\\)0*\\([0-9]+\\)\\'" s)
        (replace-match (concat "e" (match-string 1 s) (match-string 2 s)) t t s)
      s)))

(defun eas-format--precision (x p)
  "X in JavaScript's toPrecision(P) form."
  (let* ((p (max 1 p))
         (e (if (zerop x) 0 (floor (log (abs (string-to-number (format (format "%%.%de" (1- p)) x))) 10)))))
    (if (or (< e -6) (>= e p)) (eas-format--exponential x (1- p))
      (format (format "%%.%df" (max 0 (- p 1 e))) x))))

(defun eas-format--si (x p)
  "X with an SI prefix at P significant digits (d3's s)."
  (let* ((e (if (zerop x) 0 (floor (log (abs x) 10))))
         (k (max -8 (min 8 (floor e 3))))
         (prefixes ["y" "z" "a" "f" "p" "n" "µ" "m" "" "k" "M" "G" "T" "P" "E" "Z" "Y"]))
    (concat (eas-format--precision (/ x (expt 10.0 (* 3 k))) p) (aref prefixes (+ k 8)))))

(defun eas-format-number (spec value)
  "VALUE formatted by d3-format specifier SPEC (a string)."
  (unless (string-match eas-format--spec-regexp spec)
    (eas-signal "INVALID_INPUT" (format "%S is not a d3-format specifier" spec)))
  (let* ((fill (or (match-string 1 spec) " ")) (align (or (match-string 2 spec) ">"))
         (sign (or (match-string 3 spec) "-")) (symbol (match-string 4 spec))
         (zero (match-string 5 spec)) (width (and (match-string 6 spec) (string-to-number (match-string 6 spec))))
         (comma (match-string 7 spec))
         (precision (and (match-string 8 spec) (string-to-number (match-string 8 spec))))
         (trim (match-string 9 spec)) (type (or (match-string 10 spec) ""))
         (x (cond ((numberp value) (float value))
                  ((stringp value) (let ((s (string-trim value)))
                                     (if (string-match-p "\\`[-+]?[0-9.]+\\(?:e[-+]?[0-9]+\\)?\\'" s)
                                         (string-to-number s) 0.0e+NaN)))
                  (t 0.0e+NaN))))
    (when (equal type "") (setq precision (or precision 12) trim "~" type "g"))
    (when zero (setq fill "0" align "="))
    (let* ((p (or precision (if (equal type "d") 0 6)))
           (neg (and (< x 0) (not (isnan x))))
           (ax (abs x))
           (body (cond
                  ((isnan x) "NaN")
                  ((and (= ax 1.0e+INF) (equal type "d")) "∞")
                  ((= ax 1.0e+INF) "Infinity")
                  (t (pcase type
                       ("d" (format "%d" (round ax)))
                       ("f" (format (format "%%.%df" p) ax))
                       ("%" (format (format "%%.%df" p) (* 100 ax)))
                       ("e" (eas-format--exponential ax p))
                       ("g" (eas-format--precision ax p))
                       ("r" (let ((e (if (zerop ax) 0 (floor (log ax 10)))))
                              (format (format "%%.%df" (max 0 (- p 1 e))) ax)))
                       ("s" (eas-format--si ax p))
                       (_ (format "%s" ax))))))
           (body (if trim (eas-format--trim body) body))
           (body (if (and comma (string-match "\\`\\([0-9]+\\)\\(.*\\)\\'" body))
                     (concat (eas-format--group (match-string 1 body)) (match-string 2 body))
                   body))
           ;; d3 prints negative zero after rounding as zero.
           (neg (and neg (string-match-p "[1-9]" body)))
           (prefix (concat (cond (neg (if (equal sign "(") "(" "−"))
                                 ((equal sign "+") "+") ((equal sign " ") " ") (t ""))
                           (if (equal symbol "$") "$" "")))
           (suffix (concat (if (equal type "%") "%" "") (if (and neg (equal sign "(")) ")" "")))
           (len (+ (length prefix) (length body) (length suffix)))
           (pad (if (and width (< len width)) (apply #'concat (make-list (- width len) fill)) "")))
      (pcase align
        ("<" (concat prefix body suffix pad))
        ("=" (concat prefix pad body suffix))
        ("^" (let ((h (/ (length pad) 2)))
               (concat (substring pad 0 h) prefix body suffix (substring pad h))))
        (_ (concat pad prefix body suffix))))))

(provide 'eas-format)
;;; eas-format.el ends here
