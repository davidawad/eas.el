;;; eas-font.el --- text widths with the oracle's font metrics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega sizes axes and legends from measured text.  bin/chart renders
;; with vl-convert, whose font database resolves the generic
;; "sans-serif" to Arial, so layout measures text with Arial's advance
;; widths (taken from Liberation Sans, metric-compatible with Arial by
;; design) rather than Vega's headless 0.8em-per-character guess.
;; Widths are in 1/2048 em; characters outside the tables count as a
;; digit, wide (CJK) characters as one em.

;;; Code:

(defconst eas-font-sans-regular
  [569 569 727 1139 1139 1821 1366 391 682 682 797 1196 569 682 569 569
   1139 1139 1139 1139 1139 1139 1139 1139 1139 1139 569 569 1196 1196 1196 1139
   2079 1366 1366 1479 1479 1366 1251 1593 1479 569 1024 1366 1139 1706 1479 1593
   1366 1593 1479 1366 1251 1479 1366 1933 1366 1366 1251 569 569 569 961 1139
   682 1139 1139 1024 1139 1139 569 1139 1139 455 455 1024 455 1706 1139 1139
   1139 1139 682 1024 569 1139 1024 1479 1024 1024 1024 684 532 684 1196]
  "Arial advance widths for U+0020..U+007E, in 1/2048 em.")

(defconst eas-font-sans-bold
  [569 682 971 1139 1139 1821 1479 487 682 682 797 1196 569 682 569 569
   1139 1139 1139 1139 1139 1139 1139 1139 1139 1139 682 682 1196 1196 1196 1251
   1997 1479 1479 1479 1479 1366 1251 1593 1479 569 1139 1479 1251 1706 1479 1593
   1366 1593 1479 1366 1251 1479 1366 1933 1366 1366 1251 682 569 682 1196 1139
   682 1139 1251 1139 1251 1139 682 1251 1251 569 569 1139 569 1821 1251 1251
   1251 1251 797 1139 682 1251 1139 1593 1139 1139 1024 797 573 797 1196]
  "Arial Bold advance widths for U+0020..U+007E, in 1/2048 em.")

(defconst eas-font-sans-extra
  '((regular . ((#x2212 . 1196) (#x2026 . 2048) (#x00b0 . 819) (#x00b7 . 682) (#x2013 . 1139) (#x2014 . 2048)))
    (bold . ((#x2212 . 1196) (#x2026 . 2048) (#x00b0 . 819) (#x00b7 . 682) (#x2013 . 1139) (#x2014 . 2048))))
  "Advance widths of non-ASCII label characters (minus, ellipsis, dashes).")

(defconst eas-font-serif-regular
  [512 682 836 1024 1024 1706 1593 369 682 682 1024 1155 512 682 512 569
   1024 1024 1024 1024 1024 1024 1024 1024 1024 1024 569 569 1155 1155 1155 909
   1886 1479 1366 1366 1479 1251 1139 1479 1479 682 797 1479 1251 1821 1479 1479
   1139 1479 1366 1139 1251 1479 1479 1933 1479 1479 1251 682 569 682 961 1024
   682 909 1024 909 1024 909 682 1024 1024 569 569 1024 569 1593 1024 1024
   1024 1024 682 797 569 1024 1024 1479 1024 1024 909 983 410 983 1108]
  "Advance widths of ASCII 32-126 in Times New Roman, 1/2048 em.
Taken from Liberation Serif, metric-compatible with Times New Roman.")

(defconst eas-font-serif-bold
  [512 682 1137 1024 1024 2048 1706 569 682 682 1024 1167 512 682 512 569
   1024 1024 1024 1024 1024 1024 1024 1024 1024 1024 682 682 1167 1167 1167 1024
   1905 1479 1366 1479 1479 1366 1251 1593 1593 797 1024 1593 1366 1933 1479 1593
   1251 1593 1479 1139 1366 1479 1479 2048 1479 1479 1366 682 569 682 1190 1024
   682 1024 1139 909 1139 909 682 1024 1139 569 682 1139 569 1706 1139 1024
   1139 1139 909 797 682 1139 1024 1479 1024 1024 909 807 451 807 1065]
  "Advance widths of ASCII 32-126 in Times New Roman Bold, 1/2048 em.")

(defun eas-font-bold-p (weight)
  "Non-nil when CSS font WEIGHT (a number or keyword string) is bold."
  (cond ((numberp weight) (>= weight 600))
        ((stringp weight) (and (member weight '("bold" "bolder" "600" "700" "800" "900")) t))))

(defvar eas-font-family nil
  "Font family TEXT is being measured in, or nil for Arial.
A monospace family (Courier New and kin) is 0.6 em a character, a
serif one (Times New Roman, the generic serif) has Times's widths.")

(defun eas-font-serif-p (&optional family)
  "Non-nil when FAMILY (default `eas-font-family') is a Times-like serif."
  (let ((family (or family eas-font-family)))
    (and (stringp family)
         (string-match-p "\\`\\(?:serif\\|times\\|liberation serif\\)" (downcase family)))))

(defun eas-font-mono-p (&optional family)
  "Non-nil when FAMILY (default `eas-font-family') is a monospace family."
  (let ((family (or family eas-font-family)))
    (and (stringp family)
         (string-match-p "\\`\\(?:courier\\|monospace\\|menlo\\|consolas\\|monaco\\)" (downcase family)))))

(defun eas-font-text-width (text size &optional weight)
  "Width in pixels of TEXT at SIZE px with font WEIGHT.
Set in Arial, or in `eas-font-family' when that is monospace or serif."
  (if (eas-font-mono-p)
      (* 0.6 size (length text))
    (eas-font--arial-width text size weight (eas-font-serif-p))))

(defun eas-font--arial-width (text size weight &optional serif)
  "Width of TEXT in Arial at SIZE px with font WEIGHT.
SERIF measures ASCII in Times New Roman instead."
  (let* ((bold (eas-font-bold-p weight))
         (table (if serif (if bold eas-font-serif-bold eas-font-serif-regular)
                  (if bold eas-font-sans-bold eas-font-sans-regular)))
         (extra (cdr (assq (if bold 'bold 'regular) eas-font-sans-extra)))
         (units 0))
    (dotimes (i (length text))
      (let ((c (aref text i)))
        (setq units (+ units (cond ((<= 32 c 126) (aref table (- c 32)))
                                   ((cdr (assq c extra)))
                                   ((> (char-width c) 1) 2048)
                                   (t 1139))))))
    (/ (* units size) 2048.0)))

(provide 'eas-font)
;;; eas-font.el ends here
