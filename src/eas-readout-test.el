;;; eas-readout-test.el --- tests for the reserved hover readout -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-anj: the readout's lines are reserved up front, so hovering
;; never changes any other cell or pixel; x-eas.readout and
;; x-eas.tooltip are component trees; `eas-readout-functions' is the
;; Lisp hook; echo-area tooltips stay on one line.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-mode)

(defconst eas-readout-test--spec
  '(:data (:values [(:t 1 :open 3 :close 4 :s "a") (:t 2 :open 5 :close 4 :s "a") (:t 3 :open 4 :close 6 :s "a")
                    (:t 4 :open 6 :close 9 :s "a") (:t 5 :open 9 :close 7 :s "a") (:t 6 :open 7 :close 8 :s "a")
                    (:t 7 :open 8 :close 8 :s "a") (:t 8 :open 8 :close 3 :s "a")])
    :width 300 :height 120
    :mark (:type "line" :point t)
    :encoding (:x (:field "t" :type "quantitative") :y (:field "close" :type "quantitative")
               :tooltip [(:field "t" :type "quantitative") (:field "open" :type "quantitative")
                         (:field "close" :type "quantitative")]))
  "A line with points and a three-field tooltip; no selection params.")

(defmacro eas-readout-test--with (var spec &rest body)
  "Open SPEC as VAR (a text view) in a fresh registry; run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal)) (eas-action-inhibit t) (inhibit-message t))
     (let ((,var (eas-view-open ,spec :id "r" :target 'text :size '(:cols 60 :rows 14))))
       ,@body)))

(defun eas-readout-test--with-readout (readout &optional key)
  "The test spec with x-eas KEY (default readout) READOUT."
  (append (list :x-eas (list (or key :readout) readout)) eas-readout-test--spec))

(defun eas-readout-test--split (buffer)
  "BUFFER's text as (CHART . READOUT): the cells above the readout and it."
  (with-current-buffer buffer
    (let ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
      (cons (buffer-substring (point-min) beg) (buffer-substring-no-properties beg (point-max))))))

;;; No jitter

(ert-deftest eas-readout-hover-never-changes-a-non-readout-cell ()
  "Property: hovering random cells changes only the readout's line."
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t) (eas-action-inhibit t))
    (dolist (readout (list nil '(:fields [(:field "t") (:field "open") (:field "close")
                                          (:component "when" :props (:test "datum.close >= datum.open"
                                                                     :style (:color "green" :bold t))
                                           :children [(:component "badge" :props (:text "UP"))])])))
      (let* ((v (eas-view-open (if readout (eas-readout-test--with-readout readout) eas-readout-test--spec)
                               :id "tty"))
             (buffer (eas-show v 'text)))
        (unwind-protect
            (with-current-buffer buffer
              (random "eas-anj")
              (let* ((start (eas-readout-test--split buffer))
                     (cols (plist-get (eas-view-size v) :cols))
                     (lines (count-lines (point-min) (point-max)))
                     (chart-end (length (car start)))
                     (readouts nil) (hovers 0))
                (dotimes (_ 60)
                  (goto-char (1+ (random chart-end)))
                  (eas-mode--post-command)
                  (when (plist-get (eas-view-state v) :hover) (cl-incf hovers))
                  (let ((now (eas-readout-test--split buffer)))
                    ;; Every cell above the readout is as it was, properties too.
                    (should (equal-including-properties (car now) (car start)))
                    (should (= (count-lines (point-min) (point-max)) lines))
                    (should-not (string-search "\n" (cdr now)))
                    (should (<= (string-width (cdr now)) cols))
                    (push (cdr now) readouts)))
                ;; The walk did hover data, and the readout followed it.
                (should (> hovers 0))
                (should (> (length (delete-dups readouts)) 1))))
          (kill-buffer buffer))))))

(ert-deftest eas-readout-hover-never-changes-a-chart-pixel ()
  "Property: in SVG the chart image is fixed and the readout image too."
  (eas-readout-test--with v (eas-readout-test--with-readout '(:component "fields" :props (:source "auto")))
    (let ((v (eas-view-open (eas-readout-test--with-readout '(:component "fields" :props (:source "auto")))
                            :id "gui" :target 'svg :size '(400 . 200))))
      (random "eas-anj-svg")
      (let ((chart (eas-svg-render (eas-view-scene v)))
            (box (lambda (svg) (and (string-match "<svg[^>]*width=\"\\([0-9]+\\)\" height=\"\\([0-9]+\\)\"" svg)
                                    (list (match-string 1 svg) (match-string 2 svg)))))
            (sizes nil) (texts nil))
        (dotimes (_ 40)
          (eas-dispatch v (list :type "pointermove" :px (vector (random 400) (random 200))))
          (should (equal (eas-svg-render (eas-view-scene v)) chart))
          (let ((svg (eas-readout-svg v 400 18)))
            (push (funcall box svg) sizes)
            (push svg texts)))
        (should (equal (delete-dups sizes) '(("400" "18"))))
        (should (> (length (delete-dups texts)) 1))))
    (ignore v)))

(ert-deftest eas-readout-lines-are-reserved-from-the-window ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t))
    (let* ((one (eas-view-open eas-readout-test--spec :id "one"))
           (two (eas-view-open (eas-readout-test--with-readout '(:max_lines 2)) :id "two")))
      (should (= (eas-readout-max-lines one) 1))
      (should (= (eas-readout-max-lines two) 2))
      (with-temp-buffer
        (setq-local eas-mode--view one)
        (let ((rows-one (plist-get (eas-mode--window-size (selected-window) 'text) :rows))
              (px-one (cdr (eas-mode--window-size (selected-window) 'svg))))
          (setq-local eas-mode--view two)
          (should (= (plist-get (eas-mode--window-size (selected-window) 'text) :rows)
                     (max 6 (1- rows-one))))
          (should (< (cdr (eas-mode--window-size (selected-window) 'svg)) px-one))))
      ;; A two-line readout always takes two lines, hovered or not.
      (let ((buffer (eas-show two 'text)))
        (unwind-protect
            (should (= 2 (length (split-string (cdr (eas-readout-test--split buffer)) "\n"))))
          (kill-buffer buffer))))))

(ert-deftest eas-readout-mode-reserves-the-header-line ()
  (with-temp-buffer
    (eas-view-mode)
    (should (equal header-line-format ""))
    (should (eq show-help-function #'eas-readout-show-help))))

;;; Content

(ert-deftest eas-readout-default-is-the-values-strip ()
  (eas-readout-test--with v eas-readout-test--spec
    (should (equal (substring-no-properties (eas-readout-text v 59)) "latest  t=8  close=3"))
    (should (equal (substring-no-properties (eas-mode-strip-string v)) " latest  t=8  close=3"))
    ;; Narrow: the position word goes first, then later fields.
    (should (equal (substring-no-properties (eas-readout-text v 12)) "t=8  close=3"))
    (should (equal (substring-no-properties (eas-readout-text v 4)) "t=8"))))

(ert-deftest eas-readout-reads-the-hovered-datum ()
  (eas-readout-test--with v (eas-readout-test--with-readout
                             '(:component "row"
                               :children [(:component "when" :props (:test "hovered") :children ["hover"]
                                           :else [(:component "text" :props (:text (:expr "at")))])
                                          (:component "fields" :props (:labelSep ": "))
                                          (:field "close" :label "C" :rules [(:test "datum.close < datum.open"
                                                                                :color "red")])]))
    (should (equal (substring-no-properties (eas-readout-text v 59)) "latest  t: 8  close: 3  C=3"))
    (should (equal '(:foreground "red") (get-text-property 26 'face (eas-readout-text v 59))))
    (let* ((scales (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :scales))
           (px (vector (eas-scale-apply (plist-get scales :x) 4) (eas-scale-apply (plist-get scales :y) 9))))
      (eas-dispatch v (list :type "pointermove" :px px))
      (should (plist-get (eas-view-state v) :hover))
      (should (equal (plist-get (plist-get (eas-readout-context v) :datum) :close) 9))
      (should (equal (substring-no-properties (eas-readout-text v 59)) "hover  t: 4  open: 6  close: 9  C=9")))))

(ert-deftest eas-readout-hook-says-what-a-spec-cannot ()
  (eas-readout-test--with v eas-readout-test--spec
    (let ((eas-readout-functions
           (list (lambda (datum view _ctx)
                   (when (equal (eas-view-id view) "r")
                     (concat (propertize "close " 'face 'bold)
                             (propertize (format "%s" (plist-get datum :close)) 'face '(:foreground "red"))))))))
      (let ((text (eas-readout-text v 59)))
        (should (equal (substring-no-properties text) "close 3"))
        (should (eq (get-text-property 0 'face text) 'bold))
        (should (equal (get-text-property 6 'face text) '(:foreground "red"))))
      (should (string-match-p "<tspan fill=\"red\">3</tspan>" (eas-readout-svg v 300))))))

(ert-deftest eas-readout-faults-are-shown-and-validated ()
  (eas-readout-test--with v (eas-readout-test--with-readout '(:component "row" :children [(:field "t" :colour "x")]))
    (should (string-prefix-p "readout: Component field has no prop colour"
                             (substring-no-properties (eas-readout-text v 200))))
    (should (equal (plist-get (eas-test-should-code "INVALID_INPUT" (eas-readout-validate v)) :path)
                   "/x-eas/readout/children/0/props/colour")))
  (eas-readout-test--with v (eas-readout-test--with-readout '(:max_lines 0))
    (eas-test-should-code "INVALID_INPUT" (eas-readout-validate v))
    (should (= (eas-readout-max-lines v) 1))))

;;; Tooltips

(ert-deftest eas-readout-echo-tooltips-stay-on-one-line ()
  (should (equal (eas-readout-echo-line "date: Mar 03, 2026\nvalue: 104.1" 80)
                 "date: Mar 03, 2026  value: 104.1"))
  (should (equal (eas-readout-echo-line "date: Mar 03, 2026\nvalue: 104.1" 20) "date: Mar 03, 2026"))
  (should (equal (eas-readout-echo-line "short" 80) "short"))
  (let ((shown nil))
    (cl-letf (((symbol-function 'message) (lambda (_fmt &rest args) (setq shown (car args)))))
      (let ((show-help-function nil))
        (eas-readout-show-help "a: 1\nb: 2")
        (should (equal shown "a: 1  b: 2"))))))

(ert-deftest eas-readout-x-eas-tooltip-renders-the-hovered-datum ()
  (eas-readout-test--with v (eas-readout-test--with-readout
                             '(:fields [(:field "close" :label "C" :labelSep " ")
                                        (:field "open" :label "O" :labelSep " ")])
                             :tooltip)
    (should-not (eas-readout-tooltip-string v 80))
    (let* ((scales (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :scales))
           (px (vector (eas-scale-apply (plist-get scales :x) 4) (eas-scale-apply (plist-get scales :y) 9))))
      (eas-dispatch v (list :type "pointermove" :px px))
      (should (equal (substring-no-properties (eas-readout-tooltip-string v 80)) "C 9  O 6"))
      (should (equal (substring-no-properties (eas-mode-tip-text v)) "C 9  O 6")))))

(provide 'eas-readout-test)
;;; eas-readout-test.el ends here
