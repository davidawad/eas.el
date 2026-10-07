;;; eas-component-test.el --- tests for readout components and the fit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-anj: components render against a context, nest, validate their
;; props with JSON paths, style conditionally, and fit one line.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-component-test--datum
  '(:date "2026-03-11" :open 101.25 :high 103.5 :low 99.75 :close 102.5
    :volume 1234567 :change 1.25 :pct 0.0123)
  "A candle with eight fields.")

(defconst eas-component-test--fields
  [(:field "date" :label "date" :format (:type "time") :priority 100)
   (:field "open" :label "open" :format (:type "number" :decimals 2) :priority 90)
   (:field "high" :label "high" :format (:type "number" :decimals 2) :priority 80)
   (:field "low" :label "low" :format (:type "number" :decimals 2) :priority 80)
   (:field "close" :label "close" :format (:type "number" :decimals 2) :priority 95)
   (:field "volume" :label "volume" :format (:type "number" :decimals 0) :priority 40)
   (:field "change" :label "change" :format (:type "currency") :priority 30)
   (:field "pct" :label "percent" :format (:type "percent" :decimals 2) :priority 20)]
  "Field nodes over `eas-component-test--datum'.")

(defun eas-component-test--render (node &optional datum)
  "Atoms of NODE rendered on DATUM (default the candle)."
  (eas-component-render node (list :datum (or datum eas-component-test--datum) :path "/x-eas/readout")))

(defun eas-component-test--line (node width &optional lines datum)
  "NODE on DATUM fitted to WIDTH columns and LINES lines, as plain strings."
  (mapcar #'eas-component-propertize
          (eas-component-fit (eas-component-test--render node datum) width lines)))

(defun eas-component-test--faces (string)
  "Every face in STRING."
  (cl-loop for i below (length string) for f = (get-text-property i 'face string) when f collect f))

;;; Fit

(ert-deftest eas-component-fits-one-line-at-60-90-140-with-3-to-8-fields ()
  (dolist (n '(3 4 5 6 7 8))
    (let ((node (list :component "row" :children (seq-take eas-component-test--fields n))))
      (dolist (width '(60 90 140))
        (let ((lines (eas-component-test--line node width)))
          (should (= (length lines) 1))
          (should (<= (string-width (car lines)) width))
          (should-not (string-search "\n" (car lines)))
          ;; The two most important fields always survive.
          (should (string-match-p "date=Mar 11, 2026" (car lines)))
          (when (>= n 5) (should (string-match-p "close=102.50" (car lines))))
          (when (= width 140) (should-not (string-search "…" (car lines)))))))
    ;; At 140 columns all eight fit whole.
    (should (equal (car (eas-component-test--line
                         (list :component "row" :children eas-component-test--fields) 140))
                   (concat "date=Mar 11, 2026  open=101.25  high=103.50  low=99.75  close=102.50"
                           "  volume=1,234,567  change=$1.25  percent=1.23%")))))

(ert-deftest eas-component-fit-drops-then-abbreviates-then-elides ()
  (let ((node (list :component "row" :children eas-component-test--fields)))
    ;; 60: the lowest priorities (percent, change, volume, then the
    ;; equal high/low, later first) go before anything is abbreviated.
    (should (equal (car (eas-component-test--line node 60))
                   "date=Mar 11, 2026  open=101.25  high=103.50  close=102.50"))
    ;; One field left over at its full form: labels abbreviate, then
    ;; numbers shorten, then the ellipsis.
    (let ((one (list :component "row" :children [(:field "volume" :label "volume" :keep t)])))
      (should (equal (car (eas-component-test--line one 15)) "volume=1234567"))
      (should (equal (car (eas-component-test--line one 11)) "vol=1234567"))
      (should (equal (car (eas-component-test--line one 9)) "vol=1.23M"))
      (should (equal (car (eas-component-test--line one 6)) "vol=1…")))))

(ert-deftest eas-component-max-lines-wraps-instead-of-dropping ()
  (let* ((node (list :component "row" :children eas-component-test--fields))
         (lines (eas-component-test--line node 60 3)))
    (should (= (length lines) 3))
    (should (cl-every (lambda (l) (<= (string-width l) 60)) lines))
    (should (string-match-p "percent=1.23%" (mapconcat #'identity lines " ")))
    ;; Short content pads to the reserved lines.
    (should (equal (eas-component-test--line "hi" 60 2) '("hi" "")))))

(ert-deftest eas-component-separators-never-dangle ()
  (let ((node '(:component "row" :props (:sep " ")
                :children [(:component "sep") (:field "open" :priority 90) (:component "sep")
                           (:component "sep") (:field "pct" :priority 1) (:component "sep")])))
    (should (equal (car (eas-component-test--line node 80)) "open=101.25 │ pct=0.0123"))
    (should (equal (car (eas-component-test--line node 14)) "open=101.25"))))

;;; Components

(ert-deftest eas-component-conditional-color-bold-and-hide ()
  (let* ((rules [(:test "datum.close >= datum.open" :color "green" :bold t)
                 (:test "datum.close < datum.open" :color "red" :italic t)
                 (:test "datum.volume == 0" :hide t)])
         (node (list :component "row"
                     :children (vector (list :field "close" :rules rules) '(:field "open" :style (:dim t)))))
         (up (car (eas-component-test--line node 80)))
         (down (car (eas-component-test--line node 80 1 (plist-put (copy-sequence eas-component-test--datum)
                                                                   :close 100))))
         (none (car (eas-component-test--line node 80 1 (plist-put (copy-sequence eas-component-test--datum)
                                                                   :volume 0)))))
    (should (equal (substring-no-properties up) "close=102.5  open=101.25"))
    (should (member '(:foreground "green" :weight bold) (eas-component-test--faces up)))
    (should (member '(:inherit shadow) (eas-component-test--faces up)))
    (should (member '(:foreground "red" :slant italic) (eas-component-test--faces down)))
    (should-not (member '(:foreground "green" :weight bold) (eas-component-test--faces down)))
    (should (equal (substring-no-properties none) "open=101.25"))))

(ert-deftest eas-component-when-else-styles-or-omits ()
  (let ((node '(:component "row"
                :children [(:component "when" :props (:test "datum.close >= datum.open")
                            :children [(:component "badge" :props (:text "UP" :background "green"))]
                            :else [(:component "badge" :props (:text "DOWN" :background "red"))])
                           (:component "when" :props (:test "datum.volume > 1e9")
                            :children ["huge"])
                           (:component "when" :props (:test "datum.pct > 0" :style (:color "blue"))
                            :children [(:field "pct" :format (:type "percent" :decimals 1))])]))
        (down (plist-put (copy-sequence eas-component-test--datum) :close 1)))
    (should (equal (substring-no-properties (car (eas-component-test--line node 80))) " UP   pct=1.2%"))
    (should (equal (substring-no-properties (car (eas-component-test--line node 80 1 down))) " DOWN   pct=1.2%"))
    (should (member '(:foreground "blue") (eas-component-test--faces (car (eas-component-test--line node 80)))))
    ;; With a style and the test false, the children show unstyled.
    (let ((neg (plist-put (copy-sequence eas-component-test--datum) :pct -0.5)))
      (should-not (member '(:foreground "blue")
                          (eas-component-test--faces (car (eas-component-test--line node 80 1 neg))))))))

(ert-deftest eas-component-props-take-expressions-and-templates ()
  (should (equal (eas-component-test--line
                  '(:component "row" :children
                    [(:component "text" :props (:template "{date} O"))
                     (:component "field" :props (:label "range" :value (:expr "datum.high - datum.low")
                                                 :format (:type "number" :decimals 1)))
                     (:component "sparkline" :props (:values (:expr "[datum.low, datum.open, datum.close, datum.high]")))])
                  80)
                 '("2026-03-11 O  range=3.8  ▁▄▆█"))))

(ert-deftest eas-component-nesting-and-the-declarative-form ()
  ;; {"fields": [...], "separator": ...} is a row of field nodes; a
  ;; string is a text node; rows nest.
  (should (equal (eas-component-test--line
                  '(:fields [(:field "open" :label "O" :labelSep " ")
                             (:component "row" :props (:sep "/")
                              :children [(:field "high" :label "H" :labelSep " ")
                                         (:field "low" :label "L" :labelSep " ")])
                             "end"]
                    :separator " | ")
                  80)
                 '("O 101.25 | H 103.5/L 99.75 | end"))))

(ert-deftest eas-component-define-component-with-defaults ()
  (eas-define-component
   "test-change"
   :doc "Close minus open, colored by sign."
   :props '((up :type color :default "green") (down :type color :default "red")
            (decimals :type integer :default 2))
   :render (lambda (props ctx)
             (let* ((d (plist-get ctx :datum)) (x (- (plist-get d :close) (plist-get d :open))))
               (list (eas-component-atom
                      (list (eas-component-span (format (format "%%+.%df" (plist-get props :decimals)) x)
                                                (list :color (plist-get props (if (>= x 0) :up :down)))))
                      :priority 70)))))
  (unwind-protect
      (let ((line (car (eas-component-test--line '(:component "test-change" :props (:up "cyan")) 40))))
        (should (equal (substring-no-properties line) "+1.25"))
        (should (member '(:foreground "cyan") (eas-component-test--faces line)))
        (should (member "test-change" (eas-component-names))))
    (remhash "test-change" eas-component--registry)))

(ert-deftest eas-component-validation-errors-name-the-path ()
  (let ((bad (lambda (node) (eas-test-should-code "INVALID_INPUT" (eas-component-validate node "/x-eas/readout")))))
    (should (equal (plist-get (funcall bad '(:component "nope")) :path) "/x-eas/readout/component"))
    (should (string-match-p "components: badge, field" (plist-get (funcall bad '(:component "nope")) :message)))
    (let ((e (funcall bad '(:component "row" :children [(:field "a") (:component "field" :props (:labl "x"))]))))
      (should (equal (plist-get e :path) "/x-eas/readout/children/1/props/labl"))
      (should (string-match-p "has no prop labl; props: field, value, label" (plist-get e :message))))
    (should (equal (plist-get (funcall bad '(:component "row" :children [(:field "a" :priority "high")])) :path)
                   "/x-eas/readout/children/0/props/priority"))
    (should (equal (plist-get (funcall bad '(:component "when" :children ["x"])) :path)
                   "/x-eas/readout/props/test"))
    (should (equal (plist-get (funcall bad '(:component "row" :else ["x"] :children [(:field "a" :style (:colour "red"))]))
                              :path)
                   "/x-eas/readout/children/0/props/style"))
    (should (equal (plist-get (funcall bad '(:component "text" :children ["x"])) :path)
                   "/x-eas/readout/children"))
    (should (equal (plist-get (funcall bad '(:component "fields" :props (:source "both"))) :path)
                   "/x-eas/readout/props/source"))
    (should (equal (plist-get (funcall bad '(:component "field" :props (:value (:expr "datum.(")))) :path)
                   "/x-eas/readout/props/value"))
    (should (eq t (eas-component-validate '(:component "row" :children [(:field "a" :rules [(:test "datum.a>1" :bold t)])]))))
    ;; Rendering checks evaluated values too.
    (should (equal (plist-get (eas-test-should-code "INVALID_INPUT"
                                (eas-component-test--render '(:component "badge" :props (:text "x" :color (:expr "datum.open")))))
                              :path)
                   "/x-eas/readout/props/color"))
    (should-error (eas-define-component "bad name" :render #'ignore))
    (should-error (eas-define-component "x" :render #'ignore :props '((a :type colour))))))

;;; Backends

(ert-deftest eas-component-svg-and-text-backends-agree ()
  (let* ((node '(:component "row" :children [(:field "close" :style (:color "green" :bold t)) "a<b"]))
         (spans (car (eas-component-fit (eas-component-test--render node) 80))))
    (should (equal (substring-no-properties (eas-component-propertize spans)) "close=102.5  a<b"))
    (should (equal (eas-component-svg-tspans spans)
                   (concat "<tspan fill=\"green\" font-weight=\"bold\">close</tspan>"
                           "<tspan fill=\"green\" font-weight=\"bold\">=</tspan>"
                           "<tspan fill=\"green\" font-weight=\"bold\">102.5</tspan>"
                           "<tspan>  </tspan><tspan>a&lt;b</tspan>")))
    ;; Propertized text round-trips into spans.
    (should (equal (eas-component-from-string (concat "a" (propertize "b" 'face '(:foreground "red" :weight bold))))
                   '(("a") ("b" :color "red" :bold t))))))

(provide 'eas-component-test)
;;; eas-component-test.el ends here
