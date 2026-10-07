;;; eas-demo.el --- shared helpers for the examples/eas-demo-*.el demos -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each examples/eas-demo-NAME.el shows one interaction of the design
;; table (docs/design/engine.md, section 4) on two charts: a
;; financial-style one (prices, returns, tickers) and a non-financial
;; one (a health or KPI series).  A demo is a list of cases
;;
;;   (:title T :source TEMPLATE-OR-SPEC :bindings B :setup FN :steps STEPS)
;;
;; (:open FN opens it instead of `eas-view-open', with the same
;; arguments; STEPS may be a function of the opened view)
;; where STEPS use the scripts/eas-animate.el step language (event/v1
;; events, "glide", "hold", "visit") plus (:call FN), FN called with
;; the view (it may return another view to follow, as drill does).
;;
;; Three ways to run one, from the repository root:
;;
;;   emacs -Q --batch -L src -l examples/eas-demo-brush.el
;;       headless: prints the text chart, each event and what the
;;       view's inspect / selection answer after it
;;   emacs -Q -L src -l examples/eas-demo-brush.el
;;       interactive: opens both charts in eas-mode buffers (SVG in a
;;       GUI frame, text in a terminal); use the mouse or keys
;;   EAS_DEMO_RECORD=1 emacs -Q --batch -L src -l examples/eas-demo-brush.el
;;       records docs/screenshots/demos/brush.gif (SVG frames) and
;;       brush-text.gif (text frames) with scripts/eas-animate.el
;;
;; The data is generated here, deterministically, so every run draws
;; the same frames.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'eas)

(defconst eas-demo-root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The repository root.")

(add-to-list 'load-path (expand-file-name "scripts" eas-demo-root))
(require 'eas-animate)

(defvar eas-demo-svg-size '(640 . 300) "Pixel size of the SVG views.")
(defvar eas-demo-text-size '(:cols 88 :rows 22) "Cell size of the text views.")

;;; Data

(defun eas-demo--date (i)
  "ISO date of trading day I (weekdays from 2026-01-05)."
  (let ((d (+ (* 7 (/ i 5)) (% i 5))))
    (format-time-string "%Y-%m-%d" (time-add (encode-time (list 0 0 12 5 1 2026 nil nil t))
                                             (* d 86400))
                        t)))

(defun eas-demo--day (i)
  "ISO date of calendar day I (from 2026-01-01)."
  (format-time-string "%Y-%m-%d" (time-add (encode-time (list 0 0 12 1 1 2026 nil nil t)) (* i 86400)) t))

(defun eas-demo--noise (seed i)
  "A deterministic pseudo-random number in [-1, 1) for SEED and I."
  (let ((x (% (+ (* (+ seed 7) 1103515245) (* (1+ i) 12345) (* i i 2654435)) 2147483647)))
    (- (* 2.0 (/ (% (abs x) 10007) 10007.0)) 1)))

(defun eas-demo-prices (&optional n symbol base)
  "N daily closes (default 60) of SYMBOL (\"ACME\") from BASE (100)."
  (let ((p (or base 100.0)) (seed (string-to-char (or symbol "A"))))
    (vconcat
     (cl-loop for i below (or n 60)
              do (setq p (* p (+ 1.002 (* 0.018 (eas-demo--noise seed i)))))
              collect (list :date (eas-demo--date i) :symbol (or symbol "ACME")
                            :close (/ (fround (* p 100)) 100.0)
                            :volume (+ 900 (round (* 400 (abs (eas-demo--noise (1+ seed) i))))))))))

(defun eas-demo-tickers ()
  "Daily closes of three tickers, long format."
  (vconcat (eas-demo-prices 40 "ACME" 100) (eas-demo-prices 40 "GLOBX" 80) (eas-demo-prices 40 "INIT" 120)))

(defun eas-demo-heart (&optional n)
  "N days (default 60) of resting heart rate, steps and sleep."
  (vconcat
   (cl-loop for i below (or n 60)
            collect (list :date (eas-demo--day i)
                          :bpm (+ 58 (round (* 3 (sin (* i 0.21)))) (round (* 2 (eas-demo--noise 5 i))))
                          :steps (+ 6000 (* 250 (% (* i 13) 17)) (if (memq (% i 7) '(5 6)) 2500 0))
                          :sleep (/ (fround (* 10 (+ 7 (* 0.8 (sin (* i 0.7)))))) 10.0)))))

(defun eas-demo-kpis ()
  "Weekly KPIs (signups, active users, tickets) for 12 weeks, long format."
  (vconcat
   (cl-loop for kpi in '("signups" "active" "tickets") for k from 1
            append (cl-loop for w from 1 to 12
                            collect (list :week w :kpi kpi
                                          :value (round (* (pcase kpi ("signups" 120) ("active" 300) (_ 60))
                                                           (+ 1 (* 0.03 w) (* 0.08 (sin (* w k)))))))))))

(defun eas-demo-weekly-steps ()
  "Average steps per weekday."
  (vconcat (cl-loop for d in '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") for i from 0
                    collect (list :category d :value (+ 7000 (* 380 (% (* (1+ i) 5) 7)))))))

(defun eas-demo-sector-returns ()
  "Year-to-date return per sector, in percent."
  (vconcat (cl-loop for s in '("Tech" "Energy" "Health" "Finance" "Utilities" "Materials")
                    for r in '(14.2 -3.5 6.1 9.8 2.4 -1.2)
                    collect (list :category s :value r))))

;;; Specs

(defun eas-demo-xy (rows x y &optional series)
  "ROWS reshaped to the x/y(/series) columns the series templates read.
X, Y and SERIES are the keywords of ROWS' columns."
  (vconcat (mapcar (lambda (r) (append (list :x (plist-get r x) :y (plist-get r y))
                                       (and series (list :series (plist-get r series)))))
                   rows)))

(defun eas-demo-spec (template bindings &rest patch)
  "TEMPLATE resolved with BINDINGS, then PATCH (KEY VALUE ...) applied.
Each KEY is a top-level Vega-Lite key; a function VALUE is called with
the old value."
  (let ((spec (copy-sequence (eas-resolve template bindings))))
    (cl-loop for (k v) on patch by #'cddr
             do (setq spec (plist-put spec k (if (functionp v) (funcall v (plist-get spec k)) v))))
    spec))

;;; Steps

(defun eas-demo-legend-px (view label)
  "Pixel centre of VIEW's legend entry LABEL."
  (cl-loop for v across (plist-get (eas-view-scene view) :views)
           thereis (cl-loop for legend across (plist-get v :legends)
                            thereis (cl-loop for e across (plist-get legend :entries)
                                             when (equal (plist-get e :label) label)
                                             return (let ((b (plist-get e :bounds)))
                                                      (vector (+ (aref b 0) (/ (aref b 2) 2.0))
                                                              (+ (aref b 1) (/ (aref b 3) 2.0))))))))

(defun eas-demo-item-px (view mark key value)
  "Pixel [X Y] of the first datum of MARK (a type) in VIEW whose KEY is VALUE.
Bar items answer their centre; line and area items the vertex."
  (let ((k (intern (concat ":" key))))
    (cl-loop for v across (plist-get (eas-view-scene view) :views)
             thereis
             (cl-loop for m across (plist-get v :marks)
                      for rows = (plist-get m :rows)
                      when (equal (plist-get m :mark) mark)
                      thereis
                      (cl-loop for item across (plist-get m :items)
                               for d = (plist-get item :datum)
                               thereis
                               (if (vectorp d)
                                   (cl-loop for j across d for i from 0
                                            when (equal (plist-get (aref rows j) k) value)
                                            return (aref (plist-get item :points) i))
                                 (and (equal (plist-get (aref rows d) k) value)
                                      (vector (+ (plist-get item :x) (/ (or (plist-get item :w) 0) 2.0))
                                              (+ (plist-get item :y) (/ (or (plist-get item :h) 0) 2.0))))))))))

(defun eas-demo-click-steps (px &optional hold)
  "Steps gliding to PX, clicking there and holding HOLD frames (12)."
  (list (list :glide px :frames 8) '(:hold 3)
        (list :type "click" :px px) (list :hold (or hold 12))))

(defun eas-demo-plot (view)
  "Bounds [X Y W H] of VIEW's first plot."
  (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :bounds))

(defun eas-demo-at (view fx fy)
  "Pixel [X Y] at fractions FX, FY across VIEW's first plot."
  (let ((b (eas-demo-plot view)))
    (vector (+ (aref b 0) (* fx (aref b 2))) (+ (aref b 1) (* fy (aref b 3))))))

;;; Running

(defun eas-demo--open (case target)
  "Open CASE as a view drawn for TARGET (`svg' or `text')."
  (let ((view (funcall (or (plist-get case :open) #'eas-view-open) (plist-get case :source)
                             :bindings (plist-get case :bindings)
                             :id (plist-get case :id)
                             :target target
                             :size (if (eq target 'text) eas-demo-text-size eas-demo-svg-size))))
    (when-let* ((setup (plist-get case :setup))) (funcall setup view))
    view))

(defun eas-demo--summary (view)
  "What an agent reads back from VIEW after an event."
  (let ((inspect (eas-inspect view)))
    (cl-loop for k in '(:hover :click :params :views :strip)
             for v = (plist-get inspect k)
             when v append (list k v))))

(defun eas-demo--steps (case view)
  "CASE's steps for VIEW: a list, or a function of the view."
  (let ((steps (plist-get case :steps)))
    (if (functionp steps) (funcall steps view) steps)))

(defun eas-demo--batch (cases)
  "Print CASES run headless on the text target."
  (dolist (case cases)
    (let ((view (eas-demo--open case 'text)))
      (princ (format "=== %s (%s) ===\n%s\n\n" (plist-get case :title) (eas-view-id view)
                     (eas-text-render (eas-view-scene view))))
      (let ((frames (eas-animate-plan (eas-view-scene view) (eas-demo--steps case view))))
        (while frames
          (let* ((f (pop frames))
                 (ev (or (plist-get f :event)
                         (and (plist-get f :move) (list :type "pointermove" :px (plist-get f :pointer)))))
                 (fn (plist-get f :call))
                 (next (cond (ev (eas-dispatch view ev) nil) (fn (funcall fn view)))))
            (when (eas-view-p next) (setq view next))
            ;; A run of moves prints once, where it comes to rest.
            (when (and (or ev fn)
                       (not (and (equal (plist-get ev :type) "pointermove")
                                 (or (plist-get (car frames) :move)
                                     (equal (plist-get (plist-get (car frames) :event) :type) "pointermove")))))
              (princ (format "%s\n%s\n\n" (if ev (eas-json-encode ev) "(call)")
                             (eas-json-pretty (eas-demo--summary view))))))))
      (princ (format "--- after ---\n%s\n\n" (eas-text-render (eas-view-scene view)))))))

(defun eas-demo--record (name cases)
  "Record CASES, one after another, as NAME.gif and NAME-text.gif."
  (dolist (target '(svg text))
    (let ((frames nil) (eas-animate-text-font eas-animate-text-font))
      (dolist (case cases)
        (let* ((view (eas-demo--open case target))
               (dir (make-temp-file "eas-demo-" t)))
          (setq frames (append frames
                               (eas-animate-frames view (eas-animate-plan (eas-view-scene view)
                                                                          (eas-demo--steps case view))
                                                   dir :fps 10 :tip-delay 4)))))
      (let ((out (expand-file-name (format "docs/screenshots/demos/%s%s.gif" name
                                           (if (eq target 'text) "-text" ""))
                                   eas-demo-root)))
        (eas-animate-gif frames out (if (eq target 'text) 760 640))
        (message "wrote %s (%d bytes, %d frames)" out
                 (file-attribute-size (file-attributes out)) (length frames))))))

(defun eas-demo-run (name cases)
  "Run demo NAME over CASES: headless, recording, or in buffers."
  (cond ((and noninteractive (getenv "EAS_DEMO_RECORD")) (eas-demo--record name cases))
        (noninteractive (eas-demo--batch cases))
        (t (dolist (case cases)
             (eas-show (eas-demo--open case (if (display-graphic-p) 'svg 'text)))))))

(provide 'eas-demo)
;;; eas-demo.el ends here
