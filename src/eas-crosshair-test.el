;;; eas-crosshair-test.el --- tests for the crosshair and its readout -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.2: a point selection {on: pointermove, nearest: true} plus a
;; rule layer filtered by it, driven headlessly through `eas-dispatch'
;; and `eas-replay', in a text buffer through point motion.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defmacro eas-crosshair-test--with-view (var source &rest body)
  "Open SOURCE (bindings for the line template, or a spec) as VAR; run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal)))
     (let ((,var (if (plist-get ,source :data)
                     (if (vectorp (plist-get ,source :data))
                         (eas-view-open "line" :bindings ,source :id "c")
                       (eas-view-open ,source :id "c"))
                   (error "Bad source"))))
       ,@body)))

(defun eas-crosshair-test--bindings ()
  "The line template's example bindings with the crosshair on."
  (plist-put (copy-sequence (eas-template-example "line")) :crosshair t))

(defun eas-crosshair-test--view (view)
  "VIEW's first scene view."
  (aref (plist-get (eas-view-scene view) :views) 0))

(defun eas-crosshair-test--mark (view id)
  "Mark ID (\"crosshair-rule\", \"crosshair-point\") of VIEW's scene."
  (eas-scene-mark (eas-view-scene view) id))

(defun eas-crosshair-test--x (view value)
  "Scene x pixel of data VALUE in VIEW."
  (eas-scale-apply (plist-get (plist-get (eas-crosshair-test--view view) :scales) :x) value))

(defun eas-crosshair-test--top (view)
  "A y pixel just inside the top of VIEW's plot, far from most data."
  (+ 2 (aref (plist-get (eas-crosshair-test--view view) :bounds) 1)))

(defconst eas-crosshair-test--sibling-spec
  '(:data (:values [(:t 1 :p 3) (:t 2 :p 5) (:t 3 :p 4) (:t 4 :p 8)])
    :width 200 :height 100
    :layer [(:params [(:name "h" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "point"
             :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                        :opacity (:condition (:param "h" :empty :false :value 1) :value 0)))
            (:mark "line"
             :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                        :tooltip [(:field "p" :type "quantitative" :title "Price") (:field "t" :type "quantitative")]))
            (:transform [(:filter (:param "h" :empty :false))]
             :mark "rule" :encoding (:x (:field "t" :type "quantitative")))])
  "The param on an invisible point layer; only the line has a tooltip.")

;;; Template and reducer

(ert-deftest eas-crosshair-template-slot-is-opt-in ()
  (let ((off (eas-resolve "line" (eas-template-example "line")))
        (on (eas-resolve "line" (eas-crosshair-test--bindings))))
    (should (= (length (plist-get off :layer)) 1))
    (should (equal (seq-map (lambda (l) (plist-get l :name)) (plist-get on :layer))
                   '(nil "crosshair-hit" "crosshair-rule" "crosshair-point")))
    ;; The param lives on invisible rules: Vega-Lite has no `nearest' for
    ;; line marks, and rules hit-test by bisect.
    (should (equal (plist-get (aref (plist-get (aref (plist-get on :layer) 1) :params) 0) :select)
                   '(:type "point" :on "pointermove" :nearest t :encodings ["x"])))
    ;; The static path (bin/chart) draws the initial state: no crosshair.
    (dolist (i '(2 3))
      (should (equal (plist-get (aref (plist-get on :layer) i) :transform)
                     [(:filter (:param "crosshair" :empty :false))])))))

(ert-deftest eas-crosshair-snaps-to-the-nearest-x-anywhere-in-the-plot ()
  (eas-crosshair-test--with-view v (eas-crosshair-test--bindings)
    (let ((rows (plist-get (eas-crosshair-test--bindings) :data)))
      (seq-do-indexed
       (lambda (row k)
         ;; Pointer at the top of the plot, a few pixels inward of datum K:
         ;; nearest is horizontal, so the y distance does not matter.
         (let* ((x (eas-crosshair-test--x v (plist-get row :date)))
                (inspect (eas-dispatch v (list :type "pointermove"
                                                 :px (vector (+ x (if (zerop k) 3 -3)) (eas-crosshair-test--top v)))))
                (rule (plist-get (eas-crosshair-test--mark v "crosshair-rule") :items)))
           (should (equal (plist-get (plist-get (plist-get inspect :hover) :row) :date) (plist-get row :date)))
           (should (equal (plist-get (seq-find (lambda (p) (equal (plist-get p :name) "crosshair"))
                                               (plist-get inspect :params))
                                     :summary)
                          "1 value of date"))
           (should (= (length rule) 1))
           (should (< (abs (- (plist-get (aref rule 0) :x1) x)) 0.5))
           (should (equal (seq-map (lambda (r) (plist-get r :date))
                                   (plist-get (eas-crosshair-test--mark v "crosshair-point") :rows))
                          (list (plist-get row :date))))))
       rows))))

(ert-deftest eas-crosshair-leave-and-escape-clear-it ()
  (eas-crosshair-test--with-view v (eas-crosshair-test--bindings)
    (dolist (clear '((:type "pointerleave") (:type "key" :key "escape")))
      (eas-dispatch v (list :type "pointermove" :px (vector (eas-crosshair-test--x v "2026-03-05") 100)))
      (should (= (length (plist-get (eas-crosshair-test--mark v "crosshair-rule") :items)) 1))
      (eas-dispatch v clear)
      (should (= (length (plist-get (eas-crosshair-test--mark v "crosshair-rule") :items)) 0))
      (should (eq (plist-get (eas-inspect v) :hover) :null))
      (should-not (eas-crosshair-view-readout v)))))

(ert-deftest eas-crosshair-replays-from-the-log ()
  (eas-crosshair-test--with-view v (eas-crosshair-test--bindings)
    (dolist (date '("2026-03-03" "2026-03-09" "2026-03-06"))
      (eas-dispatch v (list :type "pointermove" :px (vector (eas-crosshair-test--x v date) 60))))
    (let ((log (eas-view-log v)) (want (eas-inspect v))
          (rule (plist-get (eas-crosshair-test--mark v "crosshair-rule") :items)))
      (let ((w (eas-view-open "line" :bindings (eas-crosshair-test--bindings) :id "replay")))
        (eas-replay w log)
        (should (equal (plist-get (eas-inspect w) :hover) (plist-get want :hover)))
        (should (equal (plist-get (eas-crosshair-test--mark w "crosshair-rule") :items) rule))
        (should (equal (plist-get (plist-get (plist-get want :hover) :row) :date) "2026-03-06"))))))

(ert-deftest eas-crosshair-patched-scenes-equal-full-compiles ()
  (dolist (source (list (eas-crosshair-test--bindings) eas-crosshair-test--sibling-spec))
    (eas-crosshair-test--with-view v source
      (dolist (e '((:type "pointermove" :px [70 40]) (:type "pointermove" :px [190 90])
                   (:type "pointermove" :px [120 30]) (:type "pointerleave")))
        (eas-dispatch v e)
        (should (equal (eas-scene-to-json (eas-view-scene v))
                       (eas-scene-to-json
                        (eas-params-with-state (eas-view-state v)
                          (eas-compile (eas-view-spec v) :state (eas-view-state v))))))))))

(ert-deftest eas-crosshair-date-parse-cache-is-transparent ()
  (let ((eas-time--parse-cache (make-hash-table :test 'equal)))
    (dotimes (_ 2)
      (should (equal (eas-time-parse "2026-03-05") (eas-time--parse-string "2026-03-05")))
      (should (equal (eas-time-parse "2026-03-05T12:00:00+02:00") 1772704800000))
      (should-not (eas-time-parse "AAPL"))
      (should (= (eas-time-parse 42) 42)))
    (should (eq (gethash "AAPL" eas-time--parse-cache) :none))))

;;; Readout

(ert-deftest eas-crosshair-readout-lists-every-tooltip-field ()
  (eas-crosshair-test--with-view v (eas-crosshair-test--bindings)
    (eas-dispatch v (list :type "pointermove" :px (vector (eas-crosshair-test--x v "2026-03-05") 40)))
    (should (equal (eas-crosshair-view-readout v)
                   [(:title "date" :value "Mar 05, 2026") (:title "value" :value "106.3")]))
    (should (equal (eas-crosshair-format (eas-crosshair-view-readout v))
                   "date=Mar 05, 2026  value=106.3"))))

(ert-deftest eas-crosshair-readout-borrows-a-sibling-marks-tooltip ()
  (eas-crosshair-test--with-view v eas-crosshair-test--sibling-spec
    (let ((inspect (eas-dispatch v (list :type "pointermove" :px (vector (eas-crosshair-test--x v 3) (eas-crosshair-test--top v))))))
      ;; The point layer holding the param wins the hit and has no tooltip.
      (should (equal (plist-get (plist-get inspect :hover) :mark) "main/0"))
      (should (equal (eas-crosshair-view-readout v) [(:title "Price" :value "4") (:title "t" :value "3")])))))

(ert-deftest eas-crosshair-readout-falls-back-to-the-row ()
  (eas-crosshair-test--with-view v '(:data (:values [(:t 1 :p 3) (:t 2 :p 5)])
                                       :mark "line"
                                       :encoding (:x (:field "t" :type "quantitative")
                                                  :y (:field "p" :type "quantitative")))
    (eas-dispatch v (list :type "pointermove"
                            :px (vector (eas-crosshair-test--x v 2)
                                        (eas-scale-apply (plist-get (plist-get (eas-crosshair-test--view v) :scales) :y) 5))))
    (should (equal (eas-crosshair-view-readout v) [(:title "t" :value "2") (:title "p" :value "5")]))))

;;; Glue

(defun eas-crosshair-test--rule-column (view)
  "Text column of VIEW's crosshair rule, or nil."
  (when-let* ((items (plist-get (eas-crosshair-test--mark view "crosshair-rule") :items))
              ((> (length items) 0)))
    (floor (eas-text--inside (plist-get (eas-crosshair-test--view view) :bounds) (plist-get (aref items 0) :x1))
           (aref (plist-get (plist-get (eas-view-scene view) :size) :cell) 0))))

(ert-deftest eas-crosshair-point-motion-moves-the-column ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let* ((view (eas-view-open "line" :bindings (eas-crosshair-test--bindings) :id "tty"))
           (buffer (eas-show view 'text))
           (written 0))
      (unwind-protect
          (with-current-buffer buffer
            (add-hook 'after-change-functions (lambda (beg end _) (setq written (+ written (- end beg)))) nil t)
            (let ((first (text-property-any (point-min) (point-max) 'eas-datum 1))
                  (last (text-property-any (point-min) (point-max) 'eas-datum 7))
                  columns)
              (dolist (pos (list first last))
                (goto-char pos)
                (setq written 0)
                (eas-mode--post-command)
                (let ((col (eas-crosshair-test--rule-column view))
                      (tooltip (plist-get (plist-get (eas-inspect view) :hover) :tooltip)))
                  (push col columns)
                  ;; The buffer is exactly the new grid over the values strip,
                  ;; point did not move, and only the crosshair's cells (and
                  ;; the strip) were rewritten.
                  (should (equal-including-properties
                           (buffer-substring (point-min) (1- (text-property-any (point-min) (point-max) 'eas-strip t)))
                           (eas-text-render (eas-view-scene view))))
                  (should (= (point) pos))
                  ;; The default theme has no frame, so lines are right-trimmed
                  ;; and a rule past a line's end also writes the blank run to it
                  ;; (fc-qx1.21): still a fraction of the grid, never a rewrite.
                  (should (< 0 written (max (* 3 (count-lines (point-min) (point-max)))
                                            (/ (buffer-size) 5))))
                  (should (= (current-column) (save-excursion (goto-char pos) (current-column))))
                  (should (= (length tooltip) 2))
                  (should (string-suffix-p (eas-crosshair-format tooltip) (format "%s" header-line-format)))
                  ;; A rule glyph sits in the crosshair's column.
                  (should (save-excursion
                            (goto-char (point-min))
                            (cl-loop until (eobp)
                                     thereis (progn (move-to-column col)
                                                                    (and (= (current-column) col) (eq (char-after) ?│)))
                                     do (forward-line 1))))))
              (should (> (nth 0 columns) (nth 1 columns)))))
        (kill-buffer buffer)))))

(ert-deftest eas-crosshair-gui-readout-updates-before-the-redraw ()
  (let ((eas-views (make-hash-table :test 'equal)) (redraws 0))
    (let ((view (eas-view-open "line" :bindings (eas-crosshair-test--bindings) :id "gui" :target 'svg)))
      (with-temp-buffer
        (eas-view-mode)
        (setq eas-mode--view view)
        (setf (eas-view-buffer view) (current-buffer))
        (cl-letf (((symbol-function 'eas-mode-redraw) (lambda (&rest _) (setq redraws (1+ redraws)))))
          (let* ((x (round (eas-crosshair-test--x view "2026-03-10")))
                 (image '(image :type svg :data "" :scale 1))
                 (posn (list (selected-window) 1 (cons x 40) 0 nil 1 (cons x 40) image (cons x 40) '(400 . 300))))
            (eas-mode-pointer (list 'mouse-movement posn))
            (should (string-match-p "date=Mar 10, 2026  value=110.4" (format "%s" header-line-format)))
            (should (= redraws 1))))))))

(provide 'eas-crosshair-test)
;;; eas-crosshair-test.el ends here
