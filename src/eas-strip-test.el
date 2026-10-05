;;; eas-strip-test.el --- tests for the values strip and touch-only hover -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.34: a chart shows its values passively in a strip under the
;; plot (latest, or at the pointer's column) and marks interact only
;; when the pointer touches them.  Driven headlessly through
;; `eas-dispatch' and `eas-replay', and in text and svg buffers.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-mode)

(defconst eas-strip-test--sparse
  '(:data (:values [(:t 0 :p 0) (:t 10 :p 10)])
    :width 200 :height 100
    :mark "line"
    :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
               :tooltip [(:field "p" :type "quantitative")]))
  "Two far-apart points: the middle of the segment is far from both.")

(defconst eas-strip-test--two
  '(:data (:values [(:t 1 :p 3 :s "a") (:t 2 :p 5 :s "a") (:t 3 :p 4 :s "a")
                    (:t 1 :p 30 :s "b") (:t 2 :p 20 :s "b") (:t 3 :p 25 :s "b")])
    :width 200 :height 100
    :mark "line"
    :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
               :color (:field "s" :type "nominal")))
  "Two series told apart by color.")

(defconst eas-strip-test--bars
  '(:data (:values [(:k "a" :v 1) (:k "b" :v 3) (:k "c" :v 2)])
    :width 200 :height 100
    :params [(:name "over" :select (:type "point" :on "pointermove"))
             (:name "near" :select (:type "point" :on "pointermove" :nearest t))]
    :mark "bar"
    :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
  "Bars with a touch-only and a nearest pointermove selection.")

(defmacro eas-strip-test--with (var spec &rest body)
  "Open SPEC as VAR in a fresh registry; run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal)) (eas-action-inhibit t) (inhibit-message t))
     (let ((,var (eas-view-open ,spec :id "s")))
       ,@body)))

(defun eas-strip-test--px (view x y)
  "Scene pixel of data X, Y in VIEW's first scene view."
  (let ((scales (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :scales)))
    (vector (eas-scale-apply (plist-get scales :x) x) (eas-scale-apply (plist-get scales :y) y))))

(defun eas-strip-test--move (view px)
  "Send a pointermove at PX to VIEW; return the inspect."
  (eas-dispatch view (list :type "pointermove" :px px)))

(defun eas-strip-test--fields (inspect)
  "INSPECT's strip as an alist of title . value."
  (mapcar (lambda (f) (cons (plist-get f :title) (plist-get f :value)))
          (plist-get (plist-get inspect :strip) :fields)))

;;; Touch-only hover

(ert-deftest eas-strip-hover-needs-the-pointer-on-the-line ()
  (eas-strip-test--with v eas-strip-test--sparse
    ;; Mid-segment: far from either vertex, but on the drawn line.
    (let ((on (eas-strip-test--px v 5 5)) (a (eas-strip-test--px v 0 0)))
      (should (> (abs (- (aref on 0) (aref a 0))) 40))
      (let ((hover (plist-get (eas-strip-test--move v on) :hover)))
        (should (equal (plist-get hover :mark) "main/0"))
        (should (equal (plist-get hover :tooltip) [(:title "p" :value "10")])))
      ;; 20 px above the line: the old 30 px radius hovered here.
      (should (eq (plist-get (eas-strip-test--move v (vector (aref on 0) (- (aref on 1) 20))) :hover)
                  :null)))))

(ert-deftest eas-strip-hover-on-bars-and-touch-only-selections ()
  (eas-strip-test--with v eas-strip-test--bars
    (let* ((scene (eas-view-scene v))
           (bar (aref (plist-get (eas-scene-mark scene "main/0") :items) 0))
           (inside (vector (+ (plist-get bar :x) (/ (plist-get bar :w) 2.0)) (+ (plist-get bar :y) 2)))
           (above (vector (aref inside 0) (- (plist-get bar :y) 15))))
      (let ((i (eas-strip-test--move v inside)))
        (should (equal (plist-get (plist-get i :hover) :datum) 0))
        (should (plist-get (plist-get (eas-view-state v) :params) :over)))
      (let ((i (eas-strip-test--move v above)))
        (should (eq (plist-get i :hover) :null))
        ;; Touch-only selection empties; `nearest' still follows the pointer.
        (should-not (plist-get (plist-get (eas-view-state v) :params) :over))
        (should (plist-get (plist-get (eas-view-state v) :params) :near))
        ;; The strip reads the bar under the column without a hover.
        (should (equal (eas-strip-test--fields i) '(("k" . "a") ("v" . "1"))))))))

(ert-deftest eas-strip-text-scenes-touch-within-a-cell ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let* ((v (eas-view-open eas-strip-test--sparse :id "t" :target 'text :size '(:cols 60 :rows 12)))
           (cell (plist-get (plist-get (eas-view-scene v) :size) :cell))
           (on (eas-strip-test--px v 5 5)))
      ;; A cell centre a third of a cell off the line still touches it.
      (should (equal (plist-get (plist-get (eas-strip-test--move v (vector (aref on 0) (+ (aref on 1) (/ (aref cell 1) 3.0))))
                                           :hover)
                                :mark)
                     "main/0"))
      ;; Two cells off it does not.
      (should (eq (plist-get (eas-strip-test--move v (vector (aref on 0) (- (aref on 1) (* 2 (aref cell 1))))) :hover)
                  :null)))))

;;; The strip

(ert-deftest eas-strip-shows-the-latest-values-untouched ()
  (eas-strip-test--with v eas-strip-test--two
    (let ((i (eas-inspect v)))
      (should (equal (plist-get (plist-get i :strip) :at) "latest"))
      (should (equal (eas-strip-test--fields i) '(("t" . "3") ("a" . "4") ("b" . "25"))))
      (should (equal (eas-strip-format (plist-get i :strip)) "latest  t=3  a=4  b=25")))))

(ert-deftest eas-strip-follows-the-pointer-column-and-leaves ()
  (eas-strip-test--with v eas-strip-test--two
    ;; Top of the plot at t=2: touches no line, still reads the column.
    (let* ((px (eas-strip-test--px v 2 0))
           (top (vector (aref px 0) (+ 1 (aref (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds) 1))))
           (i (eas-strip-test--move v top)))
      (should (eq (plist-get i :hover) :null))
      (should (equal (plist-get (plist-get i :strip) :at) "cursor"))
      (should (equal (eas-strip-test--fields i) '(("t" . "2") ("a" . "5") ("b" . "20")))))
    (let ((i (eas-dispatch v '(:type "pointerleave"))))
      (should (equal (plist-get (plist-get i :strip) :at) "latest")))
    ;; The strip is view state: a replay of the log reads the same.
    (eas-strip-test--move v (eas-strip-test--px v 1.1 0))
    (let ((want (plist-get (eas-inspect v) :strip)) (log (eas-view-log-entries v)))
      (eas-strip-test--with w eas-strip-test--two
        (eas-replay w log)
        (should (equal (plist-get (eas-inspect w) :strip) want))
        (should (equal (eas-strip-test--fields (eas-inspect w)) '(("t" . "1") ("a" . "3") ("b" . "30"))))))))

;;; Buffers

(ert-deftest eas-strip-text-buffer-ends-with-the-strip-and-follows-point ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t))
    (let* ((v (eas-view-open eas-strip-test--two :id "tty"))
           (buffer (eas-show v 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
              (should beg)
              (should (equal (buffer-substring-no-properties beg (point-max)) " latest  t=3  a=4  b=25"))
              ;; No hover and no event yet: no ":null" in the header.
              (should-not (string-match-p "null" (format "%s" header-line-format))))
            (goto-char (text-property-any (point-min) (point-max) 'eas-datum 0))
            (eas-mode--post-command)
            (let ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
              (should (string-prefix-p " cursor  t=1" (buffer-substring-no-properties beg (point-max))))))
        (kill-buffer buffer)))))

(ert-deftest eas-strip-svg-buffer-has-the-strip-under-the-image ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t))
    (let* ((v (eas-view-open eas-strip-test--two :id "gui"))
           (buffer (eas-show v 'svg)))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (car-safe (get-text-property (point-min) 'display)) 'image))
            (let ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
              (should (equal (buffer-substring-no-properties beg (point-max)) " latest  t=3  a=4  b=25"))
              (should (keymapp (get-text-property beg 'keymap))))
            (eas-dispatch v (list :type "pointermove" :px (eas-strip-test--px v 2 0)))
            (eas-mode--readout)
            (should (string-match-p "cursor  t=2  a=5  b=20" (buffer-string)))
            (should-not (string-match-p "null" (format "%s" header-line-format))))
        (kill-buffer buffer)))))

(provide 'eas-strip-test)
;;; eas-strip-test.el ends here
