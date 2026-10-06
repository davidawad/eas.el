;;; eas-fit-test.el --- text charts never wider than their window (fc-qx1.52) -*- lexical-binding: t; -*-

;;; Commentary:

;; Seen in a 189x56 terminal: renders a column or more wider than their
;; window ended in `$' truncation glyphs, cutting legend labels and the
;; last x tick label.  Each test pins one part of the fix.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-mode)
(require 'eas-text-check)
(require 'eas-text-gallery)
(require 'eas-vl-gallery-custom)

(defun eas-fit-test--width (text)
  "Widest line of TEXT, in columns."
  (apply #'max (mapcar #'string-width (split-string text "\n"))))

(defun eas-fit-test--scene (group name size)
  "Gallery example NAME of GROUP compiled for text at SIZE."
  (eas-vl-gallery--native
   (eas-compile (eas-text-gallery-spec group name) :target 'text :size size)))

(ert-deftest eas-fit-text-window-size-leaves-the-truncation-column ()
  ;; The chart is as wide as the window shows a line whole: in a
  ;; terminal the body less the column `$' takes, less line numbers.
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (let ((size (eas-mode--window-size (selected-window) 'text)))
      (should (= (plist-get size :cols) (max 20 (window-max-chars-per-line))))
      (should (< (plist-get size :cols) (window-body-width))))
    (cl-letf (((symbol-function 'window-max-chars-per-line) (lambda (&rest _) 90)))
      (should (= (plist-get (eas-mode--window-size (selected-window) 'text) :cols) 90)))))

(ert-deftest eas-fit-view-mode-follows-window-and-hides-line-numbers ()
  (with-temp-buffer
    (setq-local display-line-numbers t)
    (eas-view-mode)
    (should-not display-line-numbers)
    (should (memq #'eas-mode--follow-window window-size-change-functions))
    ;; A window switched to the buffer (C-x b) refits it too.
    (should (memq #'eas-mode--follow-window window-buffer-change-functions))))

(ert-deftest eas-fit-text-check-reports-wide-lines ()
  (let ((scene (eas-fit-test--scene "bar" "bar_grouped" '(:cols 60 :rows 16))))
    (should-not (seq-filter (lambda (p) (string-prefix-p "width:" p)) (eas-text-check scene 60)))
    (should (seq-find (lambda (p) (string-match-p "^width: line [0-9]+ is [0-9]+ columns, over the 20" p))
                      (eas-text-check scene 20)))))

(ert-deftest eas-fit-compositions-fit-narrow-text ()
  ;; These grew their canvas past 60 columns to keep each cell's labels.
  (dolist (name '("concat_marginal_histograms" "nested_concat_align" "trellis_scatter"))
    (let ((scene (eas-fit-test--scene "multiview" name '(:cols 60 :rows 16))))
      (should (= (plist-get (plist-get scene :size) :w) (* 60 (aref (plist-get (plist-get scene :size) :cell) 0))))
      (should-not (plist-get (plist-get scene :size) :cut))
      (should (<= (eas-fit-test--width (eas-text-render scene)) 60)))))

(ert-deftest eas-fit-cut-canvas-is-recorded-and-legends-ellipsize ()
  (let* ((spec (eas-vl-gallery-custom-spec "multiview" "scales_discretize_custom"))
         (narrow (eas-vl-gallery--native (eas-compile spec :target 'text :size '(:cols 50 :rows 14))))
         (fits (eas-vl-gallery--native (eas-compile spec :target 'text :size '(:cols 60 :rows 16))))
         (labels (lambda (scene) (cl-loop for v across (plist-get scene :views)
                                          append (cl-loop for l across (plist-get v :legends)
                                                          append (mapcar (lambda (e) (plist-get e :label))
                                                                         (plist-get l :entries)))))))
    ;; Too wide even squeezed: cut at the window, the need recorded.
    (should (> (plist-get (plist-get narrow :size) :cut) (* 50 7)))
    (should (<= (eas-fit-test--width (eas-text-render narrow)) 50))
    (should-not (eas-vl-gallery-overlaps narrow))
    ;; A label running past the edge ends in an ellipsis over no swatch.
    (should (member "40 …" (funcall labels fits)))
    (should (<= (eas-fit-test--width (eas-text-render fits)) 60))))

(ert-deftest eas-fit-strip-no-wider-than-the-chart ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t))
    (let ((view (eas-view-open "ohlc" :bindings (eas-template-example "ohlc") :target 'text
                               :size '(:cols 20 :rows 10))))
      (should (<= (string-width (eas-mode-strip-string view)) 20)))))

(provide 'eas-fit-test)
;;; eas-fit-test.el ends here
