;;; tooltips-click.el --- fc-qx1.1 demo: tooltips and click targets, headless -*- lexical-binding: t; -*-

;; Run from the repository root:
;;   emacs -Q --batch -L src -l scripts/eas-demos/tooltips-click.el
;; It opens the bars template as a text view, hovers and
;; clicks through `eas-dispatch' exactly as the buffer glue does, and
;; prints the chart, each hover tooltip and each click record.

;;; Code:

(require 'eas)

(defun eas-demo--centre (view mark i)
  "Pixel centre of item I of MARK in VIEW."
  (let ((item (aref (plist-get (eas-scene-mark (eas-view-scene view) mark) :items) i)))
    (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))))

(defun eas-demo--show (label value)
  "Print LABEL and VALUE as JSON."
  (princ (format "%s\n%s\n\n" label (eas-json-pretty value))))

(let ((eas-action-browse-function (lambda (url) (princ (format "[browse-url %s]\n" url))))
      (inhibit-message t))
  (dolist (case '(("bars" "main/0" 1)))
    (let* ((view (eas-view-open (car case) :bindings (eas-template-example (car case)) :subject "demo"))
           (px nil))
      (eas-view-resize view '(:cols 64 :rows 18) 'text)
      (princ (format "=== %s (text target) ===\n%s\n" (eas-view-id view) (eas-text-render (eas-view-scene view))))
      (setq px (eas-demo--centre view (nth 1 case) (nth 2 case)))
      (eas-demo--show (format "pointermove %s -> inspect.hover" px)
                        (plist-get (eas-dispatch view (list :type "pointermove" :px px)) :hover))
      (unless (eas-action-for view (list :mark (nth 1 case) :view "main"))
        (eas-action-bind view "*" "copy-row"))
      (eas-demo--show (format "click (RET at point) %s -> inspect.click" px)
                        (plist-get (eas-dispatch view (list :type "click" :px px)) :click))
      (eas-demo--show "log" (eas-view-log-entries view)))))

;;; tooltips-click.el ends here
