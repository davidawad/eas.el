;;; tooltips-click.el --- fc-qx1.1 demo: tooltips and click targets, headless -*- lexical-binding: t; -*-

;; Run from the repository root:
;;   emacs -Q --batch -L src/easel -l scripts/easel-demos/tooltips-click.el
;; It opens the ohlc and bars templates as text views, hovers and
;; clicks through `easel-dispatch' exactly as the buffer glue does, and
;; prints the chart, each hover tooltip and each click record.

;;; Code:

(require 'easel)

(defun easel-demo--centre (view mark i)
  "Pixel centre of item I of MARK in VIEW."
  (let ((item (aref (plist-get (easel-scene-mark (easel-view-scene view) mark) :items) i)))
    (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))))

(defun easel-demo--show (label value)
  "Print LABEL and VALUE as JSON."
  (princ (format "%s\n%s\n\n" label (easel-json-pretty value))))

(let ((easel-action-browse-function (lambda (url) (princ (format "[browse-url %s]\n" url))))
      (inhibit-message t))
  (dolist (case '(("ohlc" "candles" 3) ("bars" "main/0" 1)))
    (let* ((view (easel-view-open (car case) :bindings (easel-template-example (car case)) :subject "demo"))
           (px nil))
      (easel-view-resize view '(:cols 64 :rows 18) 'text)
      (princ (format "=== %s (text target) ===\n%s\n" (easel-view-id view) (easel-text-render (easel-view-scene view))))
      (setq px (easel-demo--centre view (nth 1 case) (nth 2 case)))
      (easel-demo--show (format "pointermove %s -> inspect.hover" px)
                        (plist-get (easel-dispatch view (list :type "pointermove" :px px)) :hover))
      (unless (easel-action-for view (list :mark (nth 1 case) :view "main"))
        (easel-action-bind view "*" "copy-row"))
      (easel-demo--show (format "click (RET at point) %s -> inspect.click" px)
                        (plist-get (easel-dispatch view (list :type "click" :px px)) :click))
      (easel-demo--show "log" (easel-view-log-entries view)))))

;;; tooltips-click.el ends here
