;;; tty-parity.el --- drive a live easel chart in emacs -nw (fc-qx1.8) -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "../../src/easel" (file-name-directory load-file-name)))
(require 'easel)
(defvar e2e-out (getenv "E2E_OUT"))
(defconst e2e-spec
  '(:data (:values [(:x 1 :y 3 :c "u") (:x 2 :y 5 :c "v") (:x 3 :y 4 :c "u") (:x 4 :y 8 :c "v") (:x 5 :y 6 :c "u")])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))
             (:name "pick" :select "point")
             (:name "legend" :select (:type "point" :fields ["c"]) :bind "legend")]
    :mark "point"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
               :color (:field "c" :type "nominal"))))
(setq easel-brush-functions nil)
(defvar e2e-view (easel-view-open e2e-spec :id "e2e"))
(easel-show e2e-view)
(redisplay t)
(defun e2e-screen (datum)
  "1-based SGR col;row of DATUM's cell."
  (with-current-buffer (easel-view-buffer e2e-view)
    (let* ((pos (text-property-any (point-min) (point-max) 'easel-datum datum))
           (cr (posn-col-row (posn-at-point pos (get-buffer-window))))
           (edges (window-inside-edges (get-buffer-window))))
      (format "%d;%d" (+ (nth 0 edges) (car cr) 1) (+ (nth 1 edges) (cdr cr) 1)))))
(with-temp-file (concat e2e-out ".cells")
  (insert (format "%s %s %s\n" (e2e-screen 1) (e2e-screen 4) (e2e-screen 2))))
(defun e2e-dump ()
  (interactive)
  (with-temp-file e2e-out
    (insert (format "target=%s xterm-mouse-mode=%s TERM=%s\n" (easel-view-target e2e-view)
                    (bound-and-true-p xterm-mouse-mode) (getenv "TERM")))
    (dolist (e (reverse (easel-view-log e2e-view)))
      (insert (format "log: %s\n" (plist-get e :summary))))
    (insert (format "state: %S\n" (easel-parity-state e2e-view)))
    (let* ((gui (easel-view-open e2e-spec :id "gui"))
           (text (easel-view-open e2e-spec :id "text" :target 'text :size (easel-view-size e2e-view)))
           (r (easel-parity-replay (easel-view-log e2e-view) text gui)))
      (insert (format "parity tty-log on fresh text vs svg: ok=%s steps=%s mismatches=%S\n"
                      (plist-get r :ok) (plist-get r :steps) (plist-get r :mismatches)))
      (insert (format "fresh text == live tty: %S\n"
                      (null (easel-parity-diff (easel-parity-state text) (easel-parity-state e2e-view)))))))
  (kill-emacs 0))
(global-set-key [f5] #'e2e-dump)
(provide (quote tty-parity))
