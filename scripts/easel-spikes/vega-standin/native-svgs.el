;;; native-svgs.el --- write easel's native SVG for each gallery spec -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -L src/easel -l native-svgs.el OUT-DIR
(require 'easel)
(setq easel-spec-supported-function nil)
(let ((out (car command-line-args-left)))
  (setq command-line-args-left nil)
  (dolist (f (directory-files easel-conformance-directory t "\\.vl\\.json\\'"))
    (with-temp-file (expand-file-name (concat (string-remove-suffix ".vl.json" (file-name-nondirectory f)) ".native.svg") out)
      (insert (easel-svg-render (easel-compile (easel-json-read-file f)))))))
