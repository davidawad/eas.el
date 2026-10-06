;;; native-svgs.el --- write eas's native SVG for each gallery spec -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -L src -l native-svgs.el OUT-DIR
(require 'eas)
(setq eas-spec-supported-function nil)
(let ((out (car command-line-args-left)))
  (setq command-line-args-left nil)
  (dolist (f (directory-files eas-conformance-directory t "\\.vl\\.json\\'"))
    (with-temp-file (expand-file-name (concat (string-remove-suffix ".vl.json" (file-name-nondirectory f)) ".native.svg") out)
      (insert (eas-svg-render (eas-compile (eas-json-read-file f)))))))
