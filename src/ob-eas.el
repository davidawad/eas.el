;;; ob-eas.el --- org-babel support for eas charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The name org-babel looks for: (org-babel-do-load-languages
;; 'org-babel-load-languages '((eas . t))) loads this file, which
;; loads the whole surface (eas-babel.el and its inline view).

;;; Code:

(require 'eas-babel)
(require 'eas-babel-source)
(require 'eas-babel-inline)

(provide 'ob-eas)
;;; ob-eas.el ends here
