;;; eas-compile-shared-pos.el --- positional scales shared across concat views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite resolves the x and y scales of concatenated
;; views independently unless resolve.scale makes them "shared"; then
;; every view drawing on the channel takes one scale over the union of
;; their domains (a mosaic's label row above its rects).  Views zoomed
;; in view state keep their own domain.

;;; Code:

(require 'eas-core)
(require 'eas-compile-scales)

(defun eas-shared-pos--channels (spec)
  "Positional channels SPEC's resolve.scale shares explicitly."
  (let ((scale (plist-get (plist-get spec :resolve) :scale)))
    (seq-filter (lambda (ch) (equal (plist-get scale ch) "shared")) '(:x :y))))

(defun eas-shared-pos--union (scales)
  "The domain covering every scale in SCALES (all of one kind), or nil."
  (if (member (plist-get (car scales) :type) '("band" "point"))
      (vconcat (sort (delete-dups (apply #'append (mapcar (lambda (s) (append (plist-get s :domain) nil)) scales)))
                     #'eas-compile--less))
    (let ((ds (mapcar (lambda (s) (plist-get s :domain)) scales)))
      (when (seq-every-p (lambda (d) (and (numberp (aref d 0)) (numberp (aref d 1)))) ds)
        (vector (apply #'min (mapcar (lambda (d) (min (aref d 0) (aref d 1))) ds))
                (apply #'max (mapcar (lambda (d) (max (aref d 0) (aref d 1))) ds)))))))

(defun eas-shared-pos-prepare (tree groups spec state)
  "Give GROUPS of concat TREE one scale per channel SPEC's resolve shares.
Groups zoomed in view STATE keep theirs."
  (when (and (plist-get tree :concat) (cdr groups))
    (dolist (ch (eas-shared-pos--channels spec))
      (let* ((free (seq-remove (lambda (g) (plist-get (plist-get (plist-get state :domains) (eas-key (plist-get g :id))) ch))
                               groups))
             (scales (delq nil (mapcar (lambda (g) (plist-get (plist-get g :scales) ch)) free)))
             (kinds (delete-dups (mapcar (lambda (s) (and (member (plist-get s :type) '("band" "point")) t)) scales)))
             (domain (and (cdr scales) (null (cdr kinds)) (eas-shared-pos--union scales))))
        (when domain
          (dolist (g free)
            (when-let* ((s (plist-get (plist-get g :scales) ch)))
              (plist-put g :scales (plist-put (plist-get g :scales) ch
                                              (plist-put (copy-sequence s) :domain domain))))))))))

(provide 'eas-compile-shared-pos)
;;; eas-compile-shared-pos.el ends here
