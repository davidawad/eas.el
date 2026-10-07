;;; eas-scene-dirty.el --- which marks a frame changed -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4/L6 (eas-b2s.3).  The scene is retained across frames: a
;; mark whose unit kept its items, rows and hit index is the very plist
;; of the last frame (`eas-compile--view'), and a patch or push keeps
;; the items of every unit it did not touch.  `eas-scene-dirty' reads
;; that identity back as dirty marks, so a renderer may redraw only
;; what changed:
;;
;;   t                       redraw everything (sizes, views, chrome or
;;                           params moved, or there is no last frame)
;;   ((VIEW-ID MARK-ID ...)  per view, the marks that are new or changed;
;;    ...)                   views that changed nothing are left out
;;
;; Nil means nothing changed.  Comparing is by `eq' on marks (a mark
;; rebuilt with equal contents counts as changed: dirty marks may over-
;; but never under-report) and by `equal' on everything else, so it
;; costs a pointer test per mark plus the chrome, mostly shared.

;;; Code:

(require 'cl-lib)

(defconst eas-scene-dirty--chrome '(:id :path :bounds :header :clip :frame :scales :axes :legends :params)
  "The view keys other than the marks: a change there redraws the scene.")

(defun eas-scene-dirty--view (old new)
  "Compare view OLD with view NEW; return the changed mark ids, or t.
It is t when anything besides the marks changed."
  (if (not (cl-every (lambda (k) (equal (plist-get old k) (plist-get new k))) eas-scene-dirty--chrome))
      t
    (let ((a (plist-get old :marks)) (b (plist-get new :marks)))
      (if (/= (length a) (length b)) t
        (cl-loop for m across b for o across a
                 unless (eq m o) collect (plist-get m :id))))))

(defun eas-scene-dirty (old new)
  "Return what scene NEW changed since scene OLD.
That is nil, t or ((VIEW-ID MARK-ID ...) ...); see the commentary."
  (if (or (null old)
          (not (cl-every (lambda (k) (equal (plist-get old k) (plist-get new k)))
                         '(:contract :target :size :background :title :params)))
          (/= (length (plist-get old :views)) (length (plist-get new :views))))
      t
    (catch 'all
      (cl-loop for o across (plist-get old :views) for n across (plist-get new :views)
               for d = (eas-scene-dirty--view o n)
               do (when (eq d t) (throw 'all t))
               when d collect (cons (plist-get n :id) d)))))

(provide 'eas-scene-dirty)
;;; eas-scene-dirty.el ends here
