;;; eas-scene.el --- scene/v1 as data: JSON and lookups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The scene is the artifact every renderer, hit-test and agent query
;; reads.  `eas-scene-to-json' serializes it with floats rounded to
;; three decimals (sub-pixel noise would only churn goldens); the
;; lookups find views, marks and the data row behind any item.

;;; Code:

(require 'eas-core)

(defun eas-scene-round (value &optional digits)
  "Return VALUE with every float rounded to DIGITS decimals (default 3)."
  (let ((scale (expt 10.0 (or digits 3))))
    (cl-labels ((walk (v)
                  (cond ((floatp v) (let ((r (/ (fround (* v scale)) scale)))
                                      (if (zerop r) 0.0 r)))
                        ((vectorp v) (vconcat (mapcar #'walk v)))
                        ((and (consp v) (keywordp (car v)))
                         (cl-loop for (k x) on v by #'cddr append (list k (walk x))))
                        (t v))))
      (walk value))))

(defun eas-scene-to-json (scene &optional pretty)
  "Serialize SCENE to JSON; PRETTY gives the indented golden form."
  (let ((value (eas-scene-round scene)))
    (if pretty (eas-json-pretty value) (eas-json-encode value))))

(defun eas-scene-view (scene id)
  "The view of SCENE whose :id is ID, or nil."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get scene :views)))

(defun eas-scene-mark (scene id)
  "The mark of SCENE whose :id is ID, or nil."
  (seq-some (lambda (v) (seq-find (lambda (m) (equal (plist-get m :id) id)) (plist-get v :marks)))
            (plist-get scene :views)))

(defun eas-scene-row (scene mark-id datum)
  "The data row behind DATUM of mark MARK-ID in SCENE."
  (when-let* ((mark (eas-scene-mark scene mark-id)))
    (aref (plist-get mark :rows) datum)))

(defun eas-scene-summary (scene)
  "Return a small plist describing SCENE: size, views, mark and item counts."
  (list :size (plist-get scene :size)
        :views (vconcat
                (seq-map (lambda (v)
                           (list :id (plist-get v :id)
                                 :bounds (plist-get v :bounds)
                                 :marks (vconcat (seq-map (lambda (m) (list :id (plist-get m :id)
                                                                            :mark (plist-get m :mark)
                                                                            :items (length (plist-get m :items))))
                                                          (plist-get v :marks)))))
                         (plist-get scene :views)))))

(provide 'eas-scene)
;;; eas-scene.el ends here
