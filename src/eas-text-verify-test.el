;;; eas-text-verify-test.el --- text fixes from the final verification -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.50: what the combined run of .48 and .49 found in the text
;; backend: a bar under half a pixel tall drew a full cell, a binned y
;; against a counted x drew its bars standing up, and a normalized
;; stack's top sliver drew no cell at 60x16.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-text-check)
(require 'eas-text-gallery)

(defun eas-text-verify-test--scene (spec size)
  "SPEC compiled for text at SIZE, gallery-native."
  (eas-vl-gallery--native (eas-compile spec :target 'text :size size)))

(defun eas-text-verify-test--mark (scene)
  "The first mark of SCENE's first view."
  (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0))

(ert-deftest eas-text-verify-tiny-bar-is-no-full-cell ()
  "A bar under half a pixel tall shows the smallest eighth, not a block."
  (let* ((scene (eas-text-verify-test--scene
                 '(:data (:values [(:k "a" :v 10000) (:k "b" :v 1)]) :mark "bar"
                   :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
                 '(:cols 40 :rows 12)))
         (mark (eas-text-verify-test--mark scene))
         (tiny (aref (plist-get mark :items) 1))
         (text (eas-text-render scene))
         (chars (cl-loop for i below (length text)
                         when (and (equal (get-text-property i 'eas-mark text) (plist-get mark :id))
                                   (equal (get-text-property i 'eas-datum text) 1))
                         collect (aref text i))))
    (should (< (plist-get tiny :h) 0.5))
    (should chars)
    (should (seq-every-p (lambda (c) (eq c ?▁)) chars))))

(ert-deftest eas-text-verify-binned-y-bars-lie-down ()
  "Vega-Lite orients a bar with binned y and a counted x horizontally."
  (let* ((scene (eas-text-verify-test--scene
                 '(:data (:values [(:v 1) (:v 2) (:v 2) (:v 7) (:v 8) (:v 8) (:v 8)]) :mark "bar"
                   :encoding (:y (:field "v" :bin t) :x (:aggregate "count")))
                 '(:cols 40 :rows 12)))
         (items (plist-get (eas-text-verify-test--mark scene) :items)))
    (should (> (length items) 0))
    (should (seq-every-p (lambda (i) (equal (plist-get i :orient) "horizontal")) items))
    (should-not (eas-text-check scene))))

(ert-deftest eas-text-verify-normalized-top-sliver-shows ()
  "The top slice of a normalized stack, under half of every cell it
touches, colors the background of the slice beneath's eighth block."
  (let* ((spec (eas-text-gallery-spec "area-circular" "stacked_area_normalize"))
         (scene (eas-text-verify-test--scene spec '(:cols 60 :rows 16)))
         (text (eas-text-render scene)))
    (should-not (eas-text-check scene))
    (should (plist-get (get-text-property 0 'face text) :background))))

(provide 'eas-text-verify-test)
;;; eas-text-verify-test.el ends here
