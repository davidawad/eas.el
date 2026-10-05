;;; eas-spec-props-test.el --- style properties: honored, named by check, lowered -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.43: the honored-property list is exactly what the audit
;; measures, check names every other property by path without stopping
;; compile, and shared legend and title styles move into config.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-spec-props)
(require 'eas-agent)

(defconst eas-spec-props-test--rows
  [(:k "a" :v 1 :w 2) (:k "b" :v 3 :w 1)]
  "Two rows.")

(ert-deftest eas-spec-props-honored-is-what-the-engine-draws ()
  "Every honored property changes the picture, and no other property does.
Regenerate the list from `eas-spec-props-audit' when this fails."
  (should (equal (eas-spec-props-audit) eas-spec-props-honored)))

(ert-deftest eas-spec-props-check-names-undrawn-properties ()
  (let* ((spec (list :data (list :values eas-spec-props-test--rows) :mark '(:type "bar" :blend "multiply")
                     :encoding '(:x (:field "k" :type "nominal" :axis (:labelBound t :labelColor "red"))
                                 :y (:field "v" :type "quantitative" :scale (:round t)))
                     :config '(:axis (:tickRound :false) :view (:fillOpacity 0.2))))
         (findings (let ((eas-spec-supported-function nil)) (eas-spec-check spec))))
    (should (equal (mapcar (lambda (f) (list (plist-get f :code) (plist-get f :path) (plist-get f :feature))) findings)
                   '(("UNSUPPORTED_FEATURE" "/mark/blend" "property/mark.bar/blend")
                     ("UNSUPPORTED_FEATURE" "/encoding/x/axis/labelBound" "property/axis/labelBound")
                     ("UNSUPPORTED_FEATURE" "/encoding/y/scale/round" "property/scale/round")
                     ("UNSUPPORTED_FEATURE" "/config/axis/tickRound" "property/config.axis/tickRound")
                     ("UNSUPPORTED_FEATURE" "/config/view/fillOpacity" "property/view/fillOpacity"))))
    (should (seq-every-p (lambda (f) (plist-get f :property)) findings))
    ;; The chart still draws natively, without those properties.
    (should-not (let ((eas-spec-supported-function nil)) (eas-spec-unsupported spec)))
    (should (eas-compile spec))
    (let ((data (plist-get (eas-agent "check" (eas-json-encode spec)) :data)))
      (should (eq (plist-get data :native) t))
      (should (= (length (plist-get data :warnings)) 5)))))

(ert-deftest eas-spec-props-enumerations-are-judged-by-value ()
  (should (eas-spec-props-honored-p 'legend 'orient "bottom-right"))
  (should (eas-spec-props-honored-p 'legend 'orient "right"))
  (should-not (eas-spec-props-honored-p 'legend 'orient "bottom"))
  (should (eas-spec-props-honored-p "line" 'interpolate "linear-closed"))
  (should (eas-spec-props-honored-p "line" 'interpolate "basis"))
  (should (eas-spec-props-honored-p "bar" 'aria :false)))

(ert-deftest eas-spec-props-shared-legend-styles-move-to-config ()
  (let* ((legend '(:symbolType "square" :labelColor "red" :title "K"))
         (spec (list :mark "point" :encoding (list :color (list :field "k" :type "nominal" :legend legend))))
         (out (eas-spec-props-lower spec)))
    (should (equal (plist-get (plist-get out :config) :legend) '(:labelColor "red" :symbolType "square")))
    (should (equal (plist-get (plist-get (plist-get out :encoding) :color) :legend) '(:title "K")))
    (should (equal (eas-spec-props-lower out) out))
    ;; No legend channel appears where there was none.
    (should-not (plist-get (plist-get out :encoding) :fill)))
  ;; Two legends that disagree keep their own.
  (let* ((spec '(:layer [(:mark "point" :encoding (:color (:field "k" :type "nominal" :legend (:labelColor "red"))))
                         (:mark "point" :encoding (:size (:field "v" :type "quantitative" :legend (:labelColor "blue"))))]))
         (out (eas-spec-props-lower spec)))
    (should (equal out spec))
    ;; Each legend still draws its own style natively (eas-legend-style.el), so check is quiet.
    (should-not (let ((eas-spec-supported-function nil)) (eas-spec-check out)))))

(ert-deftest eas-spec-props-title-anchor-moves-to-config ()
  (let ((out (eas-spec-props-lower '(:mark "bar" :title (:text "T" :anchor "end")))))
    (should (equal (plist-get out :title) '(:text "T")))
    (should (equal (plist-get (plist-get out :config) :title) '(:anchor "end"))))
  ;; A view title below keeps config.title out of it.
  (let ((spec '(:title (:text "T" :anchor "end") :vconcat [(:mark "bar" :title "cell")])))
    (should (equal (eas-spec-props-lower spec) spec))))

(provide 'eas-spec-props-test)
;;; eas-spec-props-test.el ends here
