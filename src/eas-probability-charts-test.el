;;; eas-probability-charts-test.el --- kd probability chart specs render their data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The nine probability chart specs kd emits (eas-ges), kept verbatim
;; in test/fixtures/probability-charts/.  Every render path must carry
;; their data: wherever a mark's field varies over its rows, the
;; channel it encodes must vary over the mark's items, under both the
;; svg and text targets.  An all-zero or constant render fails.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-agent)

(defconst eas-probability-charts-test--names
  '("surface" "disagreement" "dumbbell" "fan" "flow" "tree" "reliability" "frontier" "hops")
  "The kd chart outputs under test/fixtures/probability-charts/.")

(defun eas-probability-charts-test--file (name)
  "The fixture file of chart NAME."
  (eas-test-file "test/fixtures/probability-charts" (concat name ".json")))

(defun eas-probability-charts-test--spec (name)
  "The resolved spec of fixture NAME."
  (eas-resolve-spec (eas-json-read-file (eas-probability-charts-test--file name))))

(defun eas-probability-charts-test--encoding (spec path)
  "The encoding of SPEC's unit at PATH (\"/layer/2/layer/0\"), parents merged."
  (let ((node spec) (enc (plist-get spec :encoding)))
    (dolist (step (split-string (or path "") "/" t))
      (setq node (if (string-match-p "\\`[0-9]+\\'" step)
                     (aref node (string-to-number step))
                   (plist-get node (eas-key step))))
      (when (and (eas-object-p node) (plist-get node :encoding))
        (cl-loop for (ch def) on (plist-get node :encoding) by #'cddr
                 do (setq enc (eas-plist-put enc ch def)))))
    enc))

(defun eas-probability-charts-test--channel-values (channel item)
  "The values ITEM shows for CHANNEL, a list (points give one per point)."
  (let ((points (append (plist-get item :points) nil))
        (base (append (plist-get item :base) nil))
        (get (lambda (k) (plist-get item k))))
    (pcase channel
      (:x (if points (mapcar (lambda (p) (aref p 0)) points)
            (list (or (funcall get :x) (funcall get :x1)))))
      (:y (if points (mapcar (lambda (p) (aref p 1)) points)
            (list (or (funcall get :y) (funcall get :y1)))))
      (:x2 (cond (base (mapcar (lambda (p) (aref p 0)) base))
                 ((funcall get :x2) (list (funcall get :x2)))
                 ((funcall get :w) (list (+ (funcall get :x) (funcall get :w))))))
      (:y2 (cond (base (mapcar (lambda (p) (aref p 1)) base))
                 ((funcall get :y2) (list (funcall get :y2)))
                 ((funcall get :h) (list (+ (funcall get :y) (funcall get :h))))))
      (:color (list (let ((fill (funcall get :fill)))
                      (if (member fill '(nil "none")) (funcall get :stroke) fill))))
      ((or :fill :stroke :size :shape :text :opacity :strokeWidth)
       (list (funcall get channel))))))

(defun eas-probability-charts-test--marks (scene)
  "Every mark of SCENE, nested views included."
  (let (marks)
    (cl-labels ((walk (views)
                  (seq-doseq (v views)
                    (seq-doseq (m (plist-get v :marks)) (push m marks))
                    (walk (plist-get v :views)))))
      (walk (plist-get scene :views)))
    (nreverse marks)))

(defun eas-probability-charts-test--check (name spec scene)
  "Assert each mark of SCENE (compiled from fixture NAME's SPEC) encodes its data."
  (let ((marks (eas-probability-charts-test--marks scene)))
    (should marks)
    (dolist (mark marks)
      (let ((items (append (plist-get mark :items) nil))
            (rows (append (plist-get mark :rows) nil))
            (enc (eas-probability-charts-test--encoding spec (plist-get mark :path))))
        (should (list name (plist-get mark :id) :items items))
        (cl-loop
         for (channel def) on enc by #'cddr
         for field = (and (eas-object-p def) (plist-get def :field))
         for data = (and field (seq-uniq (delq nil (mapcar (lambda (r) (plist-get r (eas-key field))) rows))))
         for shown = (seq-uniq (delq nil (mapcan (lambda (it) (eas-probability-charts-test--channel-values channel it))
                                                 items)))
         when (and (cdr data) (memq channel '(:x :y :x2 :y2 :color :fill :stroke :size :shape :text
                                                 :opacity :strokeWidth)))
         do (should (equal (list name (plist-get mark :id) channel field :distinct (and (cdr shown) t))
                           (list name (plist-get mark :id) channel field :distinct t))))))))

(ert-deftest eas-probability-charts-encode-their-data ()
  "Each mark of every fixture gets non-empty, non-constant values from its data."
  (dolist (name eas-probability-charts-test--names)
    (let ((spec (eas-probability-charts-test--spec name)))
      (dolist (target '(svg text))
        (let ((scene (eas-compile spec :target target)))
          (eas-probability-charts-test--check name spec scene)
          ;; The scene/v1 JSON agents and explain read.
          (should (stringp (eas-scene-to-json scene)))))
      ;; The live view, as eas-show draws it in Emacs.
      (let ((view (eas-view-open spec :id (concat "prob-" name))))
        (unwind-protect
            (progn
              (eas-probability-charts-test--check name spec (eas-view-scene view))
              (should (string-prefix-p "<svg" (eas-svg-render (eas-view-scene view)))))
          (eas-view-close view))))))

(ert-deftest eas-probability-charts-svg-carries-the-values ()
  "The in-Emacs SVG paints the surface's cells by probability, not one color."
  (let* ((svg (eas-svg-render (eas-compile (eas-probability-charts-test--spec "surface"))))
         (fills (let (out (start 0))
                  (while (string-match "<rect x=\"[0-9.]+\" y=\"[0-9.]+\" width=\"82.5\"[^>]*fill=\"\\(#[0-9a-f]+\\)\"" svg start)
                    (push (match-string 1 svg) out)
                    (setq start (match-end 0)))
                  out)))
    (should (= (length fills) 8))
    (should (= (length (seq-uniq fills)) 8))))

(ert-deftest eas-probability-charts-export-keeps-the-data ()
  "The browser path, export --vl, keeps every layer's rows and values."
  (dolist (name eas-probability-charts-test--names)
    (let* ((fixture (eas-json-read-file (eas-probability-charts-test--file name)))
           (env (eas-agent "export" (eas-probability-charts-test--file name) :vl t))
           (vl (plist-get env :data))
           (values (lambda (spec)
                     (cons (plist-get (plist-get spec :data) :values)
                           (seq-map (lambda (l) (plist-get (plist-get l :data) :values))
                                    (plist-get spec :layer))))))
      (should (eq (plist-get env :ok) t))
      (should (equal (list name (funcall values vl)) (list name (funcall values fixture)))))))

(ert-deftest eas-probability-charts-stroke-width-scales-its-field ()
  "The flow's rule widths map mass onto its scale.range [1, 12]."
  (let* ((scene (eas-compile (eas-probability-charts-test--spec "flow")))
         (widths (seq-map (lambda (it) (plist-get it :strokeWidth))
                          (plist-get (eas-scene-mark scene "main/0") :items))))
    (should (< (abs (- (seq-min widths) 1)) 1e-9))
    (should (< (abs (- (seq-max widths) 12)) 1e-9))))

(ert-deftest eas-probability-charts-tooltips-apply-format ()
  "A tooltip field's d3 format shows, as in Vega-Lite: \".1%\" gives 83.9%."
  (let* ((scene (eas-compile (eas-probability-charts-test--spec "surface")))
         (tip (plist-get (aref (plist-get (eas-scene-mark scene "main/0") :items) 0) :tooltip)))
    (should (equal (plist-get (aref tip 2) :value) "83.9%"))
    (should (equal (plist-get (aref tip 0) :value) "150"))))

(provide 'eas-probability-charts-test)
;;; eas-probability-charts-test.el ends here
