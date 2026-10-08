;;; eas-geo-native-test.el --- tests for the native geo backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The native module (module/, eas-geo-native.el) must return what the
;; Elisp returns: the same projected paths, anchors, boxes and fit
;; bounds, bit for bit, and the same SVG path data.  Each parity test
;; draws with both backends in one session and compares; it skips, with
;; the reason, when the module is not built.  EAS_GEO_BACKEND=lisp or
;; native runs the whole suite under one backend (eas-test-support.el).

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-test-support)
(require 'eas-geo-native)
(require 'eas-view)
(require 'eas-svg)

(defun eas-geo-native-test--require ()
  "Skip the current test unless the native module loads."
  (unless (let ((eas-geo-backend 'auto)) (eq (eas-geo-backend-active) 'native))
    (eas-test-skip (format "the native geo module is not built (%s); build it with cargo in module/"
                           (eas-geo-native-reason)))))

(defmacro eas-geo-native-test--state (&rest body)
  "Run BODY, then restore the module's load state."
  (declare (indent 0))
  `(let ((saved eas-geo-native--state))
     (unwind-protect (progn ,@body) (setq eas-geo-native--state saved))))

(ert-deftest eas-geo-native-backend-selection ()
  "lisp never loads the module; without it auto falls back and native errs."
  (eas-geo-native-test--state
    (let ((eas-geo-backend 'lisp)) (should (eq (eas-geo-backend-active) 'lisp)))
    (cl-letf (((symbol-function 'eas-geo-module-candidates) (lambda () nil)))
      (unless (featurep 'eas-geo-module)
        (setq eas-geo-native--state nil)
        (let ((eas-geo-backend 'auto)) (should (eq (eas-geo-backend-active) 'lisp)))
        (should (string-match-p "not built" (eas-geo-native-reason)))
        (let ((eas-geo-backend 'native)) (should-error (eas-geo-backend-active) :type 'user-error))))))

(ert-deftest eas-geo-native-module-candidates ()
  "The module is looked for under lib/ and module/ of the root, and beside the library."
  (let ((files (eas-geo-module-candidates)))
    (when module-file-suffix
      (should (member (expand-file-name (concat "lib/eas-geo-module" module-file-suffix) eas-geo-native--root) files))
      (should (member (expand-file-name (concat "module/eas-geo-module" module-file-suffix) eas-geo-native--root)
                      files))
      (should (member (expand-file-name (concat "eas-geo-module" module-file-suffix) eas-geo-native--here) files)))))

;;; Parity of projected shapes

(defun eas-geo-native-test--shapes ()
  "Test shapes: sphere, graticule, points, a line and some countries."
  (let ((world (append (eas-topojson-features
                        (eas-json-read-file (eas-test-file "test" "vega-examples" "data" "world-110m.json"))
                        "countries")
                       nil)))
    (append (list (list :type "Sphere")
                  (eas-geo-graticule)
                  (list :type "Point" :coordinates [12 45])
                  (list :type "MultiPoint" :coordinates [[-170 10] [170.5 -10] [0 89.9]])
                  (list :type "LineString" :coordinates [[-179 0] [179 1] [10 60] [-100 -80]])
                  (list :type "Feature" :geometry (list :type "GeometryCollection"
                                                        :geometries (vector (list :type "Point" :coordinates [1 2])
                                                                            (list :type "LineString"
                                                                                  :coordinates [[0 0] [90 0]])))))
            ;; Fiji, Tanzania, ..., Canada, the USA; Russia; Antarctica
            (seq-take world 6) (list (nth 18 world))
            (seq-filter (lambda (f) (equal (plist-get (plist-get f :geometry) :type) "MultiPolygon"))
                        (seq-drop world 150)))))

(defun eas-geo-native-test--projections ()
  "Projection specs: every type, and variants of rotation, clip and precision."
  (append
   (mapcar (lambda (type) (list :type type)) (eas-geo-raw-names))
   (cl-loop for type in '("equalEarth" "orthographic" "mercator" "conicConformal" "transverseMercator"
                          "interruptedMollweide" "polyhedralButterfly" "berghaus" "stereographic" "albers")
            append (list (list :type type :rotate [30 -20 10])
                         (list :type type :clipAngle 60)
                         (list :type type :clipExtent [[20 30] [380 260]])
                         (list :type type :reflectX t :reflectY t :angle 15)
                         (list :type type :precision 0)
                         (list :type type :rotate [-150 70 0] :clipAngle 120)))
   (list (list :type "albersUsa" :precision 2) (list :type "conicEqualArea" :parallels [10 70]))))

(defun eas-geo-native-test--unpacked (value)
  "VALUE, as `eas-geoshape--project' makes it, with its paths unpacked."
  (cons (car value) (cons (eas-geoshape-paths (list :paths (cadr value))) (cddr value))))

(defun eas-geo-native-test--compare (spec shapes)
  "Problems found projecting SHAPES under projection SPEC with both backends."
  (let* ((proj (eas-geo-proj spec 120 '(200 150))) (problems nil))
    (dolist (o '((0 . 0) (10.5 . 20.25)))
      (let ((out (eas-geo-native-shapes (plist-get proj :native) shapes (car o) (cdr o) eas-geoshape-tolerance)))
        (cl-loop for shape in shapes for r across out for i from 0
                 do (let* ((lisp (eas-geoshape--project proj shape))
                           (native (and r (eas-geo-native-test--unpacked
                                           (eas-geoshape--native-value r (car o) (cdr o))))))
                      (cond
                       ((not (equal lisp native))
                        (push (format "%S shape %d: projected values differ" spec i) problems))
                       ((and r (not (equal (aref r 5) (eas-geoshape--svg-rings (+ (car o) (aref r 0)) (+ (cdr o) (aref r 1))
                                                                               (eas-geoshape-paths (list :paths (aref r 2)))))))
                        (push (format "%S shape %d: SVG path data differ" spec i) problems)))))))
    (let ((objects (seq-remove (lambda (s) (equal (plist-get s :type) "Sphere")) shapes)))
      (dolist (objs (list shapes objects))
        (let ((native (eas-geo-proj-fit spec objs [0 0 400 300]))
              (lisp (let ((eas-geo-backend 'lisp)) (eas-geo-proj-fit spec objs [0 0 400 300]))))
          (unless (equal native lisp)
            (push (format "%S: fits differ, %S and %S" spec native lisp) problems)))))
    problems))

(ert-deftest eas-geo-native-projections-match-lisp ()
  "Every projection type and variant projects every test shape as the Elisp does.
Anchors, relative paths, circles and boxes are compared with `equal',
so bit for bit, as are SVG path data and fit bounds."
  (eas-geo-native-test--require)
  (let ((eas-geo-backend 'native) (shapes (eas-geo-native-test--shapes)) problems)
    (eas-geoshape-forget)
    (dolist (spec (eas-geo-native-test--projections))
      (setq problems (append problems (eas-geo-native-test--compare spec shapes))))
    (eas-geoshape-forget)
    (should (equal problems nil))))

(ert-deftest eas-geo-native-paths-stay-packed-until-read ()
  "Native paths are a module handle, unpacked once, to the Elisp's values."
  (eas-geo-native-test--require)
  (let* ((eas-geo-backend 'native) (shapes (eas-geo-native-test--shapes))
         (proj (eas-geo-proj (list :type "equalEarth") 120 '(200 150)))
         (out (eas-geo-native-shapes (plist-get proj :native) shapes 0 0 eas-geoshape-tolerance))
         (i (cl-position-if (lambda (r) (and r (user-ptrp (aref r 2)))) out))
         (item (list :paths (aref (aref out i) 2))))
    (should (eq (eas-geoshape-paths item) (eas-geoshape-paths item)))
    (should (equal (eas-geoshape-paths item)
                   (car (cdr (let ((eas-geo-backend 'lisp)) (eas-geoshape--project proj (nth i shapes)))))))))

;;; Parity of map templates

(defun eas-geo-native-test--svg (name backend)
  "The SVG of map template NAME drawn cold with BACKEND."
  (let ((eas-geo-backend backend))
    (eas-geoshape-forget)
    (let ((view (eas-view-open name :bindings (eas-template-example name) :target 'svg)))
      (unwind-protect (eas-svg-render (eas-view-scene view)) (eas-view-close view)))))

(defun eas-geo-native-test--templates (names)
  "Names of NAMES whose SVG differs between the backends."
  (prog1 (seq-remove (lambda (name) (equal (eas-geo-native-test--svg name 'lisp)
                                           (eas-geo-native-test--svg name 'native)))
                     names)
    (eas-geoshape-forget)))

(defconst eas-geo-native-test-maps
  '("world-map" "projections" "county-unemployment" "map-with-tooltip" "zoomable-world-map"
    "distortion-comparison" "annual-precipitation" "airport-connections" "dorling-cartogram"
    "volcano-contours" "earthquakes" "earthquakes-globe")
  "The map templates of templates/vega/.")

(ert-deftest eas-geo-native-world-map-svg-is-identical ()
  "world-map and earthquakes-globe draw the same SVG with both backends."
  (eas-geo-native-test--require)
  (should (equal (eas-geo-native-test--templates '("world-map" "earthquakes-globe")) nil)))

(ert-deftest eas-geo-native-map-templates-svg-is-identical ()
  "Every map template draws byte-identical SVG with both backends.
A minute or so with the Elisp backend, so it runs with the gallery."
  :tags '(:gallery)
  (eas-geo-native-test--require)
  (should (equal (eas-geo-native-test--templates eas-geo-native-test-maps) nil)))

(ert-deftest eas-geo-native-auto-falls-back-on-a-module-error ()
  "Under auto, a module that fails is dropped with a message; Elisp draws on."
  (eas-geo-native-test--require)
  (eas-geo-native-test--state
    (let ((expected (eas-geo-native-test--svg "world-map" 'lisp)) (messages nil))
      (cl-letf (((symbol-function 'eas-geo-module-shapes) (lambda (&rest _) (error "Simulated failure")))
                ((symbol-function 'eas-geo-module-fit) (lambda (&rest _) (error "Simulated failure")))
                ((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (should (equal (eas-geo-native-test--svg "world-map" 'auto) expected))
        (should (= (length messages) 1))
        (should (string-match-p "Simulated failure" (car messages)))
        (let ((eas-geo-backend 'auto)) (should (eq (eas-geo-backend-active) 'lisp)))))
    (eas-geoshape-forget)))

(ert-deftest eas-geo-native-errors-signal-under-native ()
  "Under native, a module error is signalled, not hidden."
  (eas-geo-native-test--require)
  (eas-geo-native-test--state
    (cl-letf (((symbol-function 'eas-geo-module-fit) (lambda (&rest _) (error "Simulated failure"))))
      (let ((eas-geo-backend 'native))
        (should-error (eas-geo-proj-fit (list :type "mercator") (list (list :type "Point" :coordinates [1 2]))
                                        [0 0 100 100]))))))

(ert-deftest eas-geo-backend-initial-value-follows-the-environment ()
  (dolist (case '(("lisp" . lisp) ("native" . native) ("auto" . auto) ("nonsense" . auto) (nil . auto)))
    (let ((process-environment (cons (if (car case) (concat "EAS_GEO_BACKEND=" (car case)) "EAS_GEO_BACKEND")
                                     process-environment)))
      (should (eq (car (read-from-string
                        (shell-command-to-string
                         (format "%s -Q --batch -L %s -l eas-geo-native --eval '(prin1 eas-geo-backend)'"
                                 (shell-quote-argument (expand-file-name invocation-name invocation-directory))
                                 (shell-quote-argument (expand-file-name "src" eas-test-root))))))
                  (cdr case))))))

(provide 'eas-geo-native-test)
;;; eas-geo-native-test.el ends here
