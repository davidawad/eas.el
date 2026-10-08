;;; eas-geo-native.el --- the optional native geo backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  An optional dynamic module, eas-geo-module (Rust, in
;; module/), does the hot half of a map's first render: it projects,
;; resamples and clips shapes, measures their anchors and prints their
;; SVG path data, number for number as eas-geo-stream.el,
;; eas-geo-clip.el, eas-geo-proj.el and eas-geoshape-render.el do.  The
;; Elisp stays the source of truth and the fallback: eas needs no
;; module, and every map draws the same without one.
;;
;; `eas-geo-backend' picks the backend: `auto' (the module when it
;; loads, else Elisp), `native' (the module or a `user-error') or
;; `lisp'.  The
;; module is loaded lazily, on the first map, from <root>/lib/,
;; <root>/module/ or beside this file (a flat install), named
;; eas-geo-module plus `module-file-suffix'.  Under auto, a module that
;; fails at run time is dropped with one message and Elisp draws on.
;;
;; The module works on batches: a geometry is read once into a handle
;; (`eas-geo-native-handle', kept weakly per coordinates object), and
;; one call projects every shape of a map or measures a fit.  A shape's
;; paths come back packed, unpacked on first read (`eas-geo-native-paths').

;;; Code:

(require 'cl-lib)
(require 'eas-core)

(defgroup eas nil
  "Emacs-native interactive charts."
  :group 'applications
  :prefix "eas-")

(defcustom eas-geo-backend
  (pcase (getenv "EAS_GEO_BACKEND")
    ("lisp" 'lisp) ("native" 'native) (_ 'auto))
  "Which backend projects map shapes.
The initial value follows the environment variable EAS_GEO_BACKEND,
set to `lisp', `native' or `auto', so bin/eas and batch runs can pick
one without Lisp; any other value means `auto'.
The symbol `auto' uses the native module eas-geo-module when it loads
and Elisp otherwise; `native' requires the module (a `user-error' when
it is missing or fails to load); `lisp' never loads it.  Both backends
draw the same SVG."
  :type '(choice (const :tag "Module if available, else Elisp" auto)
                 (const :tag "Native module (required)" native)
                 (const :tag "Elisp only" lisp))
  :group 'eas)

(defconst eas-geo-native--here
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "The directory of this file.")

(defconst eas-geo-native--root
  (if (file-directory-p (expand-file-name "templates" eas-geo-native--here))
      eas-geo-native--here
    (file-name-directory (directory-file-name eas-geo-native--here)))
  "The eas root, as `eas-template--root' finds it.")

(defvar eas-geo-native--state nil
  "Nil before the module was looked for, `loaded', or (failed . REASON).")

(defun eas-geo-module-candidates ()
  "Files the module is looked for in, in order."
  (when (stringp module-file-suffix)
    (let ((name (concat "eas-geo-module" module-file-suffix)))
      (append
       (mapcar (lambda (dir) (expand-file-name name (expand-file-name dir eas-geo-native--root)))
               '("lib" "module"))
       (list (expand-file-name name eas-geo-native--here))
       ;; A cargo build in place: module/target/release/libeas_geo.
       (mapcar (lambda (ext) (expand-file-name (concat "module/target/release/libeas_geo" ext)
                                               eas-geo-native--root))
               (delete-dups (list module-file-suffix ".so" ".dylib")))))))

(defun eas-geo-module-file ()
  "The module file this Emacs would load, or nil."
  (seq-find #'file-exists-p (eas-geo-module-candidates)))

(defun eas-geo-native--load ()
  "Load the module once; return non-nil when it is loaded."
  (pcase eas-geo-native--state
    ('loaded t)
    (`(failed . ,_) nil)
    (_
     (condition-case err
         (let ((file (eas-geo-module-file)))
           (cond ((not (and (stringp module-file-suffix) (fboundp 'module-load)))
                  (error "This Emacs was built without dynamic module support"))
                 ((not file)
                  (error "The module is not built (looked for %s)"
                         (mapconcat #'abbreviate-file-name (eas-geo-module-candidates) ", ")))
                 (t (unless (featurep 'eas-geo-module) (module-load file))
                    (unless (fboundp 'eas-geo-module-shapes)
                      (error "%s does not define eas-geo-module-shapes" file))))
           (setq eas-geo-native--state 'loaded)
           t)
       (error (setq eas-geo-native--state (cons 'failed (error-message-string err)))
              nil)))))

(defun eas-geo-native-reason ()
  "Why the module is not in use, or nil when it is loaded."
  (pcase eas-geo-native--state ('loaded nil) (`(failed . ,r) r) (_ "not loaded yet")))

(defun eas-geo-backend-active ()
  "The backend that projects map shapes now: the symbol `lisp' or `native'.
Load the module on first use when `eas-geo-backend' allows it.  With
`eas-geo-backend' native, signal a `user-error' when it cannot load."
  (pcase eas-geo-backend
    ('lisp 'lisp)
    ('native (if (eas-geo-native--load) 'native
               (user-error "The native geo backend is unavailable: %s (eas-geo-backend is native)"
                           (eas-geo-native-reason))))
    (_ (if (eas-geo-native--load) 'native 'lisp))))

(defun eas-geo-native-p ()
  "Non-nil when map shapes go through the native module."
  (eq (eas-geo-backend-active) 'native))

(defun eas-geo-native--failed (err)
  "Handle module error ERR: re-signal it under native, else drop the module."
  (if (eq eas-geo-backend 'native)
      (signal (car err) (cdr err))
    (setq eas-geo-native--state (cons 'failed (error-message-string err)))
    (message "eas: the native geo backend failed (%s); drawing maps with Elisp"
             (error-message-string err))
    nil))

(defmacro eas-geo-native--guard (&rest body)
  "BODY's value, or nil after `eas-geo-native--failed' on an error."
  (declare (indent 0) (debug t))
  `(condition-case err (progn ,@body) (error (eas-geo-native--failed err))))

;;; Geometry handles

(defvar eas-geo-native--handles (make-hash-table :test 'eq :weakness 'key)
  "Module handles per geometry, weakly by its coordinates object.
Each value is (TYPE . HANDLE).")

(defvar eas-geo-native--sphere nil "The module handle of a Sphere.")

(declare-function eas-geo-module-geometry "eas-geo-module")
(declare-function eas-geo-module-shapes "eas-geo-module")
(declare-function eas-geo-module-fit "eas-geo-module")
(declare-function eas-geo-module-paths "eas-geo-module")

(defconst eas-geo-native--codes
  '(("Point" . 2) ("MultiPoint" . 3) ("LineString" . 4) ("MultiLineString" . 5)
    ("Polygon" . 6) ("MultiPolygon" . 7))
  "The module's code of each coordinate geometry type.")

(defun eas-geo-native--geometry (g)
  "Return the module handle of GeoJSON geometry G.
G is read as `eas-geo-stream-geometry' streams it."
  (let* ((type (and (eas-object-p g) (plist-get g :type)))
         (code (cdr (assoc type eas-geo-native--codes)))
         (coords (plist-get g :coordinates)))
    (cond
     ((equal type "Sphere")
      (or eas-geo-native--sphere (setq eas-geo-native--sphere (eas-geo-module-geometry 1 nil))))
     ((and code (vectorp coords))
      (let ((hit (gethash coords eas-geo-native--handles)))
        (if (equal (car hit) type) (cdr hit)
          (let ((h (eas-geo-module-geometry code coords)))
            (puthash coords (cons type h) eas-geo-native--handles)
            h))))
     ((equal type "GeometryCollection")
      (let* ((gs (plist-get g :geometries))
             (hit (and (vectorp gs) (gethash gs eas-geo-native--handles))))
        (if (equal (car hit) type) (cdr hit)
          (let ((h (eas-geo-module-geometry 8 (vconcat (mapcar #'eas-geo-native--geometry gs)))))
            (when (vectorp gs) (puthash gs (cons type h) eas-geo-native--handles))
            h))))
     (t (eas-geo-module-geometry 0 nil)))))

(defun eas-geo-native-handle (o)
  "Return the module handle of GeoJSON object O.
O is read as `eas-geo-stream-object' streams it."
  (pcase (and (eas-object-p o) (plist-get o :type))
    ("Feature" (eas-geo-native--geometry (plist-get o :geometry)))
    ("FeatureCollection"
     (eas-geo-module-geometry 8 (vconcat (mapcar (lambda (f) (eas-geo-native--geometry (plist-get f :geometry)))
                                                 (plist-get o :features)))))
    (_ (eas-geo-native--geometry o))))

(defun eas-geo-native-forget ()
  "Forget every module handle, as a fresh Emacs would have none."
  (clrhash eas-geo-native--handles)
  (setq eas-geo-native--sphere nil))

;;; Calls

(defun eas-geo-native-fit (params objects)
  "Bounds [X0 Y0 X1 Y1] of OBJECTS under projection PARAMS, or nil.
PARAMS is a projection's :native; nil when the module is not in use."
  (and params (eas-geo-native-p)
       (eas-geo-native--guard
         (eas-geo-module-fit params (vconcat (mapcar #'eas-geo-native-handle objects))))))

(defun eas-geo-native-shapes (params shapes ox oy tolerance)
  "SHAPES projected under projection PARAMS for items placed at OX OY.
Return a vector with, per shape, nil or [AX AY PATHS CIRCLES BOX D]: the
anchor, the paths, circles and box relative to it (as
`eas-geoshape--relative'), and the SVG path data of the paths moved by
OX + AX, OY + AY.  Non-empty PATHS stay in the module, packed, when it
can unpack them (`eas-geo-native-paths'): a map's SVG needs only D,
and its hundreds of thousands of floats are made when the text
renderer or a hit test reads them.  TOLERANCE as `eas-geo-proj-path'.
Return nil when the module is not in use or failed."
  (and params (eas-geo-native-p)
       (eas-geo-native--guard
         (let ((handles (vconcat (mapcar #'eas-geo-native-handle shapes))))
           ;; A module built before eas-geo-module-paths takes 6 arguments.
           (if (fboundp 'eas-geo-module-paths)
               (eas-geo-module-shapes params handles ox oy tolerance 4.5 t)
             (eas-geo-module-shapes params handles ox oy tolerance 4.5))))))

(defvar eas-geo-native--unpacked (make-hash-table :test 'eq :weakness 'key)
  "Paths unpacked by `eas-geo-native-paths', weakly by their packed handle.")

(defun eas-geo-native-paths (packed)
  "The [CLOSED XY...] vectors of PACKED paths, made once per PACKED.
PACKED comes from `eas-geo-native-shapes'."
  (or (gethash packed eas-geo-native--unpacked)
      (puthash packed (eas-geo-module-paths packed) eas-geo-native--unpacked)))

(provide 'eas-geo-native)
;;; eas-geo-native.el ends here
