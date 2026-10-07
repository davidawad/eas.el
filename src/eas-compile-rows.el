;;; eas-compile-rows.el --- incremental compile for pushed rows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4/L6 (eas-7r1.19).  A live view (an order book pushed a few
;; times a second, a ticker) gets new rows every frame while its
;; scales, axes and layout mostly stay put.  A full compile spends most
;; of a frame placing axes and chrome (`eas-place-layout'), which only
;; depends on the scales.  `eas-compile-rows-patch' updates a compile
;; plan for new root rows instead:
;;
;;   transforms and units    run again (they read the rows)
;;   scales                  rebuilt, then mapped onto the old plot
;;   layout, axes, legends   kept when every scale, axis and legend
;;                           spec is `equal' to the old plan's
;;   marks                   rebuilt only for units whose rows changed
;;
;; It returns nil whenever a full compile is the only correct answer: a
;; scale domain moved (a nice domain that grows a tick, a new category),
;; a unit's encoding or mark changed, the plan has several views, a
;; facet header, or a size that follows the marks (no fitted SIZE: Vega
;; grows the canvas around what the marks overhang).  The result is the
;; plan a full compile would make; a property test compares the scenes
;; over random push sequences (eas-live-test.el).

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-compile)
(require 'eas-compile-memo)
(require 'eas-compile-place)
(require 'eas-text-snap)
(require 'eas-container)
(require 'eas-layout)
(require 'eas-marks)
(require 'eas-polar)
(require 'eas-independent)

(defvar eas-compile-rows-stats (list :patched 0 :full 0 :units 0 :reused 0)
  "Counters of `eas-compile-rows-patch'.
Plans patched, refused (full), units seen and units whose items were
kept.")

(defconst eas-compile-rows--unit-keys '(:path :name :mark :encoding :aggregated :params)
  "Unit keys that must not change for a patch to keep the layout.")

(defconst eas-compile-rows--geometry
  '(:x0 :y0 :w :h :chrome :grid-cell :legend-limit)
  "Group keys the layout sets that scale ranges read.")

(defun eas-compile-rows--count (key)
  "Increment `eas-compile-rows-stats' KEY."
  (plist-put eas-compile-rows-stats key (1+ (or (plist-get eas-compile-rows-stats key) 0))))

(defun eas-compile-rows--same-units (old new)
  "Non-nil when unit lists OLD and NEW draw the same layers the same way."
  (and (= (length old) (length new))
       (cl-every (lambda (o n)
                   (cl-every (lambda (k) (equal (plist-get o k) (plist-get n k)))
                             eas-compile-rows--unit-keys))
                 old new)))

(defun eas-compile-rows--items (group old unit metrics)
  "UNIT's items in GROUP, keeping OLD unit's items for unchanged rows.
Only per-row marks (one item per row, `eas-marks-row-fn') reuse items;
return nil (items built afresh) for anything else.  METRICS are the
layout's."
  (let ((rows (plist-get unit :rows)) (old-rows (plist-get old :rows))
        (items (plist-get old :items)))
    (when (and (vectorp items) (= (length items) (length old-rows) (length rows))
               (not (eas-polar-unit-p unit)))
      (eas-marks-with-cache
       (when-let* ((row-fn (eas-marks-row-fn
                            unit (eas-independent-unit-scales group unit)
                            (vector (plist-get group :x0) (plist-get group :y0)
                                    (plist-get group :w) (plist-get group :h))
                            metrics)))
         (catch 'fresh
           (cl-loop with out = (make-vector (length rows) nil)
                    for i below (length rows)
                    do (aset out i (if (equal (aref rows i) (aref old-rows i)) (aref items i)
                                     (or (funcall row-fn (aref rows i) i) (throw 'fresh nil))))
                    finally return out)))))))

(defun eas-compile-rows--group (old new state metrics)
  "Return OLD group with NEW group's units if NEW has OLD's scales.
Return nil otherwise.
NEW comes from collecting the new rows; its scales are built under
STATE and METRICS and mapped onto OLD's placed plot."
  (when (eas-compile-rows--same-units (plist-get old :units) (plist-get new :units))
    (eas-compile--scales new state metrics)
    (dolist (k eas-compile-rows--geometry)
      (when (plist-member old k) (plist-put new k (plist-get old k))))
    (eas-compile--ranges new)
    (when (eas-layout-text-p metrics) (eas-text-snap-bands new metrics))
    (when (and (equal (plist-get new :scales) (plist-get old :scales))
               (equal (plist-get new :axis-defs) (plist-get old :axis-defs))
               (equal (plist-get new :legend-specs) (plist-get old :legend-specs)))
      (let ((group (copy-sequence old)))
        (plist-put group :units
                   (cl-mapcar
                    (lambda (o n)
                      (eas-compile-rows--count :units)
                      (if (equal (plist-get o :rows) (plist-get n :rows))
                          (progn (eas-compile-rows--count :reused) o)
                        (let ((unit (copy-sequence o)))
                          (cl-loop for (k v) on n by #'cddr do (setq unit (plist-put unit k v)))
                          (plist-put unit :items (eas-compile-rows--items group o unit metrics))
                          (plist-put unit :index nil))))
                    (plist-get old :units) (plist-get new :units)))))))

(cl-defun eas-compile-rows-patch (plan rows &key size state)
  "PLAN recompiled for new root ROWS, reusing its layout; or nil.
SIZE is the size PLAN was compiled at and STATE the view state (it must
be the state PLAN was made under).  The returned plan is new: PLAN and
the scene made from it are not changed."
  (let* ((metrics (plist-get plan :metrics))
         (spec (plist-get plan :spec))
         (old (plist-get plan :groups))
         (result
          (and size (= (length old) 1) (not (plist-get (car old) :header))
               (not (plist-get plan :cut))
               (let* ((gc-cons-threshold (max gc-cons-threshold eas-compile-gc-threshold))
                      (eas-compile-scales-text (eas-layout-text-p metrics))
                      (eas-container-autosize (eas-container-autosize-of spec))
                      (eas-encode-count-title (let ((c (plist-get (plist-get spec :config) :countTitle)))
                                                (and (stringp c) c)))
                      (rows (if (eas-data-p rows) (plist-get rows :rows) rows))
                      (tree (eas-compile-memo
                             (eas-compile--collect spec (list :path "" :rows [] :transforms nil :encoding nil
                                                              :config (plist-get metrics :config))
                                                   (plist-get plan :env) rows nil)))
                      (new (eas-place--groups tree))
                      (group (and (= (length new) 1) (plist-get tree :group)
                                  (equal (plist-get (car new) :id) (plist-get (car old) :id))
                                  (eas-compile-rows--group (car old) (car new) state metrics))))
                 (and group (eas-plist-put plan :groups (list group)))))))
    (eas-compile-rows--count (if result :patched :full))
    result))

(provide 'eas-compile-rows)
;;; eas-compile-rows.el ends here
