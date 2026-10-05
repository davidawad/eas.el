;;; eas-compile-patch.el --- incremental compile for selection changes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4/L6.  Hover and clicks change selection stores, not
;; domains, data or size.  `eas-compile-patch' updates a compile plan
;; for such a change instead of compiling again:
;;
;;   unit filtered by a changed param    recompile that unit alone, if its
;;                                       values still fit the view's scales
;;   unit with a condition on a changed  rebuild only the items whose row
;;   param (style channels)              changed membership
;;   anything else                       reuse cached items and index
;;
;; It returns nil whenever a full compile is the only correct answer
;; (zoom, push, conditions on position, values outside a scale, a scale
;; domain that follows a changed selection).

;;; Code:

(require 'eas-core)
(require 'eas-params)
(require 'eas-params-index)
(require 'eas-marks)
(require 'eas-compile)
(require 'eas-compile-scales)
(require 'eas-link-scale)

(defun eas-patch--param-names (value)
  "Param names referenced by {\"param\": ...} anywhere in VALUE."
  (let (names)
    (cl-labels ((walk (v)
                  (cond ((vectorp v) (seq-do #'walk v))
                        ((and (consp v) (keywordp (car v)))
                         (when (stringp (plist-get v :param)) (push (plist-get v :param) names))
                         (cl-loop for (_ x) on v by #'cddr do (walk x))))))
      (walk value))
    names))

(defun eas-patch--mentions (value names)
  "Members of NAMES that occur as words in VALUE's expression strings."
  (let ((text (format "%S" value)))
    (seq-filter (lambda (n) (string-match-p (concat "\\_<" (regexp-quote n) "\\_>") text)) names)))

(defun eas-patch--changed (old new)
  "Names of selection stores that differ between states OLD and NEW."
  (let ((a (plist-get old :params)) (b (plist-get new :params)) names)
    (dolist (k (delete-dups (append (eas-plist-keys a) (eas-plist-keys b))))
      (unless (equal (plist-get a k) (plist-get b k)) (push (eas-key-name k) names)))
    names))

(defun eas-patch--fits (unit group)
  "Non-nil when UNIT's x/y values lie inside GROUP's current scales."
  (cl-loop for ch in '(:x :y)
           for scale = (plist-get (plist-get group :scales) ch)
           for pairs = (eas-compile--defs (list unit) ch)
           always (or (null scale) (null pairs)
                      (let ((values (eas-compile--values pairs ch)) (d (plist-get scale :domain)))
                        (if (member (plist-get scale :type) '("band" "point" "ordinal"))
                            (seq-every-p (lambda (v) (seq-contains-p d v)) values)
                          (let ((lo (min (aref d 0) (aref d 1))) (hi (max (aref d 0) (aref d 1))))
                            (seq-every-p (lambda (v) (let ((x (eas-params--number v))) (or (null x) (<= lo x hi))))
                                         values)))))))

(defun eas-patch--conditions (unit changed)
  "(PARAM . EMPTY) pairs of UNIT's encoding conditions on CHANGED params."
  (let (pairs)
    (cl-loop for (_ def) on (plist-get unit :encoding) by #'cddr
             do (seq-do (lambda (c)
                          (when (member (plist-get c :param) changed)
                            (push (cons (plist-get c :param) (not (eq (plist-get c :empty) :false))) pairs)))
                        (let ((c (and (eas-object-p def) (plist-get def :condition))))
                          (cond ((vectorp c) c) (c (list c))))))
    pairs))

(defun eas-patch--positional-p (unit changed)
  "Non-nil when a condition on a CHANGED param sits on a position channel."
  (cl-loop for ch in '(:x :y :x2 :y2)
           thereis (seq-some (lambda (n) (member n changed))
                              (eas-patch--param-names (plist-get (plist-get unit :encoding) ch)))))

(defvar eas-patch--positions (make-hash-table :test 'eq :weakness 'key)
  "Unit rows vector -> hash of datum -> position in the unit's items.
Patching only replaces items in place, so positions hold while the rows
vector lives; a full compile makes a new one.")

(defun eas-patch--where (rows items)
  "Datum -> position in ITEMS, for the unit whose rows are ROWS."
  (or (gethash rows eas-patch--positions)
      (let ((where (make-hash-table :test 'eql :size (max 1 (length items)))))
        (dotimes (k (length items)) (puthash (plist-get (aref items k) :datum) k where))
        (puthash rows where eas-patch--positions))))

(defun eas-patch--items (unit group metrics old new pairs)
  "UNIT's items with rows whose membership changed (per PAIRS) rebuilt.
Only the rows a point-selection index names are tested (fc-qx1.9)."
  (let* ((rows (plist-get unit :rows)) (items (copy-sequence (plist-get unit :items)))
         (where (eas-patch--where rows items))
         (changed (eas-params-index-changed rows pairs old new))
         (bounds (vector (plist-get group :x0) (plist-get group :y0) (plist-get group :w) (plist-get group :h))))
    (eas-marks-with-cache
     (let ((row-fn (eas-marks-row-fn unit (plist-get group :scales) bounds metrics)))
       (dolist (i (if (eq changed 'all) (number-sequence 0 (1- (length rows))) changed))
         (let ((row (aref rows i)))
           (when (seq-some (lambda (p) (not (eq (not (eas-params-test old (car p) row (cdr p)))
                                                (not (eas-params-test new (car p) row (cdr p))))))
                           pairs)
             (when-let* ((k (gethash i where)) (item (funcall row-fn row i)))
               (aset items k item)))))))
    items))

(defun eas-compile-patch (plan old new)
  "Update PLAN from view state OLD to NEW, or return nil for a full compile.
Must run with selection hooks bound to NEW (`eas-params-with-state')."
  (catch 'full
    (unless (equal (plist-get old :domains) (plist-get new :domains)) (throw 'full nil))
    (let* ((changed (eas-patch--changed old new))
           (env (eas-compile--env (plist-get plan :spec) new))
           (metrics (plist-get plan :metrics)))
      ;; A scale domain that follows a changed selection moves every item.
      (when (seq-intersection changed (eas-link-plan-domain-params plan)) (throw 'full nil))
      (when changed
        (dolist (group (plist-get plan :groups))
          (plist-put
           group :units
           (mapcar
            (lambda (unit)
              (let* ((ctx (plist-get unit :ctx))
                     (filter-deps (append (eas-patch--param-names (plist-get ctx :transforms))
                                          (eas-patch--mentions (plist-get ctx :transforms) changed))))
                (cond
                 ((seq-intersection filter-deps changed)
                  (let ((fresh (append (eas-compile--unit (plist-get unit :node) ctx env)
                                       (list :node (plist-get unit :node) :ctx ctx))))
                    (unless (eas-patch--fits fresh group) (throw 'full nil))
                    fresh))
                 ((eas-patch--mentions (plist-get unit :encoding) changed)
                  (if (or (eas-patch--positional-p unit changed)
                          (member (plist-get (plist-get unit :mark) :type) '("line" "area" "trail"))
                          (null (plist-get unit :items))
                          (seq-some (lambda (c) (plist-get c :test))
                                    (cl-loop for (_ d) on (plist-get unit :encoding) by #'cddr
                                             when (eas-object-p d) collect d)))
                      (eas-plist-put (eas-plist-put unit :items nil) :env env)
                    (let ((pairs (eas-patch--conditions unit changed)))
                      (eas-plist-put (eas-plist-put unit :items (eas-patch--items unit group metrics old new pairs))
                                       :env env))))
                 (t unit))))
            (plist-get group :units)))))
      plan)))

(provide 'eas-compile-patch)
;;; eas-compile-patch.el ends here
