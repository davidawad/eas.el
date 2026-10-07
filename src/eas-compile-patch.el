;;; eas-compile-patch.el --- incremental compile for selection changes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4/L6.  Hover and clicks change selection stores, not
;; domains, data or size.  `eas-compile-patch' updates a compile plan
;; for such a change instead of compiling again:
;;
;;   unit filtered by a changed param    recompile that unit alone, if the
;;                                       view's scales rebuilt over the new
;;                                       units come out the same, and its
;;                                       values fit them or nothing can
;;                                       overhang into the layout
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
(require 'eas-compile-memo)
(require 'eas-link-scale)
(require 'eas-layout)
(require 'eas-text-snap)
(require 'eas-vega-bounds)

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

(defvar eas-patch--mentioned (make-hash-table :test 'eq :weakness 'key)
  "VALUE -> (TEXT (NAMES . MENTIONED) ...) for `eas-patch--mentions'.
A unit's transforms and encoding outlive the frames that patch it.")

(defun eas-patch--mentions (value names)
  "Members of NAMES that occur as words in VALUE's expression strings."
  (if (null value) nil
    (let* ((cell (or (gethash value eas-patch--mentioned)
                     (puthash value (list (format "%S" value)) eas-patch--mentioned)))
           (hit (assoc names (cdr cell))))
      (if hit (cdr hit)
        (let ((found (seq-filter (lambda (n) (string-match-p (concat "\\_<" (regexp-quote n) "\\_>") (car cell)))
                                 names)))
          (setcdr cell (cons (cons (copy-sequence names) found) (seq-take (cdr cell) 7)))
          found)))))

(defun eas-patch--changed (old new)
  "Names of params whose selection differs between states OLD and NEW."
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

(defun eas-patch--fixed-layout-p (plan group state)
  "Return non-nil when no mark of GROUP can move PLAN's layout.
A plan fitted to a size, a text plan, autosize none and a clipped view
never grow their canvas around the marks.  STATE is the view state."
  (or (plist-get plan :size) (eas-layout-text-p (plist-get plan :metrics))
      (eas-vega-bounds-autosize-none-p (plist-get plan :spec))
      (eas-compile--clipped-p group state)))

(defun eas-patch--same-scales-p (group units state metrics)
  "Non-nil when GROUP's scales, rebuilt over UNITS, equal its current ones.
STATE and METRICS are as for `eas-compile--scales'.  GROUP is not
changed."
  (let ((probe (copy-sequence group)))
    (plist-put probe :units units)
    (eas-compile--scales probe state metrics)
    (eas-compile--ranges probe)
    (when (eas-layout-text-p metrics) (eas-text-snap-bands probe metrics))
    (and (equal (plist-get probe :scales) (plist-get group :scales))
         (equal (plist-get probe :axis-defs) (plist-get group :axis-defs))
         (equal (plist-get probe :legend-specs) (plist-get group :legend-specs)))))

(defconst eas-patch--unscaled '(:text :tooltip :href :detail :key :order :description :url)
  "Channels that feed no scale.")

(defun eas-patch--explicit-p (def)
  "Non-nil when DEF has no scale or one whose domain its data cannot move.
That is a value def, or a field def without bins whose scale is null or
has a literal array for its domain (and does not discretize)."
  (let ((d (eas-encode-data-def def)))
    (or (null d)
        (and (eas-object-p d)
             (memq (plist-get d :bin) '(nil :false))
             (let ((scale (plist-get d :scale)))
               ;; scale: null draws values as they are.
               (or (eq scale :null)
                   (let ((domain (plist-get scale :domain)))
                     ;; A quantile scale's domain is the data's sample.
                     (and (not (member (plist-get scale :type) '("quantile" "quantize" "threshold" "bin-ordinal")))
                          (vectorp domain) (> (length domain) 0)
                          (seq-every-p (lambda (v) (or (numberp v) (stringp v))) domain)))))))))

(defun eas-patch--fixed-scales-p (old fresh)
  "Non-nil when unit FRESH, refiltered from OLD, cannot change any scale.
Both encode alike and every scaled channel has a literal domain, so
the rows they draw feed no domain, axis or legend (eas-b2s.3: the probe
`eas-patch--same-scales-p' is then not needed)."
  (let ((enc (plist-get fresh :encoding)))
    (and (equal enc (plist-get old :encoding))
         (equal (plist-get fresh :mark) (plist-get old :mark))
         (cl-loop for (ch def) on enc by #'cddr
                  always (cond ((memq ch eas-patch--unscaled))
                               ;; x2 and y2 share their partner's scale.
                               ((memq ch '(:x2 :y2))
                                (let ((partner (plist-get enc (if (eq ch :x2) :x :y))))
                                  (and partner (eas-patch--explicit-p partner))))
                               (t (eas-patch--explicit-p def)))))))

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
  "Non-nil when UNIT conditions a position channel on a CHANGED param."
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
Only the rows a point-selection index names are tested (fc-qx1.9).
GROUP gives the plot bounds and METRICS the layout; OLD and NEW are
the states before and after."

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

(defvar eas-patch--refusals (make-hash-table :test 'eq :weakness 'key)
  "Plan source spec -> ((CHANGED . COUNT) ...): patches refused in a row.
COUNT counts the patches for the params CHANGED (sorted names) that
moved a scale since the last one that held.")

(defvar eas-patch-retry 8
  "After two refusals, a patch for the same params is tried every Nth time.
The ones between go straight to a full compile: the slider of
pi-monte-carlo moves a data domain on every step, and recompiling its
units only to find that out costs more than the compile itself.")

(defun eas-patch--refusal (plan changed)
  "The (CHANGED . COUNT) cell of PLAN's source for CHANGED param names."
  (when-let* ((source (plist-get plan :source)))
    (let ((runs (gethash source eas-patch--refusals)))
      (or (assoc changed runs)
          (car (puthash source (cons (cons changed 0) (seq-take runs 7)) eas-patch--refusals))))))

(defun eas-compile-patch (plan old new)
  "Update PLAN from view state OLD to NEW, or return nil for a full compile.
Must run with selection hooks bound to NEW (`eas-params-with-state')."
  (let* ((changed (sort (eas-patch--changed old new) #'string<))
         (cell (and changed (eas-patch--refusal plan changed))))
    (if (and cell (>= (cdr cell) 2) (/= 0 (% (cdr cell) eas-patch-retry)))
        (progn (cl-incf (cdr cell)) nil)
      (let ((out (eas-compile-memo (eas-compile-patch--1 plan old new))))
        (when cell (setcdr cell (if (eq out 'refused) (1+ (cdr cell)) 0)))
        (and (not (eq out 'refused)) out)))))

(defun eas-compile-patch--1 (plan old new)
  "Do `eas-compile-patch' of PLAN from OLD to NEW, transforms memoized.
Return the plan, nil, or `refused' when new rows moved a scale."
  (catch 'full
    (unless (equal (plist-get old :domains) (plist-get new :domains)) (throw 'full nil))
    (let* ((changed (eas-patch--changed old new))
           (env (eas-compile--env (plist-get plan :spec) new))
           (metrics (plist-get plan :metrics))
           (units nil))
      ;; A scale domain that follows a changed selection moves every item.
      (when (seq-intersection changed (eas-link-plan-domain-params plan)) (throw 'full nil))
      (when changed
        (dolist (group (plist-get plan :groups))
          (let ((refiltered nil) (probe nil))
            (setq
             units
             (mapcar
              (lambda (unit)
                (let* ((ctx (plist-get unit :ctx))
                       (filter-deps (append (eas-patch--param-names (plist-get ctx :transforms))
                                            (eas-patch--mentions (plist-get ctx :transforms) changed))))
                  (cond
                   ;; A projection reading a changed param moves every shape (eas-geoshape.el).
                   ((eas-patch--mentions (plist-get (plist-get (plist-get unit :node) :x-eas) :geo) changed)
                    (throw 'full nil))
                   ((seq-intersection filter-deps changed)
                    (let ((fresh (append (eas-compile--unit (plist-get unit :node) ctx env)
                                         (list :node (plist-get unit :node) :ctx ctx))))
                      (setq refiltered t)
                      (unless (eas-patch--fixed-scales-p unit fresh) (setq probe t))
                      (unless (or (eas-patch--fits fresh group) (eas-patch--fixed-layout-p plan group new))
                        (throw 'full 'refused))
                      fresh))
                   ((eas-patch--mentions (plist-get unit :encoding) changed)
                    (if (or (eas-patch--positional-p unit changed)
                            (member (plist-get (plist-get unit :mark) :type) '("line" "area" "trail" "geoshape"))
                            (null (plist-get unit :items))
                            ;; A test condition may read any param: rebuild every item.
                            (seq-some (lambda (d) (let ((c (plist-get d :condition)))
                                                    (or (plist-get d :test)
                                                        (seq-some (lambda (x) (and (eas-object-p x) (plist-get x :test)))
                                                                  (cond ((vectorp c) c) (c (list c)))))))
                                      (cl-loop for (_ d) on (plist-get unit :encoding) by #'cddr
                                               when (eas-object-p d) collect d)))
                        (eas-plist-put (eas-plist-put unit :items nil) :env env)
                      (let ((pairs (eas-patch--conditions unit changed)))
                        (eas-plist-put (eas-plist-put unit :items (eas-patch--items unit group metrics old new pairs))
                                       :env env))))
                   (t unit))))
              (plist-get group :units)))
            ;; New rows can move a data domain either way (or leave an
            ;; explicit one): keep the plan only if the scales a full
            ;; compile would build are the ones it has.
            (when (and refiltered probe (not (eas-patch--same-scales-p group units new metrics)))
              (throw 'full 'refused))
            (plist-put group :units units))))
      (eas-plist-put plan :env env))))

(provide 'eas-compile-patch)
;;; eas-compile-patch.el ends here
