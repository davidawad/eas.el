;;; eas-compile-shared.el --- shared scales and legends across concat views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite resolves the non-positional scales of
;; concatenated views as shared unless resolve.scale or resolve.legend
;; says "independent": one color (size, opacity) scale over the union
;; of the views' domains, and one legend for it at the top level, to
;; the right of the whole composition and level with its top.
;; `eas-shared-prepare' merges the scales and lifts the legends off the
;; views; layout reserves room for them with `eas-shared-extent' and
;; places them with `eas-shared-place', which hands them to the first
;; view to draw.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-legend)
(require 'eas-layout)
(require 'eas-encode)

(declare-function eas-compile--less "eas-compile-scales")

(defun eas-shared--independent-p (spec channel)
  "Non-nil when SPEC resolves CHANNEL's scale or legend independently."
  (let ((name (eas-key (eas-key-name channel))) (resolve (plist-get spec :resolve)))
    (or (equal (plist-get (plist-get resolve :scale) name) "independent")
        (equal (plist-get (plist-get resolve :legend) name) "independent"))))

(defun eas-shared--merge (a b)
  "Scale A widened by scale B's domain (same channel)."
  (pcase (plist-get a :type)
    ("ordinal"
     (let* ((da (plist-get a :domain))
            (domain (vconcat (delete-dups (append da (plist-get b :domain) nil)))))
       (if (= (length domain) (length da)) a
         (let ((range (if (> (length (plist-get b :domain)) (length da)) (plist-get b :range) (plist-get a :range))))
           (plist-put (plist-put (copy-sequence a) :domain (vconcat (sort (append domain nil) #'eas-compile--less)))
                      :range range)))))
    (_ (let ((da (plist-get a :domain)) (db (plist-get b :domain)))
         (if (and (numberp (aref da 0)) (numberp (aref db 0)))
             (plist-put (copy-sequence a) :domain (vector (min (aref da 0) (aref db 0)) (max (aref da 1) (aref db 1))))
           a)))))

(defun eas-shared-prepare (tree groups spec)
  "Share TREE's non-positional scales across GROUPS unless SPEC resolves
them independently; lift their legends onto TREE as :shared-legends."
  (when (and (plist-get tree :concat) (cdr groups))
    (let (shared)
      (dolist (g groups)
        (dolist (ls (plist-get g :legend-specs))
          (let* ((ch (plist-get ls :channel))
                 (slot (if (memq ch '(:color :fill :stroke)) :color ch)))
            (unless (eas-shared--independent-p spec ch)
              (let ((old (assq slot shared)))
                (if old (setcdr old (plist-put (copy-sequence (cdr old)) :scale
                                               (eas-shared--merge (plist-get (cdr old) :scale) (plist-get ls :scale))))
                  (setq shared (append shared (list (cons slot ls))))))))))
      (when shared
        (dolist (g groups)
          (let ((scales (plist-get g :scales)))
            (pcase-dolist (`(,_ . ,ls) shared)
              (let ((ch (plist-get ls :channel)))
                (when (plist-get scales ch) (setq scales (plist-put scales ch (plist-get ls :scale))))))
            (plist-put g :scales scales)
            (plist-put g :legend-specs
                       (seq-filter (lambda (ls) (eas-shared--independent-p spec (plist-get ls :channel)))
                                   (plist-get g :legend-specs)))))
        (plist-put tree :shared-legends (mapcar #'cdr shared)))))
  tree)

(defun eas-shared-models (tree metrics plot-h)
  "TREE's shared legend models under METRICS (gradients PLOT-H long)."
  (delq nil (mapcar (lambda (ls)
                      (let ((l (eas-legend-model ls metrics)))
                        (and l (eas-legend-sized (append l (list :plot-h plot-h)) metrics))))
                    (plist-get tree :shared-legends))))

(defun eas-shared--size (legend metrics)
  "(W . H) of LEGEND, without its offset from the plot."
  (if (eas-layout-text-p metrics)
      (let ((s (eas-legend-size legend metrics))) (cons (- (car s) (plist-get metrics :legend-offset)) (cdr s)))
    (let ((b (plist-get (eas-legend-place legend 0 0 metrics) :box)))
      (cons (ceiling (aref b 2)) (ceiling (aref b 3))))))

(defun eas-shared-extent (tree metrics plot-h)
  "Width TREE's shared legends add to the right of the layout (0 if none)."
  (let ((models (eas-shared-models tree metrics plot-h)))
    (if (null models) 0
      (+ (plist-get metrics :legend-offset)
         (apply #'max (mapcar (lambda (l) (car (eas-shared--size l metrics))) models))))))

(defun eas-shared-place (tree groups metrics x y)
  "Give the first of GROUPS TREE's shared legends, stacked from X Y."
  (when-let* ((models (eas-shared-models tree metrics (plist-get (car groups) :h))))
    (let ((x (+ x (plist-get metrics :legend-offset))) placed)
      (dolist (l models)
        (push (list l x y) placed)
        (setq y (+ y (cdr (eas-shared--size l metrics))
                   (if (eas-layout-text-p metrics) 0 (plist-get metrics :legend-margin)))))
      (plist-put (car groups) :shared-legends (nreverse placed)))))

(defun eas-shared-axis-def (pairs)
  "The axis def for a layer's (UNIT . DEF) PAIRS on one channel.
Vega-Lite titles a shared axis with its layers' distinct titles joined
by \", \" unless the first def sets one."
  (let* ((def (cdar pairs))
         (titles (delete-dups (delq nil (mapcar (lambda (p) (eas-encode-title (cdr p))) pairs)))))
    (if (or (cdr titles) (null titles)) 
        (if (or (plist-member (plist-get def :axis) :title) (plist-member def :title) (null (cdr titles))) def
          (eas-plist-put def :title (string-join titles ", ")))
      def)))

(provide 'eas-compile-shared)
;;; eas-compile-shared.el ends here
