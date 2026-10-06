;;; eas-vl-lower.el --- Vega-Lite sugar lowered to the native subset -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2.  Vega-Lite's own compiler normalizes some constructs
;; into others before it compiles; `eas-vl-lower' does the same, as an
;; `eas-spec-rewrite-functions' entry, so parse, check and compile all
;; see the lowered spec and export stays pure Vega-Lite:
;;
;;   data.url, data.sequence   inline data.values (eas-data-url.el)
;;   datasets + data.name      the named dataset's rows as data.values
;;   repeat + spec             layer (repeat.layer), vconcat (repeat.row),
;;                             hconcat (repeat.column or a plain array), a
;;                             grid of rows (row and column, or an array
;;                             with "columns"); {"repeat": "layer"}
;;                             references substituted
;;   mark.point on line/area   a layer of the line and a point overlay,
;;                             as Vega-Lite's PathOverlayNormalizer does
;;   top-level view            merged into config.view (it styles the
;;                             single view cell)
;;
;; Every rewrite is idempotent.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-data-url)

;;; repeat

(defun eas-vl-lower--substitute (value bindings)
  "VALUE with every {\"repeat\": KEY} object replaced from BINDINGS."
  (cond
   ((vectorp value) (vconcat (mapcar (lambda (v) (eas-vl-lower--substitute v bindings)) value)))
   ((and (consp value) (keywordp (car value)))
    (let ((key (plist-get value :repeat)))
      (if (and (stringp key) (null (cddr value)))
          (let ((cell (assoc key bindings)))
            (if cell (cdr cell)
              (eas-signal "INVALID_INPUT" (format "{\"repeat\": %S} is outside a repeat over %s" key key)
                          :path "/spec")))
        ;; Data rows are never substituted (a row may well have a "repeat" field).
        (cl-loop for (k v) on value by #'cddr
                 append (list k (if (eq k :data) v (eas-vl-lower--substitute v bindings)))))))
   (t value)))

(defun eas-vl-lower--repeat (spec)
  "SPEC's repeat expanded into layer, vconcat or hconcat."
  (let* ((repeat (plist-get spec :repeat)) (inner (plist-get spec :spec))
         (columns (plist-get spec :columns))
         (outer (eas--plist-without (eas--plist-without (eas--plist-without spec :repeat) :spec) :columns))
         ;; Rows of cells laid out as Vega-Lite's grid (align "all").
         (grid (lambda (rows) (eas-plist-put (eas-plist-put outer :x-eas (eas-plist-put (plist-get outer :x-eas) :grid t))
                                             :vconcat (vconcat (mapcar (lambda (r) (list :hconcat (vconcat r))) rows)))))
         (cells (lambda (key values bindings)
                  (vconcat (mapcar (lambda (v) (eas-vl-lower--substitute inner (cons (cons key v) bindings)))
                                   values)))))
    (cond
     ;; An array repeats across one row, or wraps after "columns" cells.
     ((vectorp repeat)
      (let ((all (append (funcall cells "repeat" repeat nil) nil)))
        (if (not (and (numberp columns) (> columns 0) (< columns (length all))))
            (eas-plist-put outer :hconcat (vconcat all))
          (funcall grid (cl-loop for i from 0 below (length all) by columns
                                 collect (seq-subseq all i (min (length all) (+ i columns))))))))
     ((plist-get repeat :layer)
      (when (or (plist-get repeat :row) (plist-get repeat :column))
        (eas-signal "UNSUPPORTED_FEATURE" "repeat.layer with row or column is not supported natively"
                    :path "/repeat" :feature "composition/repeat"))
      (eas-plist-put outer :layer (funcall cells "layer" (plist-get repeat :layer) nil)))
     ((and (plist-get repeat :row) (plist-get repeat :column))
      (funcall grid (mapcar (lambda (r) (append (funcall cells "column" (plist-get repeat :column)
                                                         (list (cons "row" r)))
                                                nil))
                            (plist-get repeat :row))))
     ((plist-get repeat :row) (eas-plist-put outer :vconcat (funcall cells "row" (plist-get repeat :row) nil)))
     ;; Vega-Lite aligns repeated cells ("align": "all"), one row of them too.
     ((plist-get repeat :column)
      (eas-plist-put (eas-plist-put outer :hconcat (funcall cells "column" (plist-get repeat :column) nil))
                     :x-eas (append (list :grid t) (plist-get outer :x-eas))))
     (t (eas-signal "INVALID_INPUT" "repeat needs layer, row, column or an array of fields" :path "/repeat")))))

;;; Path overlays

(defun eas-vl-lower--point-overlay (spec)
  "A layer of SPEC's line or area and its point overlay, or SPEC itself."
  (let* ((mark (plist-get spec :mark))
         (type (if (stringp mark) mark (plist-get mark :type)))
         (point (and (consp mark) (plist-get mark :point))))
    (if (not (and (member type '("line" "area" "trail")) point (not (memq point '(:false :null)))))
        (if (and (consp mark) (plist-member mark :point))
            (eas-plist-put spec :mark (eas--plist-without mark :point))
          spec)
      (let* ((enc (plist-get spec :encoding))
             (outer (eas--plist-without (eas--plist-without spec :mark) :encoding))
             (overlay (append (list :type "point" :opacity 1 :filled t)
                              (cl-loop for k in '(:clip :tooltip) when (plist-member mark k)
                                       append (list k (plist-get mark k)))
                              (when (eas-object-p point) point))))
        (eas-plist-put outer :layer
                       (vector (list :mark (eas--plist-without mark :point) :encoding enc)
                               (list :mark (let ((o nil))
                                             ;; Later keys win, as in an object spread.
                                             (cl-loop for (k v) on overlay by #'cddr do (setq o (eas-plist-put o k v)))
                                             o)
                                     :encoding (eas--plist-without enc :shape))))))))

;;; Entry point

(defun eas-vl-lower--view (spec)
  "SPEC with its top-level view properties moved into config.view."
  (let ((view (plist-get spec :view)))
    (if (not (and view (eas-object-p view))) spec
      (let* ((config (plist-get spec :config)) (cv (plist-get config :view)))
        (cl-loop for (k v) on view by #'cddr do (setq cv (eas-plist-put cv k v)))
        (eas-plist-put (eas--plist-without spec :view) :config (eas-plist-put config :view cv))))))

(defun eas-vl-lower--named (spec datasets)
  "SPEC with every data.name named in DATASETS replaced by its rows."
  (cond
   ((vectorp spec) (vconcat (mapcar (lambda (s) (eas-vl-lower--named s datasets)) spec)))
   ((and (consp spec) (keywordp (car spec)))
    (cl-loop for (k v) on spec by #'cddr
             append (list k (if (and (eq k :data) (eas-object-p v) (stringp (plist-get v :name))
                                     (vectorp (plist-get datasets (eas-key (plist-get v :name)))))
                                (list :values (plist-get datasets (eas-key (plist-get v :name))))
                              (eas-vl-lower--named v datasets)))))
   (t spec)))

(defun eas-vl-lower--datasets (spec)
  "SPEC with its top-level datasets inlined where data.name names them."
  (let ((datasets (plist-get spec :datasets)))
    (if (not (and datasets (eas-object-p datasets))) spec
      (eas-vl-lower--named (eas--plist-without spec :datasets) datasets))))

(defun eas-vl-lower (spec)
  "SPEC with Vega-Lite sugar lowered to the native subset, recursively."
  (if (not (and spec (eas-object-p spec))) spec
    (let ((out (eas-vl-lower--view (eas-data-url-inline (eas-vl-lower--datasets spec)))))
      (when (plist-get out :repeat) (setq out (eas-vl-lower--repeat out)))
      (dolist (key '(:layer :vconcat :hconcat))
        (when (vectorp (plist-get out key))
          (setq out (eas-plist-put out key (vconcat (mapcar #'eas-vl-lower (plist-get out key)))))))
      (if (plist-get out :mark) (eas-vl-lower--point-overlay out) out))))

(add-hook 'eas-spec-rewrite-functions #'eas-vl-lower)

(provide 'eas-vl-lower)
;;; eas-vl-lower.el ends here
