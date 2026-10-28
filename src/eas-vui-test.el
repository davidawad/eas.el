;;; eas-vui-test.el --- the optional vui.el components -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; vui.el is a soft dependency.  Without it eas and eas-vui load and
;; the functions under the components work in any buffer; that runs
;; always.  With it (EAS_VUI_DIR names a vui.el checkout, or vui is on
;; `load-path'), the components mount on a vui page, draw the chart,
;; click through to :on-click and list a row in a card; otherwise that
;; test skips and says why.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-vui)
(require 'eas-test-support)

(declare-function vui-mount "vui" (component-vnode &optional buffer-name no-display))
(declare-function vui-component "vui" (type &rest props-and-children))

(defconst eas-vui-test--bindings
  '(:nodes [(:name "In" :p 0.9) (:name "Out" :grade "A")]
    :links [(:source "In" :target "Out" :value 3)])
  "A one-link flow whose nodes carry fields of their own.")

(defun eas-vui-test--find (echo)
  "The first position of the current buffer whose help-echo is ECHO."
  (cl-loop for pos from (point-min) below (point-max)
           when (equal (get-text-property pos 'help-echo) echo) return pos))

(defun eas-vui-test--batch (&rest forms)
  "Run FORMS in a fresh `emacs -Q --batch' with only src/ on the path.
Return what the last form prints."
  (with-temp-buffer
    (let ((status (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                         "-Q" "--batch" "-L" (eas-test-file "src")
                         (mapcan (lambda (f) (list "--eval" (prin1-to-string f))) forms))))
      (should (equal (list status (buffer-string)) (list 0 (buffer-string))))
      (car (last (split-string (string-trim (buffer-string)) "\n"))))))

(ert-deftest eas-vui-loads-and-works-without-vui ()
  "eas never loads eas-vui or vui; eas-vui loads without vui and draws."
  (should (equal (eas-vui-test--batch
                  '(require 'eas)
                  '(prin1 (list (featurep 'eas-vui) (featurep 'vui) (locate-library "vui"))))
                 "(nil nil nil)"))
  (should (equal (eas-vui-test--batch
                  '(require 'eas-vui)
                  '(let ((s (eas-vui-chart-string (eas-vui-chart-open "sankey" :target 'text :size '(:cols 40 :rows 10)
                                                                      :bindings '(:nodes [(:name "a") (:name "b")]
                                                                                  :links [(:source "a" :target "b" :value 1)])))))
                     (prin1 (list (featurep 'vui) (eas-vui-available-p) (fboundp 'eas-vui-chart-string)
                                  (and (get-text-property 0 'eas-vui-view s) t)
                                  (> (length s) 400)))))
                 "(nil nil t t t)")))

(ert-deftest eas-vui-chart-string-clicks-through-in-any-buffer ()
  "The chart string drops into any buffer; RET on a node passes its row on."
  (let* ((eas-views (make-hash-table :test 'equal))
         (clicked nil)
         (view (eas-vui-chart-open "sankey" :id "vui-t" :bindings eas-vui-test--bindings
                                   :target 'text :size '(:cols 50 :rows 12))))
    (unwind-protect
        (with-temp-buffer
          ;; A page: a heading, then the chart indented two columns.
          (insert "Flows\n")
          (let ((start (point)))
            (insert (eas-vui-chart-string view :on-click (lambda (row _click) (setq clicked row))))
            (indent-rigidly start (point) 2))
          (should (equal (get-text-property (point-min) 'eas-vui-view) nil))
          ;; Point on a cell of the Out node: the node mark, right column.
          (goto-char (point-min))
          (let ((pos (eas-vui-test--find "name: Out\ngrade: A\nvalue: 3")))
            (should pos)
            (goto-char pos)
            (should (eq (lookup-key (get-text-property pos 'keymap) (kbd "RET")) #'eas-vui-click))
            (let ((inhibit-message t)) (eas-vui-click))
            (should (equal (plist-get clicked :name) "Out"))
            (should (equal (plist-get clicked :grade) "A"))))
      (eas-vui-chart-close "vui-t"))
    (should-not (gethash "vui-t" eas-views)))
  ;; An SVG chart is one character displaying the image, its hot spots
  ;; bound to the click.
  (let* ((eas-views (make-hash-table :test 'equal))
         (s (eas-vui-chart-string (eas-vui-chart-open "sankey" :bindings eas-vui-test--bindings :target 'svg)))
         (image (get-text-property 0 'display s))
         (area (nth 1 (car (plist-get (cdr image) :map)))))
    (should (= (length s) 1))
    (should (eq (car image) 'image))
    (should (eq (lookup-key (get-text-property 0 'keymap s) (vector area 'mouse-1)) #'eas-vui-click))))

(ert-deftest eas-vui-card-lists-a-rows-own-fields ()
  "A card shows every field but the engine's own, or the fields asked for."
  (should (equal (eas-vui-card-lines '(:name "In" :p 0.9 :eas_x0 3 :value 3))
                 '(("name" . "In") ("p" . "0.9") ("value" . "3"))))
  (should (equal (eas-vui-card-lines '(:name "In" :p 0.9 :value 3) '("value" "name"))
                 '(("value" . "3") ("name" . "In")))))

(defun eas-vui-test--vui ()
  "Load vui from EAS_VUI_DIR or `load-path'; non-nil when it loaded."
  (let ((dir (getenv "EAS_VUI_DIR")))
    (when (and dir (file-directory-p dir)) (add-to-list 'load-path (expand-file-name dir)))
    (require 'vui nil t)))

(ert-deftest eas-vui-components-on-a-vui-page ()
  "With vui, the chart and card components mount on a page and click through."
  (unless (eas-vui-test--vui)
    (eas-test-skip "vui.el is not installed: set EAS_VUI_DIR to a vui.el checkout"))
  (let ((eas-views (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *eas-vui-test*"))
        (clicked nil))
    (unwind-protect
        (progn
          (vui-mount (vui-component 'eas-vui-chart
                                    :source "sankey" :bindings eas-vui-test--bindings :id "page"
                                    :size '(:cols 50 :rows 12) :target 'text
                                    :on-click (lambda (row _click) (setq clicked row)))
                     (buffer-name buffer) t)
          (with-current-buffer buffer
            (should (gethash "page" eas-views))
            (should (string-match-p "Out" (buffer-string)))
            (goto-char (eas-vui-test--find "name: In\np: 0.9\nvalue: 3"))
            (let ((inhibit-message t)) (eas-vui-click))
            (should (equal (plist-get clicked :name) "In")))
          (vui-mount (vui-component 'eas-vui-card :title "Node" :row '(:name "In" :p 0.9 :eas_x0 1))
                     (buffer-name buffer) t)
          (with-current-buffer buffer
            (should (string-match-p "\\`Node\nname  In\np     0.9" (buffer-string))))
          ;; Mounting the card replaced the chart: its view is closed.
          (should-not (gethash "page" eas-views)))
      (kill-buffer buffer))))

(provide 'eas-vui-test)
;;; eas-vui-test.el ends here
