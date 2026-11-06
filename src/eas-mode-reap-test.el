;;; eas-mode-reap-test.el --- chart buffers do not leak views (eas-7zl) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-mode-reap-test--spec
  '(:data (:values [(:x 1 :y 2) (:x 2 :y 3)]) :width 200 :height 100
    :mark "line"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")))
  "A small line chart.")

(defmacro eas-mode-reap-test--with-registry (&rest body)
  "Run BODY with a fresh view registry."
  (declare (indent 0))
  `(let ((eas-views (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest eas-mode-killing-chart-buffer-closes-its-view ()
  (eas-mode-reap-test--with-registry
    (let* ((view (eas-view-open eas-mode-reap-test--spec :id "k"))
           (buffer (eas-show view 'text)))
      (should (gethash "k" eas-views))
      (kill-buffer buffer)
      (should-not (gethash "k" eas-views)))))

(ert-deftest eas-mode-reopening-after-kill-reuses-the-id ()
  (eas-mode-reap-test--with-registry
    (let* ((view (eas-view-open eas-mode-reap-test--spec :subject "s"))
           (id (eas-view-id view)))
      (kill-buffer (eas-show view 'text))
      (should (equal (eas-view-id (eas-view-open eas-mode-reap-test--spec :subject "s")) id))
      (should (= (hash-table-count eas-views) 1)))))

(ert-deftest eas-mode-orphan-view-frees-its-id ()
  ;; A view orphaned before the kill hook existed still gives its id back.
  (eas-mode-reap-test--with-registry
    (let ((view (eas-view-open eas-mode-reap-test--spec :id "o"))
          (dead (generate-new-buffer " dead")))
      (kill-buffer dead)
      (setf (eas-view-buffer view) dead)
      (should (equal (eas-view-id (eas-view-open eas-mode-reap-test--spec :id "o")) "o"))
      (should (= (hash-table-count eas-views) 1)))))

(ert-deftest eas-mode-live-view-still-gets-a-new-id ()
  (eas-mode-reap-test--with-registry
    (let* ((view (eas-view-open eas-mode-reap-test--spec :id "l"))
           (buffer (eas-show view 'text)))
      (unwind-protect
          (should (equal (eas-view-id (eas-view-open eas-mode-reap-test--spec :id "l")) "l<2>"))
        (kill-buffer buffer)))))

(ert-deftest eas-mode-reap-hidden-kills-only-long-hidden-chart-buffers ()
  (eas-mode-reap-test--with-registry
    (let* ((eas-reap-hidden-after 100)
           (old (eas-show (eas-view-open eas-mode-reap-test--spec :id "old") 'text))
           (new (eas-show (eas-view-open eas-mode-reap-test--spec :id "new") 'text))
           (now (float-time)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
            (with-current-buffer old (setq buffer-display-time (time-subtract nil 500)))
            (with-current-buffer new (setq buffer-display-time (current-time)))
            (should (equal (eas-mode-reap-hidden now) (list "*eas old*")))
            (should-not (buffer-live-p old))
            (should (buffer-live-p new))
            (should-not (gethash "old" eas-views))
            (should (gethash "new" eas-views))
            (let ((eas-reap-hidden-after nil))
              (should-not (eas-mode-reap-hidden (+ now 10000)))))
        (dolist (b (list old new)) (when (buffer-live-p b) (kill-buffer b)))))))

(provide 'eas-mode-reap-test)
;;; eas-mode-reap-test.el ends here
