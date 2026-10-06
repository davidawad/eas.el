;;; eas-babel-test.el --- tests for the org-babel surface (fc-qx1.13) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'org)
(require 'ox-ascii)
(require 'ob-eas)

(defconst eas-babel-test--table
  "#+name: tbl
| date       | category | value |
|------------+----------+-------|
| 2026-03-02 | Mon      |  8200 |
| 2026-03-03 | Tue      | 10400 |
| 2026-03-04 | Wed      |  6100 |
| 2026-03-05 | Thu      |  9300 |
"
  "Four days of steps as a named org table.")

(defconst eas-babel-test--block
  "\n#+name: steps\n#+begin_src eas :template bars :data tbl :title \"Steps\" :cols 60 :rows 14%s\n%s#+end_src\n"
  "A bars block over tbl; format with extra headers and a body.")

(defmacro eas-babel-test--with-org (headers body &rest forms)
  "Run FORMS in an org buffer holding the table and a block with HEADERS and BODY.
Point starts on the block; views and inline state are fresh."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-babel-sources (make-hash-table :test 'equal))
         (eas-babel-inline--views (make-hash-table :test 'equal))
         (eas-babel-inline--pending nil)
         (eas-babel-target 'text)
         (org-confirm-babel-evaluate nil))
     (with-temp-buffer
       (insert eas-babel-test--table (format eas-babel-test--block ,headers ,body))
       (org-mode)
       (goto-char (point-min))
       (re-search-forward "#\\+begin_src eas")
       (beginning-of-line)
       ,@forms)))

(defun eas-babel-test--execute ()
  "Execute the block at point and return its result."
  (save-excursion (org-babel-execute-src-block)))

(defun eas-babel-test--item-px (view mark i)
  "Centre px of item I of MARK in VIEW's first scene view."
  (let* ((sv (aref (plist-get (eas-view-scene view) :views) 0))
         (m (seq-find (lambda (m) (equal (plist-get m :id) mark)) (plist-get sv :marks)))
         (it (aref (plist-get m :items) i)))
    (vector (+ (plist-get it :x) (/ (plist-get it :w) 2.0))
            (+ (plist-get it :y) (/ (plist-get it :h) 2.0)))))

(defun eas-babel-test--px-pos (view px)
  "Buffer position of the inline result cell under PX in VIEW."
  (let* ((ov (plist-get (eas-babel-inline--entry view) :overlay))
         (cell (plist-get (plist-get (eas-view-scene view) :size) :cell)))
    (save-excursion
      (goto-char (overlay-start ov))
      (forward-line (floor (aref px 1) (aref cell 1)))
      (move-to-column (+ (eas-babel-inline--prefix) (floor (aref px 0) (aref cell 0))))
      (point))))

;;; Data and bindings

(ert-deftest eas-babel-data-reads-tables-results-and-files ()
  (should (equal (plist-get (eas-babel-data '(("x" "y") hline (1 2) (2 3))) :rows)
                 [(:x 1 :y 2) (:x 2 :y 3)]))
  (should (equal (plist-get (eas-babel-data '((:x 1) (:x 2))) :rows) [(:x 1) (:x 2)]))
  (let ((csv (make-temp-file "eas-babel" nil ".csv" "x,y\n1,2\n")))
    (unwind-protect
        (should (equal (plist-get (eas-babel-data csv) :rows) [(:x 1 :y 2)]))
      (delete-file csv)))
  (eas-babel-test--with-org "" ""
    (should (equal (aref (plist-get (eas-babel-data "tbl") :rows) 3)
                   '(:date "2026-03-05" :category "Thu" :value 9300)))
    (should (equal (plist-get (eas-error-plist (should-error (eas-babel-data "missing.csv")
                                                               :type 'eas-not-found))
                              :file)
                   "missing.csv"))
    (let ((err (should-error (eas-babel-data "nope") :type 'eas-not-found)))
      (should (equal (plist-get (eas-error-plist err) :table) "nope")))))

(ert-deftest eas-babel-bindings-take-slots-from-headers-var-and-body ()
  (let* ((template (eas-template-get "line"))
         (b (eas-babel-bindings template
                                  '((:points . "true") (:title . "Daily") (:results . "verbatim")
                                    (:var . ("data" . (("date" "value") hline ("2026-03-02" 1) ("2026-03-03" 2)))))
                                  "{\"y\": \"value\"}")))
    (should (eq (plist-get b :points) t))
    (should (equal (plist-get b :title) "Daily"))
    (should (equal (plist-get b :y) "value"))
    (should (equal (plist-get b :data) [(:date "2026-03-02" :value 1) (:date "2026-03-03" :value 2)]))
    (should-not (plist-member b :results)))
  ;; The data slot is the first required shape slot, whatever its name.
  (should (eq (eas-babel-data-slot
               '(:name "t" :meta (:slots (:title (:type "string") :rows (:shape "plist")
                                          :bars (:shape "bar/v1" :required t)))))
              :bars))
  (should-error (eas-babel-bindings (eas-template-get "line") nil "[1, 2]") :type 'eas-invalid-input)
  (should-error (eas-babel-bindings (eas-template-get "line") nil "{oops") :type 'eas-parse-error))

(ert-deftest eas-babel-block-errors-name-their-fix ()
  (should-error (org-babel-execute:eas "" '((:as . "text"))) :type 'eas-invalid-input)
  (should-error (org-babel-execute:eas "" '((:template . "nope"))) :type 'eas-not-found)
  (should-error (org-babel-execute:eas "" '((:template . "line") (:as . "pdf"))) :type 'eas-invalid-input)
  (should-error (org-babel-execute:eas "" '((:template . "line") (:as . "text"))) :type 'eas-slot-missing)
  (let ((err (should-error (org-babel-execute:eas "" '((:template . "line") (:file . "x.svg")
                                                         (:result-params "verbatim")))
                           :type 'eas-invalid-input)))
    (should (string-match-p ":results file" (cadr err)))))

;;; Outputs

(ert-deftest eas-babel-text-result-is-the-deterministic-text-chart ()
  (eas-babel-test--with-org " :as text" ""
    (let ((result (eas-babel-test--execute)))
      (should (string-match-p "Steps" result))
      (should (equal (eas-view-ids) nil))
      (eas-test-golden "babel-bars.txt" result)
      (should (search-forward "#+RESULTS: steps" nil t)))))

(ert-deftest eas-babel-vl-result-is-the-resolved-vega-lite ()
  (eas-babel-test--with-org " :as vl" ""
    (let* ((result (eas-babel-test--execute)) (spec (eas-json-parse result)))
      (should-not (string-match-p "x-eas" result))
      (should (equal (length (plist-get (plist-get spec :data) :values)) 4))
      (should (equal (plist-get spec :title) "Steps"))
      (should (equal (eas-resolve-hash spec)
                     (eas-resolve-hash
                      (eas-resolve "bars" (list :title "Steps"
                                                  :data (plist-get (eas-babel-data
                                                                    '(("date" "category" "value")
                                                                      ("2026-03-02" "Mon" 8200)
                                                                      ("2026-03-03" "Tue" 10400)
                                                                      ("2026-03-04" "Wed" 6100)
                                                                      ("2026-03-05" "Thu" 9300)))
                                                                   :rows)))))))))

(ert-deftest eas-babel-file-results-write-by-extension ()
  (let ((dir (make-temp-file "eas-babel" t)))
    (unwind-protect
        (dolist (case '(("c.svg" . "<svg") ("c.json" . "\"$schema\"") ("c.txt" . "Steps")))
          (let ((file (expand-file-name (car case) dir)))
            (eas-babel-test--with-org (format " :results file :file %s" file) ""
              (should (equal (eas-babel-test--execute) file))
              (should (search-forward (format "[[file:%s]]" file) nil t))
              (should (string-match-p (regexp-quote (cdr case))
                                      (with-temp-buffer (insert-file-contents file) (buffer-string)))))))
      (delete-directory dir t))))

(ert-deftest eas-babel-plain-spec-body-takes-data-rows ()
  (eas-babel-test--with-org
      "" ""
    (let ((result (org-babel-execute:eas
                   "{\"mark\": \"line\", \"width\": 200, \"height\": 100,
                     \"encoding\": {\"x\": {\"field\": \"date\", \"type\": \"temporal\"},
                                    \"y\": {\"field\": \"value\", \"type\": \"quantitative\"}}}"
                   '((:data . "tbl") (:as . "text") (:cols . 40) (:rows . 10)))))
      (should (stringp result))
      (should (string-match-p "value" result)))))

(ert-deftest eas-babel-export-yields-text-and-opens-no-view ()
  (eas-babel-test--with-org " :exports results" ""
    (let ((out (org-export-as 'ascii nil nil t)))
      (should (string-match-p "Steps" out))
      (should (string-match-p "category" out))
      (should-not (eas-view-ids))))
  (eas-babel-test--with-org " :exports results" ""
    (let* ((eas-babel-export-as "vl")
           (out (org-export-as 'ascii nil nil t)))
      (should (string-match-p "vega-lite/v6" out))
      (should-not (eas-view-ids)))))

;;; The live view

(ert-deftest eas-babel-view-opens-inline-under-the-block-name ()
  (eas-babel-test--with-org "" ""
    (let ((result (eas-babel-test--execute)))
      (should (equal (eas-view-ids) '("bars:steps")))
      (let* ((view (eas-view-get "bars:steps"))
             (ov (plist-get (eas-babel-inline--entry view) :overlay)))
        (should (overlayp ov))
        (should (eq (overlay-get ov 'keymap) eas-babel-inline-map))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right result "\n+")))
        (should (equal (eas-babel-source-of view) (list :name "tbl" :buffer (current-buffer))))
        (should (equal (eas-action-for view '(:mark "main/0" :view "main")) "org-source-row"))))
    ;; Re-running replaces the view rather than adding a second one.
    (eas-babel-test--execute)
    (should (equal (eas-view-ids) '("bars:steps")))))

(ert-deftest eas-babel-click-on-a-bar-jumps-to-its-table-row ()
  (eas-babel-test--with-org "" ""
    (eas-babel-test--execute)
    (let* ((view (eas-view-get "bars:steps"))
           (inspect (eas-dispatch view (list :type "click" :px (eas-babel-test--item-px view "main/0" 2))))
           (click (plist-get inspect :click)))
      (should (equal (plist-get click :action) "org-source-row"))
      (should (eq (plist-get click :ran) t))
      (should (equal (plist-get click :result) "tbl:3"))
      (should (string-prefix-p "2026-03-04 |" (buffer-substring (point) (line-end-position))))
      ;; A replayed log records the click without jumping again.
      (goto-char (point-min))
      (eas-replay view (eas-view-log-entries view))
      (should (eq (plist-get (plist-get (eas-inspect view) :click) :ran) :false))
      (should (= (point) (point-min))))))

(ert-deftest eas-babel-terminal-ret-on-the-result-clicks-the-cell ()
  (eas-babel-test--with-org "" ""
    (eas-babel-test--execute)
    (let* ((view (eas-view-get "bars:steps"))
           (px (eas-babel-test--item-px view "main/0" 0))
           (pos (eas-babel-test--px-pos view px)))
      (goto-char pos)
      (let ((back (eas-babel-inline-pos-px pos)))
        (should (< (abs (- (aref back 0) (aref px 0))) 7))
        (should (< (abs (- (aref back 1) (aref px 1))) 14)))
      ;; Point motion hovers.
      (eas-babel-inline--post-command)
      (should (equal (plist-get (plist-get (plist-get (eas-inspect view) :hover) :row) :date) "2026-03-02"))
      (should (eq (lookup-key eas-babel-inline-map (kbd "RET")) #'eas-babel-inline-click-at-point))
      (eas-babel-inline-click-at-point)
      (should (equal (plist-get (plist-get (eas-inspect view) :click) :result) "tbl:1"))
      (should (string-prefix-p "2026-03-02 |" (buffer-substring (point) (line-end-position)))))))

(ert-deftest eas-babel-terminal-push-rewrites-the-result ()
  (eas-babel-test--with-org "" ""
    (let* ((before (eas-babel-test--execute))
           (view (eas-view-get "bars:steps")))
      (eas-push view [(:date "2026-03-06" :category "Fri" :value 12050)])
      (let ((ov (plist-get (eas-babel-inline--entry view) :overlay))
            (now (substring-no-properties (eas-text-render (eas-view-scene view)))))
        (should-not (equal (string-trim-right before "\n+") (string-trim-right now "\n+")))
        (should (string-match-p "Fri" now))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right now "\n+")))
        (should (search-forward "#+end_example" nil t))))))

(ert-deftest eas-babel-terminal-zoom-keys-on-a-plain-spec ()
  (eas-babel-test--with-org "" ""
    (goto-char (point-max))
    (insert "\n#+name: zoomable\n#+begin_src eas :data tbl :cols 50 :rows 12
{\"mark\": \"line\", \"width\": 300, \"height\": 120,
 \"params\": [{\"name\": \"grid\", \"select\": \"interval\", \"bind\": \"scales\"}],
 \"encoding\": {\"x\": {\"field\": \"date\", \"type\": \"temporal\"},
              \"y\": {\"field\": \"value\", \"type\": \"quantitative\"}}}
#+end_src\n")
    (re-search-backward "#\\+begin_src eas :data")
    (let ((before (eas-babel-test--execute))
          (view (eas-view-get "chart:zoomable")))
      (goto-char (overlay-start (plist-get (eas-babel-inline--entry view) :overlay)))
      (should (eq (lookup-key eas-babel-inline-map "+") #'eas-babel-inline-key))
      (let ((unread-command-events nil))
        (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () [?+])))
          (eas-babel-inline-key)))
      (should (eq (plist-get (aref (plist-get (eas-inspect view) :views) 0) :zoomed) t))
      (let ((ov (plist-get (eas-babel-inline--entry view) :overlay)))
        (should-not (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                           (string-trim-right before "\n+")))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right (substring-no-properties (eas-text-render (eas-view-scene view)))
                                          "\n+")))))))

(ert-deftest eas-babel-gui-overlay-shows-the-svg ()
  (eas-babel-test--with-org "" ""
    (let ((eas-babel-target 'svg))
      (eas-babel-test--execute)
      (let* ((view (eas-view-get "bars:steps"))
             (ov (plist-get (eas-babel-inline--entry view) :overlay))
             (image (overlay-get ov 'display)))
        (should (eq (car image) 'image))
        (should (string-prefix-p "<svg" (plist-get (cdr image) :data)))
        (should track-mouse)
        (let ((area (nth 1 (car (plist-get (cdr image) :map)))))
          (should area)
          (should (eq (lookup-key (overlay-get ov 'keymap) (vector area 'mouse-1)) #'eas-babel-inline-up)))
        (eas-push view [(:date "2026-03-06" :category "Fri" :value 12050)])
        (should-not (eq (overlay-get ov 'display) image))
        (should (string-prefix-p "<svg" (plist-get (cdr (overlay-get ov 'display)) :data)))))))

(ert-deftest eas-babel-open-shows-a-second-view-that-still-jumps ()
  (eas-babel-test--with-org "" ""
    (eas-babel-test--execute)
    (let ((org (current-buffer)) (eas-babel-target 'text))
      (goto-char (overlay-start (plist-get (eas-babel-inline--entry "bars:steps") :overlay)))
      (save-window-excursion
        (eas-babel-inline-open)
        (should (equal (eas-view-ids) '("bars:steps" "bars:steps/full")))
        (should (equal (eas-babel-source-of "bars:steps/full") (list :name "tbl" :buffer org)))
        (kill-buffer (eas-view-buffer (eas-view-get "bars:steps/full")))))))

(provide 'eas-babel-test)
;;; eas-babel-test.el ends here
