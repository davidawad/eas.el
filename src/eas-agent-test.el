;;; eas-agent-test.el --- tests for the agent surface (fc-qx1.10) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas-agent)
(require 'eas-agent-cli)

(defconst eas-agent-test--brush-spec
  "{\"data\":{\"values\":[{\"x\":1,\"y\":3,\"c\":\"u\"},{\"x\":2,\"y\":5,\"c\":\"v\"},{\"x\":3,\"y\":4,\"c\":\"u\"},
    {\"x\":4,\"y\":8,\"c\":\"v\"},{\"x\":5,\"y\":6,\"c\":\"u\"}]},
  \"width\":200,\"height\":100,
  \"params\":[{\"name\":\"brush\",\"select\":{\"type\":\"interval\",\"encodings\":[\"x\"]}}],
  \"mark\":\"point\",
  \"encoding\":{\"x\":{\"field\":\"x\",\"type\":\"quantitative\"},\"y\":{\"field\":\"y\",\"type\":\"quantitative\"},
              \"color\":{\"field\":\"c\",\"type\":\"nominal\"}}}"
  "Points with an x brush, as JSON (what an agent sends).")

(defconst eas-agent-test--unsupported-spec
  "{\"mark\":\"geoshape\",\"data\":{\"values\":[{\"a\":1}]},\"encoding\":{\"latitude\":{\"field\":\"a\",\"type\":\"quantitative\"}}}"
  "A spec outside the native subset.")

(defun eas-agent-test--bindings ()
  "The line template's example bindings."
  (eas-template-example "line"))

(defmacro eas-agent-test--with-views (&rest body)
  "Run BODY with a fresh view registry holding view \"t\" (the brush spec)."
  `(let ((eas-views (make-hash-table :test 'equal)))
     (eas-agent "open" eas-agent-test--brush-spec :id "t" :backend "text")
     ,@body))

(defun eas-agent-test--shape (env)
  "Assert ENV is a chart/v1 envelope that encodes as JSON; return ENV."
  (should (equal (plist-get env :contract) "chart/v1"))
  (should (memq (plist-get env :ok) '(t :false)))
  (should (plist-member env :data))
  (should (vectorp (plist-get env :next)))
  (should (seq-every-p #'stringp (plist-get env :next)))
  (let ((back (eas-json-parse (eas-json-encode env))))
    (should (equal (plist-get back :contract) "chart/v1")))
  env)

(defun eas-agent-test--ok (env)
  "Assert ENV is a successful envelope; return its data."
  (eas-agent-test--shape env)
  (unless (eq (plist-get env :ok) t)
    (ert-fail (list "expected ok" env)))
  (should-not (plist-member env :reason))
  (plist-get env :data))

(defun eas-agent-test--fail (env &optional code)
  "Assert ENV failed (with reason CODE) and carries next[]; return evidence."
  (eas-agent-test--shape env)
  (should (eq (plist-get env :ok) :false))
  (should (assoc (plist-get env :reason) eas-reason-codes))
  (when code (should (equal (plist-get env :reason) code)))
  (should (stringp (plist-get (plist-get env :evidence) :message)))
  (should (> (length (plist-get env :next)) 0))
  (plist-get env :evidence))

(defconst eas-agent-test--cases
  `(("describe" ("verbs") ("nope"))
    ("example" ("line") ("nope"))
    ("check" ("line" :data ,(lambda () (eas-agent-test--bindings))) ("line"))
    ("explain" ("line" :data ,(lambda () (eas-agent-test--bindings)) :stage "scene")
     ("line" :data ,(lambda () (eas-agent-test--bindings)) :stage "pixels"))
    ("render" ("line" :data ,(lambda () (eas-agent-test--bindings)) :backend "svg")
     (,eas-agent-test--unsupported-spec :backend "text"))
    ("export" ("line" :data ,(lambda () (eas-agent-test--bindings)) :vl t) ("{not json"))
    ("bench" ("line" :data ,(lambda () (eas-agent-test--bindings)) :n 1) (:points "0"))
    ("doctor" () ("extra" :bogus 1))
    ("views" () (:bogus 1))
    ("open" ("line" :data ,(lambda () (eas-agent-test--bindings)) :subject "s") ("line" :data "[1,2]"))
    ("close" ("t") ("nope"))
    ("inspect" ("t") ("nope"))
    ("dispatch" ("t" "{\"type\":\"key\",\"key\":\"+\"}") ("t" "{\"type\":\"teleport\"}"))
    ("log" ("t" :n 5) ("nope"))
    ("selection" ("t" :as "org") ("t" :as "xml"))
    ("link" ("t" "agent-test-bus") ("nope" "agent-test-bus"))
    ("unlink" ("t") ("t" :bus "no-such-bus"))
    ("buses" () (:bogus 1)))
  "VERB, success args and failure args; functions are called for values.")

(defun eas-agent-test--args (args)
  "ARGS with function values called."
  (mapcar (lambda (a) (if (functionp a) (funcall a) a)) args))

(ert-deftest eas-agent-every-verb-has-envelope-cases ()
  (should (equal (sort (mapcar #'car eas-agent-test--cases) #'string<) (eas-agent-verb-names))))

(ert-deftest eas-agent-every-verb-returns-the-envelope ()
  (dolist (case eas-agent-test--cases)
    (eas-agent-test--with-views
      (ert-info ((format "%s ok" (car case)))
        (eas-agent-test--ok (apply #'eas-agent (car case) (eas-agent-test--args (nth 1 case))))))
    (eas-agent-test--with-views
      (ert-info ((format "%s fail" (car case)))
        (eas-agent-test--fail (apply #'eas-agent (car case) (eas-agent-test--args (nth 2 case))))))))

(ert-deftest eas-agent-unknown-verbs-and-options-fail-with-next ()
  (should (string-match-p "verbs: bench" (plist-get (eas-agent-test--fail (eas-agent "draw") "INVALID_INPUT")
                                                   :message)))
  (should (equal (plist-get (eas-agent-test--fail (eas-agent "render" "line" :colour "red")) :option) "colour"))
  (should (member "bin/eas describe verbs" (append (plist-get (eas-agent 'nope) :next) nil))))

(ert-deftest eas-agent-describe-adds-verbs-events-and-reasons ()
  (let ((all (eas-agent-test--ok (eas-agent "describe"))))
    (should (plist-get all :templates))
    (should (equal (plist-get all :contract) "chart/v1"))
    (should (seq-find (lambda (v) (equal (plist-get v :name) "dispatch")) (plist-get all :verbs)))
    (should (equal (length (plist-get all :reasons)) (length eas-reason-codes))))
  (should (equal (eas-plist-keys (eas-agent-test--ok (eas-agent "describe" "events"))) '(:events)))
  (should (equal (eas-plist-keys (eas-agent-test--ok (eas-agent 'describe 'adapters))) '(:adapters))))

(ert-deftest eas-agent-example-renders-as-is ()
  (let ((bindings (eas-agent-test--ok (eas-agent "example" "line"))))
    (eas-agent-test--ok (eas-agent "check" "line" :data bindings))
    (should (string-match-p "Daily value" (plist-get (eas-agent-test--ok (eas-agent "render" "line" :data bindings))
                                                     :output)))))

(ert-deftest eas-agent-check-names-code-path-and-next ()
  (let ((ev (eas-agent-test--fail
             (eas-agent "check" "{\"mark\":\"bar\",\"encoding\":{\"x\":{\"field\":\"a\",\"type\":\"bogus\"}}}")
             "INVALID_INPUT")))
    (should (equal (plist-get ev :path) "/encoding/x/type")))
  (let ((env (eas-agent "check" "line" :data '(:data [(:date "2026-01-01" :value 1)] :y "price"))))
    (eas-agent-test--fail env "FIELD_MISSING")
    (should (member "bin/eas example line" (append (plist-get env :next) nil))))
  (let ((env (eas-agent "check" "line" :data '(:data [(:date "2026-01-01" :value "x")]))))
    (eas-agent-test--shape env))
  ;; Unsupported features still open (a static view): a warning, not a failure.
  (let* ((env (eas-agent "check" eas-agent-test--unsupported-spec))
         (data (eas-agent-test--ok env)))
    (should (eq (plist-get data :native) :false))
    (should (equal (plist-get (aref (plist-get data :warnings) 0) :path) "/mark"))
    (should (seq-find (lambda (c) (string-match-p "export .* --vl" c)) (plist-get env :next)))))

(ert-deftest eas-agent-render-text-is-the-engine-text-deterministically ()
  (let* ((bindings (eas-agent-test--bindings))
         (expected (substring-no-properties
                    (eas-text-render (eas-compile (eas-resolve "line" bindings) :target 'text
                                                      :size '(:cols 50 :rows 12)))))
         (a (eas-agent "render" "line" :data bindings :cols 50 :rows 12))
         (b (eas-agent "render" "line" :data (eas-json-encode bindings) :cols "50" :rows "12")))
    (should (equal (plist-get (eas-agent-test--ok a) :output) expected))
    (should (equal (plist-get (eas-agent-test--ok b) :output) expected)))
  (should (string-prefix-p "<svg" (plist-get (eas-agent-test--ok
                                              (eas-agent "render" eas-agent-test--brush-spec :backend "svg"))
                                             :output)))
  (eas-agent-test--fail (eas-agent "render" "line" :data (eas-agent-test--bindings) :cols 50) "INVALID_INPUT")
  (eas-agent-test--fail (eas-agent "render" eas-agent-test--brush-spec :data "{}") "INVALID_INPUT"))

(ert-deftest eas-agent-explain-stages-and-export ()
  (let* ((bindings (eas-agent-test--bindings))
         (resolved (eas-resolve "line" bindings)))
    (should (equal (plist-get (eas-agent-test--ok (eas-agent "explain" "line" :data bindings)) :artifact)
                   resolved))
    (let ((compiled (plist-get (eas-agent-test--ok (eas-agent "explain" "line" :data bindings :stage "compile"))
                               :artifact)))
      (should (plist-get (plist-get (aref (plist-get compiled :views) 0) :scales) :x)))
    (should (equal (plist-get (plist-get (eas-agent-test--ok (eas-agent "explain" "line" :data bindings
                                                                            :stage "scene"))
                                         :artifact)
                              :contract)
                   "scene/v1"))
    (let ((vl (eas-agent-test--ok (eas-agent "export" "line" :data bindings :vl t))))
      (should (equal vl resolved))
      (should-not (string-match-p "x-eas" (eas-json-encode vl))))))

(ert-deftest eas-agent-live-loop-open-dispatch-inspect-log-selection ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (should (equal (plist-get (eas-agent-test--ok (eas-agent "open" "line" :data (eas-agent-test--bindings)
                                                                 :subject "daily"))
                              :id)
                   "line:daily"))
    (should (equal (plist-get (aref (eas-agent-test--ok (eas-agent "views")) 0) :id) "line:daily"))
    (eas-agent "open" eas-agent-test--brush-spec :id "pts")
    (should (equal (plist-get (eas-agent-test--ok
                               (eas-agent "dispatch" "pts" "{\"type\":\"wheel\",\"px\":[100,50],\"delta\":-3}"))
                              :last-event)
                   "wheel at [100 50]"))
    (let ((after (eas-agent-test--ok (eas-agent "dispatch" "pts" '(:type "brush" :x [2 4])))))
      (should (string-prefix-p "brush x 2..4" (plist-get after :last-event))))
    (let ((sel (eas-agent-test--ok (eas-agent "selection" "pts"))))
      (should (equal (plist-get sel :n) 3))
      (should (equal (mapcar (lambda (r) (plist-get r :x)) (plist-get sel :rows)) '(2 3 4))))
    (should (string-prefix-p "| x | y | c |"
                             (plist-get (eas-agent-test--ok (eas-agent "selection" "pts" :as "org")) :org)))
    (let ((log (eas-agent-test--ok (eas-agent "log" "pts"))))
      (should (equal (mapcar (lambda (e) (plist-get e :type)) log) '("wheel" "brush")))
      ;; Replaying the log on a fresh view reproduces the selection.
      (eas-agent "open" eas-agent-test--brush-spec :id "replay")
      (eas-agent-test--ok (eas-agent "dispatch" "replay" (eas-json-encode
                                                              (vconcat (mapcar (lambda (e) (plist-get e :event)) log)))))
      (should (equal (plist-get (eas-agent-test--ok (eas-agent "selection" "replay")) :n) 3)))
    (eas-agent-test--ok (eas-agent "close" "pts"))
    (let ((env (eas-agent "inspect" "pts")))
      (eas-agent-test--fail env "VIEW_NOT_FOUND")
      (should (equal (aref (plist-get env :next) 0) "emacsclient --eval '(eas-agent-json \"views\")'")))
    (let ((env (eas-agent "dispatch" "replay" "{\"type\":\"key\",\"key\":\"q\"}")))
      (should (equal (plist-get (eas-agent-test--fail env "EVENT_INVALID") :field) "key"))
      (should (member "bin/eas describe events" (append (plist-get env :next) nil))))))

(ert-deftest eas-agent-json-is-the-envelope-as-json ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let ((env (eas-json-parse (eas-agent-json "views"))))
      (should (eq (plist-get env :ok) t))
      (should (equal (plist-get env :data) [])))
    (should (equal (plist-get (eas-json-parse (eas-agent-json "inspect" "x")) :reason) "VIEW_NOT_FOUND"))))

(ert-deftest eas-agent-doctor-rows-are-eager-and-typed ()
  (let ((rows (plist-get (eas-agent-test--ok (eas-agent "doctor")) :rows)))
    (should (seq-find (lambda (r) (equal (plist-get r :name) "template:line")) rows))
    (seq-doseq (r rows)
      (should (member (plist-get r :status) '("pass" "fail" "skip")))
      (should (stringp (plist-get r :detail))))))

(ert-deftest eas-agent-bench-measures-every-stage ()
  (let ((ms (plist-get (eas-agent-test--ok (eas-agent "bench" "line" :data (eas-agent-test--bindings) :n 2))
                       :ms)))
    (dolist (k '(:resolve :compile-svg :render-svg :compile-text :render-text :hover))
      (should (numberp (plist-get (plist-get ms k) :mean))))))

(ert-deftest eas-agent-cli-parses-shell-arguments ()
  (should (equal (eas-agent-cli-args "export" '("line" "--data" "b.json" "--vl" "--backend=text"))
                 '(("line" :data "b.json" :vl t :backend "text"))))
  (should (equal (cdr (eas-agent-cli-args "render" '("line" "--raw"))) t))
  (should (equal (car (eas-agent-cli-run '("check" "line"))) 1))
  (let ((out (eas-agent-cli-run '("inspect" "x"))))
    (should (equal (car out) 1))
    (should (string-match-p "emacsclient --eval" (cdr out))))
  (let ((out (eas-agent-cli-run (list "render" "line" "--data" (eas-test-file "examples/line.data.json")
                                        "--cols" "40" "--rows" "10" "--raw"))))
    (should (equal (car out) 0))
    (should (string-match-p "Daily value" (cdr out)))))

(ert-deftest eas-agent-bin-eas-runs-in-batch ()
  (let* ((process-environment (cons (concat "EMACS=" (expand-file-name invocation-name invocation-directory))
                                    process-environment))
         (run (lambda (input &rest args)
                (with-temp-buffer
                  (let ((exit (if input
                                  (progn (insert input)
                                         (apply #'call-process-region (point-min) (point-max)
                                                (eas-test-file "bin/eas") t t nil args))
                                (apply #'call-process (eas-test-file "bin/eas") nil t nil args))))
                    (cons exit (buffer-string)))))))
    (let ((ok (funcall run nil "describe" "verbs")))
      (should (equal (car ok) 0))
      (should (equal (plist-get (eas-json-parse (cdr ok)) :contract) "chart/v1")))
    (let* ((bad (funcall run nil "check" "line"))
           (env (eas-json-parse (cdr bad))))
      (should (equal (car bad) 1))
      (should (equal (plist-get env :reason) "SLOT_MISSING"))
      (should (> (length (plist-get env :next)) 0)))
    (let ((piped (funcall run eas-agent-test--brush-spec "render" "-" "--cols" "30" "--rows" "8" "--raw")))
      (should (equal (car piped) 0))
      (should (string-match-p "┤" (cdr piped))))))

(provide 'eas-agent-test)
;;; eas-agent-test.el ends here
