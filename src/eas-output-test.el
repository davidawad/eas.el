;;; eas-output-test.el --- tests for output modes (eas-rhy) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(declare-function eas-raster-module-encode "eas-geo-module")

(defconst eas-output-test--line
  '(:data (:values [(:x 1 :y 3) (:x 2 :y 5) (:x 3 :y 4) (:x 4 :y 8) (:x 5 :y 6)])
    :width 200 :height 100
    :layer [(:mark "line" :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")))
            (:params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "point"
             :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                        :opacity (:condition (:param "hover" :empty :false :value 1) :value 0.2)))])
  "A small line with a nearest-x hover.")

(defun eas-output-test--caps (&rest props)
  "Terminal capabilities: an xterm with the module, PROPS overriding."
  (let ((caps (list :gui nil :svg nil :term "xterm-256color" :term-program nil
                    :kitty-window-id nil :tmux nil :rasterizer 'module :cell '(10 . 20))))
    (while props
      (setq caps (plist-put caps (pop props) (pop props))))
    caps))

(defconst eas-output-test--kitty (eas-output-test--caps :term "xterm-kitty" :term-program nil)
  "A Kitty terminal.")

(defconst eas-output-test--gui '(:gui t :svg t :rasterizer module)
  "A graphical frame.")

(defmacro eas-output-test--with (caps &rest body)
  "Run BODY with capabilities CAPS stubbed and terminal writes captured.
`written' is bound to a list of the strings written, newest first, and
`encoded' to the encode calls."
  (declare (indent 1))
  `(let* ((written nil) (encoded nil)
          (eas-views (make-hash-table :test 'equal))
          (eas-output--surfaces (make-hash-table :test 'eq))
          (eas-output-capabilities-function (lambda (_w _l) ,caps))
          (eas-output-write-function (lambda (s _caps) (push s written)))
          (eas-action-inhibit t) (inhibit-message t))
     (ignore written encoded)
     (cl-letf (((symbol-function 'eas-output-encode)
                (lambda (_svg w h mode &optional id _bg)
                  (push (list w h mode id) encoded)
                  (format "<%s %d %dx%d>" mode id w h))))
       (unwind-protect (progn ,@body)
         (maphash (lambda (b _) (when (buffer-live-p b) (kill-buffer b))) eas-output--surfaces)))))

(defun eas-output-test--resolve (value caps)
  "The mode VALUE resolves to under CAPS."
  (plist-get (eas-output-resolve value :caps caps :id "t") :mode))

;;; Values

(ert-deftest eas-output-normalize-takes-symbols-strings-lists-and-vectors ()
  (should (equal (eas-output-normalize 'kitty) '(kitty)))
  (should (equal (eas-output-normalize "Sixel") '(sixel)))
  (should (equal (eas-output-normalize ["kitty" "sixel" "text"]) '(kitty sixel text)))
  (should (equal (eas-output-normalize '(auto)) '(auto))))

(ert-deftest eas-output-unknown-values-are-invalid-user-errors ()
  (dolist (bad (list 'png "iterm" ["kitty" 3] [] '(kitty . sixel)))
    (let ((err (should-error (eas-output-normalize bad "x-eas.output of the spec") :type 'eas-output-invalid)))
      (should (memq 'user-error (get (car err) 'error-conditions)))
      (should (equal (plist-get (eas-error-plist err) :code) "OUTPUT_INVALID"))
      (should (string-match-p "x-eas.output of the spec" (plist-get (eas-error-plist err) :message))))))

;;; Resolution

(ert-deftest eas-output-default-keeps-svg-in-gui-and-text-in-terminals ()
  (should (eq (eas-output-test--resolve (default-value 'eas-output) eas-output-test--gui) 'svg))
  (should (eq (eas-output-test--resolve (default-value 'eas-output) (eas-output-test--caps)) 'text))
  (should (eq (eas-output-test--resolve (default-value 'eas-output) eas-output-test--kitty) 'text))
  (should (eq (eas-output-test--resolve (default-value 'eas-output) '(:gui t :svg nil)) 'text)))

(ert-deftest eas-output-auto-picks-per-window ()
  (should (eq (eas-output-test--resolve 'auto eas-output-test--gui) 'svg))
  (should (eq (eas-output-test--resolve 'auto eas-output-test--kitty) 'kitty))
  (dolist (caps (list (eas-output-test--caps :term-program "WezTerm")
                      (eas-output-test--caps :term-program "ghostty")
                      (eas-output-test--caps :term "xterm-ghostty")
                      (eas-output-test--caps :kitty-window-id "1")
                      (eas-output-test--caps :kitty-reply t)))
    (should (eq (eas-output-test--resolve 'auto caps) 'kitty)))
  (should (eq (eas-output-test--resolve 'auto (eas-output-test--caps :da1 '(62 4 22))) 'sixel))
  (should (eq (eas-output-test--resolve 'auto (eas-output-test--caps)) 'text))
  ;; Without a rasterizer there are no pixels: auto falls to text.
  (should (eq (eas-output-test--resolve 'auto (plist-put (copy-sequence eas-output-test--kitty) :rasterizer nil))
              'text)))

(ert-deftest eas-output-preference-lists-take-the-first-mode-that-shows ()
  (should (eq (eas-output-test--resolve ["kitty" "sixel" "text"] (eas-output-test--caps)) 'text))
  (should (eq (eas-output-test--resolve '(kitty sixel text) (eas-output-test--caps :da1 '(4))) 'sixel))
  (should (eq (eas-output-test--resolve '(sixel kitty) eas-output-test--kitty) 'kitty))
  (should (eq (eas-output-test--resolve '(svg auto) eas-output-test--kitty) 'kitty))
  (let ((r (eas-output-resolve '(kitty sixel text) :caps (eas-output-test--caps) :id "t")))
    (should (equal (mapcar #'car (plist-get r :skipped)) '(kitty sixel)))))

(defun eas-output-test--error (value caps)
  "The plist of the unsupported error VALUE raises under CAPS."
  (let ((err (should-error (eas-output-resolve value :caps caps :id "line:t" :source "eas-output (global)")
                           :type 'eas-output-unsupported)))
    (should (memq 'user-error (get (car err) 'error-conditions)))
    (eas-error-plist err)))

(ert-deftest eas-output-strict-errors-name-chart-mode-source-facts-reason-and-fix ()
  (let ((p (eas-output-test--error 'kitty (eas-output-test--caps))))
    (should (equal (plist-get p :code) "OUTPUT_UNSUPPORTED"))
    (should (equal (plist-get p :view) "line:t"))
    (should (equal (plist-get p :mode) "kitty"))
    (should (equal (plist-get p :source) "eas-output (global)"))
    (should (string-match-p "TERM=xterm-256color" (plist-get p :facts)))
    (should (string-match-p "tmux: no" (plist-get p :facts)))
    (let ((r (car (plist-get p :reasons))))
      (should (string-match-p "does not speak the Kitty graphics protocol" (plist-get r :reason)))
      (should (string-match-p "WezTerm" (plist-get r :fix))))
    (should (string-match-p "line:t" (plist-get p :message)))))

(ert-deftest eas-output-every-strict-error-has-its-reason ()
  (dolist (case `((kitty ,eas-output-test--gui "graphical frame" "svg")
                  (sixel ,eas-output-test--gui "graphical frame" "svg")
                  (svg ,(eas-output-test--caps) "need a graphical frame" "auto, kitty or sixel")
                  (svg (:gui t :svg nil) "cannot display SVG" "librsvg")
                  (sixel ,(eas-output-test--caps) "does not report sixel" "foot")
                  (kitty ,(eas-output-test--caps :term "xterm-kitty" :rasterizer nil)
                         "raster feature" "make module FEATURES=raster")
                  (kitty ,(eas-output-test--caps :term "tmux-256color" :tmux '(:env "/s,1,0" :pane "%1" :passthrough "off"
                                                                              :client-term "xterm-kitty"))
                         "allow-passthrough is off" "tmux set -g allow-passthrough on")))
    (pcase-let ((`(,mode ,caps ,reason ,fix) case))
      (let ((r (car (plist-get (eas-output-test--error mode caps) :reasons))))
        (should (string-match-p reason (plist-get r :reason)))
        (should (string-match-p (regexp-quote fix) (plist-get r :fix)))))))

(ert-deftest eas-output-lists-not-ending-in-text-error-and-name-every-mode ()
  (let ((p (eas-output-test--error '(kitty sixel) (eas-output-test--caps))))
    (should (equal (plist-get p :mode) "kitty sixel"))
    (should (equal (mapcar (lambda (r) (plist-get r :mode)) (plist-get p :reasons)) '("kitty" "sixel")))))

(ert-deftest eas-output-auto-and-lists-ending-in-text-never-error ()
  (dolist (caps (list eas-output-test--gui (eas-output-test--caps) '(:gui t :svg nil)
                      (eas-output-test--caps :rasterizer nil :term "xterm-kitty")))
    (should (eas-output-test--resolve 'auto caps))
    (should (eas-output-test--resolve '(kitty sixel svg text) caps))
    (should (eas-output-test--resolve '(kitty auto) caps))))

;;; tmux

(ert-deftest eas-output-tmux-counts-only-when-the-tty-is-a-pane ()
  (let ((process-environment (cons "TMUX=/tmp/tmux-1/default,123,0" process-environment))
        (calls nil))
    (cl-letf (((symbol-function 'eas-output--call)
               (lambda (_program &rest args)
                 (push args calls)
                 (pcase (nth 2 args)
                   ("list-panes" '("/dev/pts/9\t%3" "/dev/pts/4\t%1"))
                   ("show-options" (if (member "-pqv" args) '("") '("on")))
                   ("display-message" (if (string-match-p "termname" (car (last args)))
                                          '("xterm-kitty") '("kitty 0.40")))))))
      (let ((pane (eas-output--tmux (selected-frame) "/dev/pts/4")))
        (should (equal (plist-get pane :pane) "%1"))
        (should (equal (plist-get pane :passthrough) "on"))
        (should (equal (plist-get pane :client-term) "xterm-kitty"))
        (should (equal (nth 1 (car (last calls))) "/tmp/tmux-1/default")))
      ;; A TMUX left in the environment of a tty that is no pane (seen live).
      (let ((stray (eas-output--tmux (selected-frame) "/dev/pts/7")))
        (should (plist-get stray :env))
        (should-not (plist-get stray :pane))
        (should-not (plist-get stray :passthrough))))))

(ert-deftest eas-output-a-stray-tmux-neither-blocks-nor-wraps ()
  (let ((stray (eas-output-test--caps :term "xterm-kitty" :tmux '(:env "/s,1,0" :pane nil)))
        (pane (eas-output-test--caps :term "tmux-256color"
                                     :tmux '(:env "/s,1,0" :pane "%1" :passthrough "all" :client-term "xterm-kitty"))))
    (should (eq (eas-output-test--resolve 'kitty stray) 'kitty))
    (should (equal (eas-output-wrap "\e_Gx\e\\" stray) "\e_Gx\e\\"))
    (should (string-match-p "not a pane" (eas-output-facts stray)))
    (should (eq (eas-output-test--resolve 'kitty pane) 'kitty))
    (should (equal (eas-output-wrap "\e_Gx\e\\" pane) "\ePtmux;\e\e_Gx\e\e\\\e\\"))
    (should (string-match-p "allow-passthrough all" (eas-output-facts pane)))))

;;; Terminal replies and encoders

(ert-deftest eas-output-parses-query-replies ()
  (let ((r (eas-output-parse-replies "\e_Gi=31;OK\e\\\e[6;20;10t\e[4;480;800t\e[?62;4;22c")))
    (should (eq (plist-get r :kitty) t))
    (should (equal (plist-get r :cell) '(10 . 20)))
    (should (equal (plist-get r :text) '(800 . 480)))
    (should (equal (plist-get r :da1) '(62 4 22))))
  (let ((r (eas-output-parse-replies "\e[?1;2c")))
    (should-not (plist-get r :kitty))
    (should (equal (plist-get r :da1) '(1 2))))
  (should (eq (plist-get (eas-output-parse-replies "\e_Gi=31;ENOENT:x\e\\\e[?6c") :kitty) nil)))

;;; The query never leaks replies as keys (eas-o3c)

(defmacro eas-output-test--query (replies &rest body)
  "Run BODY with the terminal answering a query with REPLIES.
REPLIES are queued as raw input when the query is sent; `sent' is
bound to the strings sent."
  (declare (indent 1))
  `(let ((sent nil) (unread-command-events nil) (eas-output-query-drain 0.2))
     (ignore sent)
     (cl-letf (((symbol-function 'send-string-to-terminal)
                (lambda (s &optional _terminal)
                  (push s sent)
                  (setq unread-command-events (append unread-command-events ,replies)))))
       ,@body)))

(ert-deftest eas-output-query-consumes-every-reply ()
  (let ((ran nil))
    (eas-output-test--query
        (append (string-to-list "\e_Gi=31;OK\e\\\e[6;28;14t\e[4;1848;3024t\e[?62;4;22c")
                '((mouse-movement)))
      (let ((pre-command-hook (list (lambda () (setq ran t))))
            (r (eas-output--query (selected-frame) (eas-output-test--caps))))
        (should (eq (plist-get r :kitty) t))
        (should (equal (plist-get r :cell) '(14 . 28)))
        (should (equal (plist-get r :da1) '(62 4 22)))
        ;; DA1 is the last thing asked: the sentinel.
        (should (string-suffix-p "\e[16t\e[14t\e[c" (car sent)))
        ;; Not one reply byte is left to run as a key; other events stay.
        (should (equal unread-command-events '((mouse-movement))))
        (should-not ran)))))

(ert-deftest eas-output-query-discards-a-partial-reply ()
  (eas-output-test--query (string-to-list "\e[6;28;14t\e[4;18")
    (let ((r (let ((eas-output-query-timeout 0.05))
               (eas-output--query (selected-frame) (eas-output-test--caps)))))
      (should-not (plist-get r :cell))
      (should-not unread-command-events))))

(ert-deftest eas-output-query-reads-on-to-a-late-sentinel ()
  ;; The first byte arrives in time, DA1 after the timeout: still read.
  (let* ((script (list nil ?\e ?\[ nil nil ?? ?6 ?c ?x))
         (clock 0.0) (unread-command-events nil) (eas-output-query-drain 1.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function 'read-event)
               (lambda (&rest _) (setq clock (+ clock 0.1)) (pop script))))
      (should (equal (eas-output--read-replies 0.15) "\e[?6c"))
      (should (equal script '(?x))))))

(ert-deftest eas-output-query-skipped-when-the-environment-says-kitty ()
  (dolist (env '((:term-program "WezTerm") (:term-program "ghostty") (:term-program "kitty")
                 (:kitty-window-id "1") (:term "xterm-kitty")))
    (let ((caps (apply #'eas-output-test--caps :da1 nil :kitty-reply nil env)))
      (should (eq (eas-output--query-blocker caps) 'env))))
  ;; WezTerm without a query still shows kitty, and sixel when asked.
  (let ((wez (eas-output-test--caps :term-program "WezTerm" :da1 nil)))
    (should (eq (eas-output-test--resolve 'auto wez) 'kitty))
    (should (eq (eas-output-test--resolve 'sixel wez) 'sixel)))
  (let ((noninteractive nil) (eas-output-query-terminal t))
    (should-not (eas-output--query-blocker (eas-output-test--caps :da1 nil)))
    (let ((eas-output--querying t))
      (should (eq (eas-output--query-blocker (eas-output-test--caps)) 'nested)))))

(ert-deftest eas-output-detect-sends-nothing-under-wezterm ()
  (let ((eas-output--cache (make-hash-table :test 'eq :weakness 'key))
        (noninteractive nil) (eas-output-query-terminal t) (queried nil) (sent nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'getenv)
               (lambda (var &optional _frame)
                 (cdr (assoc var '(("TERM" . "xterm-256color") ("TERM_PROGRAM" . "WezTerm"))))))
              ((symbol-function 'eas-output--frame-tty) (lambda (_f) "/dev/pts/9"))
              ((symbol-function 'eas-output--query) (lambda (&rest _) (setq queried t) nil))
              ((symbol-function 'send-string-to-terminal) (lambda (&rest a) (push a sent))))
      (let ((caps (eas-output-detect nil 'query)))
        (should-not queried)
        (should-not sent)
        (should (eq (plist-get caps :blocker) 'env))
        (should (eas-output-kitty-p caps))))))

(ert-deftest eas-output-detect-never-queries-from-a-timer ()
  (let ((eas-output--cache (make-hash-table :test 'eq :weakness 'key))
        (noninteractive nil) (eas-output-query-terminal t) (queried 0) (caps nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'getenv)
               (lambda (var &optional _frame) (and (equal var "TERM") "xterm-256color")))
              ((symbol-function 'eas-output--frame-tty) (lambda (_f) "/dev/pts/9"))
              ((symbol-function 'eas-output--query) (lambda (&rest _) (cl-incf queried) nil)))
      (let ((timer (run-at-time 0 nil (lambda () (setq caps (eas-output-detect nil 'query))))))
        (while (memq timer timer-list) (accept-process-output nil 0.01)))
      (should (eq (plist-get caps :blocker) 'timer))
      (should (= queried 0))
      ;; Not cached: the next chart, run from a command, asks.
      (should (eq (plist-get (eas-output-detect nil 'query) :blocker) nil))
      (should (= queried 1)))))

(ert-deftest eas-output-kitty-transmit-chunks-like-the-module ()
  (should (equal (eas-output-kitty-transmit "f=100" 3 "x") "\e_Ga=t,f=100,i=3,q=2,m=0;eA==\e\\"))
  (let* ((payload (apply #'unibyte-string (number-sequence 0 255)))
         (big (apply #'concat (make-list 40 payload)))
         (seq (eas-output-kitty-transmit "f=100" 7 big))
         (chunks (split-string seq "\e_G" t)))
    (should (> (length chunks) 1))
    (should (string-prefix-p "a=t,f=100,i=7,q=2,m=1;" (car chunks)))
    (should (string-prefix-p "m=0;" (car (last chunks))))
    (should (equal (base64-decode-string
                    (mapconcat (lambda (c) (substring c (1+ (string-search ";" c)) -2)) chunks ""))
                   big))))

(ert-deftest eas-output-cell-size-falls-back ()
  (should (equal (eas-output-cell '(:cell (9 . 18))) '(9 . 18)))
  (should (equal (eas-output-cell '(:cell nil)) eas-output-default-cell-size))
  (let ((eas-output-cell-size '(8 . 16)))
    (should (equal (eas-output-cell '(:cell (9 . 18))) '(8 . 16)))))

;;; eas-show: strict errors, precedence, pixels

(ert-deftest eas-output-show-strict-error-writes-the-buffer-and-no-bytes ()
  (eas-output-test--with (eas-output-test--caps)
    (let* ((eas-output 'kitty)
           (view (eas-view-open eas-output-test--line :id "strict"))
           (err (should-error (eas-show view) :type 'eas-output-unsupported)))
      (should (memq 'user-error (get (car err) 'error-conditions)))
      (should (null written))
      (should (null encoded))
      (with-current-buffer "*eas strict*"
        (let ((text (buffer-string)))
          (should (string-match-p "chart strict cannot be drawn with output kitty" text))
          (should (string-match-p "from eas-output (global)" text))
          (should (string-match-p "TERM=xterm-256color" text))
          (should (string-match-p "fix: run Emacs in Kitty" text))
          (should (string-match-p "Nothing was sent" text)))
        (kill-buffer)))))

(ert-deftest eas-output-spec-beats-buffer-beats-global ()
  (eas-output-test--with (eas-output-test--caps)
    (let ((eas-output 'kitty))
      ;; The spec says text: it wins over the global kitty.
      (let ((view (eas-view-open (append '(:x-eas (:output "text")) eas-output-test--line) :id "spec")))
        (with-current-buffer (eas-show view)
          (should (eq eas-mode--output 'text))
          (kill-buffer)))
      ;; The calling buffer's local value wins over the global one.
      (with-temp-buffer
        (setq-local eas-output '(sixel text))
        (with-current-buffer (eas-show (eas-view-open eas-output-test--line :id "local"))
          (should (eq eas-mode--output 'text))
          (should (string-match-p "buffer-local eas-output in" eas-mode--output-source))
          (should (string-match-p "buffer-local" (cdr (eas-mode-output-setting))))
          (kill-buffer)))
      ;; The spec's strict kitty fails, saying where it came from.
      (let* ((eas-output 'text)
             (view (eas-view-open (append '(:x-eas (:output ["kitty"])) eas-output-test--line) :id "spec2"))
             (err (should-error (eas-show view) :type 'eas-output-unsupported)))
        (should (equal (plist-get (eas-error-plist err) :source) "x-eas.output of the spec"))
        (kill-buffer "*eas spec2*")))))

(ert-deftest eas-output-show-invalid-value-errors ()
  (eas-output-test--with (eas-output-test--caps)
    (let ((eas-output "png"))
      (should-error (eas-show (eas-view-open eas-output-test--line :id "bad")) :type 'eas-output-invalid)
      (with-current-buffer "*eas bad*"
        (should (string-match-p "not an output mode" (buffer-string)))
        (kill-buffer)))))

(ert-deftest eas-output-explicit-target-bypasses-output ()
  (eas-output-test--with (eas-output-test--caps)
    (let ((eas-output 'kitty))
      (with-current-buffer (eas-show (eas-view-open eas-output-test--line :id "tgt") 'text)
        (should (null eas-mode--output))
        (should (null written))
        (kill-buffer)))))

(defun eas-output-test--writes (written regexp)
  "The strings in WRITTEN matching REGEXP."
  (seq-filter (lambda (s) (string-match-p regexp s)) written))

(ert-deftest eas-output-kitty-places-over-blank-cells-and-resends-only-on-change ()
  (eas-output-test--with eas-output-test--kitty
    (let* ((eas-output 'kitty)
           (view (eas-view-open eas-output-test--line :id "px"))
           (buffer (eas-show view))
           (s (eas-output-surface buffer)))
      (with-current-buffer buffer
        (should (eq eas-mode--output 'kitty))
        (should (eq (eas-view-target view) 'svg))
        (pcase-let ((`(,cols . ,rows) eas-mode--pixel-cells))
          ;; The view is compiled at the cells' pixel size.
          (should (equal (eas-view-size view) (cons (* 10 cols) (* 20 rows))))
          ;; ROWS blank lines, then the strip.
          (goto-char (point-min))
          (dotimes (_ rows)
            (should (looking-at (concat (make-string cols ?\s) "\n")))
            (forward-line 1))
          (should (null written))
          (eas-output-sync)
          (should (equal (car encoded) (list (* 10 cols) (* 20 rows) 'kitty (eas-output-surface-image s))))
          (let ((out (car written)) (id (eas-output-surface-image s)))
            (should (string-match-p (format "<kitty %d " id) out))
            (should (string-match-p (format "\e_Ga=p,i=%d,p=[0-9]+,x=0,y=0,w=%d,h=%d,c=%d,r=%d,C=1,q=2"
                                            id (* 10 cols) (* 20 rows) cols rows)
                                    out))
            ;; The cursor is saved, moved to the window's first body cell
            ;; and restored around the placement.
            (pcase-let ((`(,left ,top . ,_) (window-body-edges (get-buffer-window buffer))))
              (should (string-match-p (format "\e7\e\\[%d;%dH\e_Ga=p" (1+ top) (1+ left)) out))
              (should (string-suffix-p "\e8" out))))
          ;; Nothing changed: nothing is sent.
          (setq written nil)
          (eas-output-sync)
          (should (null written))
          ;; A hover changes the picture: one new image, the old one deleted.
          (let ((old (eas-output-surface-image s)) (blank (buffer-substring 1 (* (1+ cols) rows))))
            (eas-tty-next-datum)
            (eas-mode-redraw)
            (should (equal (buffer-substring 1 (* (1+ cols) rows)) blank))
            (eas-output-sync)
            (should (= (length encoded) 2))
            (should (string-match-p (format "\e_Ga=d,d=I,i=%d," old) (car written)))
            (should (/= old (eas-output-surface-image s)))))))))

(ert-deftest eas-output-kitty-deletes-when-the-window-or-buffer-goes ()
  (eas-output-test--with eas-output-test--kitty
    (let* ((eas-output 'kitty)
           (buffer (eas-show (eas-view-open eas-output-test--line :id "gone")))
           (s (eas-output-surface buffer))
           (window (get-buffer-window buffer)))
      (eas-output-sync)
      (should (assq window (eas-output-surface-placements s)))
      (let ((id (eas-output-surface-image s)))
        ;; Another buffer in the window: its placement is deleted.
        (setq written nil)
        (set-window-buffer window (get-buffer-create "*eas-output-other*"))
        (eas-output-sync)
        (should (eas-output-test--writes written (format "\e_Ga=d,d=i,i=%d,p=" id)))
        (should (null (eas-output-surface-placements s)))
        ;; Shown again: placed again, not sent again.
        (setq written nil)
        (set-window-buffer window buffer)
        (eas-output-sync)
        (should (= (length encoded) 1))
        (should (eas-output-test--writes written "a=p,"))
        ;; Killed: the image and its data are deleted, the surface forgotten.
        (setq written nil)
        (kill-buffer buffer)
        (should (equal written (list (format "\e_Ga=d,d=I,i=%d,q=2\e\\" id))))
        (should (null (eas-output-surface buffer)))
        (kill-buffer "*eas-output-other*")))))

(ert-deftest eas-output-placement-follows-scrolling ()
  (eas-output-test--with eas-output-test--kitty
    (let* ((eas-output 'kitty)
           (buffer (eas-show (eas-view-open eas-output-test--line :id "scroll")))
           (s (eas-output-surface buffer))
           (window (get-buffer-window buffer)))
      (eas-output-sync)
      (setq written nil)
      (with-current-buffer buffer
        (set-window-start window (save-excursion (goto-char (point-min)) (forward-line 3) (point)) t))
      (pcase-let ((`(,_top ,_left ,y0 ,rows) (eas-output-geometry s window)))
        (should (= y0 3))
        (should (= rows (min (- (eas-output-surface-rows s) 3) (window-body-height window)))))
      (eas-output-sync)
      ;; Re-placed with the source cropped by three cell rows, not re-sent.
      (should (= (length encoded) 1))
      (should (eas-output-test--writes written "a=p,.*,y=60,")))))

(ert-deftest eas-output-tmux-placements-count-from-the-screen ()
  ;; Passed-through escapes reach the outer terminal: a pane at row 5,
  ;; column 40 moves the cursor by that much.
  (eas-output-test--with (eas-output-test--caps :term "tmux-256color"
                                                :tmux '(:env "/s,1,0" :socket "/s" :pane "%2" :passthrough "on"
                                                        :client-term "xterm-kitty"))
    (cl-letf (((symbol-function 'eas-output--call)
               (lambda (&rest args) (and (member "#{pane_top} #{pane_left}" args) '("5 40")))))
      (let* ((eas-output 'kitty)
             (buffer (eas-show (eas-view-open eas-output-test--line :id "pane"))))
        (eas-output-sync)
        (pcase-let ((`(,left ,top . ,_) (window-body-edges (get-buffer-window buffer))))
          (should (string-match-p (format "\e\\[%d;%dH\e_Ga=p" (+ 6 top) (+ 41 left)) (car written))))))))

(ert-deftest eas-output-sixel-redraws-in-place ()
  (eas-output-test--with (eas-output-test--caps :da1 '(62 4))
    (let* ((eas-output '(kitty sixel text))
           (buffer (eas-show (eas-view-open eas-output-test--line :id "six")))
           (s (eas-output-surface buffer)))
      (should (eq (buffer-local-value 'eas-mode--output buffer) 'sixel))
      (eas-output-sync)
      (should (equal written (list (format "\e7\e[%d;1H<sixel %d %dx%d>\e8"
                                           (1+ (nth 1 (window-body-edges (get-buffer-window buffer))))
                                           (eas-output-surface-image s)
                                           (eas-output-surface-width s) (eas-output-surface-height s)))))
      (setq written nil)
      (eas-output-sync)
      (should (null written))
      (kill-buffer buffer))))

(ert-deftest eas-output-pixel-hover-maps-cells-as-text-does ()
  ;; n steps to the first datum and point lands on its cell, by the
  ;; terminal's cell size; the hover is the datum text mode would hover.
  (eas-output-test--with eas-output-test--kitty
    (let* ((eas-output 'kitty)
           (view (eas-view-open eas-output-test--line :id "hover"))
           (buffer (eas-show view)))
      (with-current-buffer buffer
        (eas-tty-next-datum)
        (let* ((hover (plist-get (eas-view-state view) :hover))
               (px (eas-mode-point-px)))
          (should (equal (plist-get (plist-get hover :row) :x) 1))
          ;; Point's cell holds the datum's pixel.
          (should (<= (abs (- (aref px 0) (plist-get hover :x))) 5))
          (should (<= (abs (- (aref px 1) (plist-get hover :y))) 10))
          (should (string-match-p "x" (format "%s" header-line-format))))
        (kill-buffer)))))

(ert-deftest eas-output-describe-reports-the-verdicts ()
  (eas-output-test--with (eas-output-test--caps :da1 '(4))
    (let ((eas-output 'kitty))
      (eas-output-describe)
      (with-current-buffer "*eas-output*"
        (let ((text (buffer-string)))
          (should (string-match-p "eas-output kitty (from eas-output (global))" text))
          (should (string-match-p "kitty  no: the terminal does not speak" text))
          (should (string-match-p "sixel  yes" text))
          (should (string-match-p "auto picks sixel" text))
          (should (string-match-p "This setting fails" text)))
        (kill-buffer)))))

;;; The rasterizer (needs the module built with its raster feature)

(defun eas-output-test--require-raster ()
  "Skip unless the native module has its raster feature."
  (unless (eq (let ((eas-output-rasterizer 'module)) (eas-output-rasterizer-find)) 'module)
    (eas-test-skip "the native module is not built with its raster feature (make module FEATURES=raster)")))

(defun eas-output-test--png-pixels (png)
  "(W H RGBA) of PNG bytes as the raster module writes them (filter 0)."
  (let ((pos 8) (w nil) (h nil) (idat nil))
    (while (< pos (length png))
      (let* ((len (seq-reduce (lambda (a b) (+ (* a 256) b)) (substring png pos (+ pos 4)) 0))
             (type (substring png (+ pos 4) (+ pos 8)))
             (data (substring png (+ pos 8) (+ pos 8 len))))
        (pcase type
          ("IHDR" (setq w (seq-reduce (lambda (a b) (+ (* a 256) b)) (substring data 0 4) 0)
                        h (seq-reduce (lambda (a b) (+ (* a 256) b)) (substring data 4 8) 0)))
          ("IDAT" (push data idat)))
        (setq pos (+ pos 12 len))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (apply #'concat (nreverse idat)))
      (should (zlib-decompress-region (point-min) (point-max)))
      (let ((raw (buffer-string)) (out (make-string (* w h 4) 0)))
        (dotimes (y h)
          (should (= (aref raw (* y (1+ (* 4 w)))) 0))
          (let ((from (1+ (* y (1+ (* 4 w))))))
            (dotimes (i (* 4 w)) (aset out (+ (* y 4 w) i) (aref raw (+ from i))))))
        (list w h out)))))

(defun eas-output-test--png-golden (name png)
  "Compare PNG with golden test/eas/golden/output/NAME.png, allowing 2%.
Pixels differ when a channel moves more than 48: fonts and resvg
versions shift glyph edges, not the chart."
  (let ((file (eas-test-file "test/eas/golden/output" (concat name ".png"))))
    (if (or (getenv "EAS_UPDATE_GOLDEN") (not (file-exists-p file)))
        (progn
          (make-directory (file-name-directory file) t)
          (let ((coding-system-for-write 'binary)) (write-region png nil file nil 'silent))
          (unless (getenv "EAS_UPDATE_GOLDEN")
            (ert-fail (format "Golden %s was missing and has been written; review and rerun" file))))
      (pcase-let ((`(,w ,h ,a) (eas-output-test--png-pixels png))
                  (`(,gw ,gh ,b) (eas-output-test--png-pixels
                                  (with-temp-buffer
                                    (set-buffer-multibyte nil)
                                    (insert-file-contents-literally file)
                                    (buffer-string)))))
        (should (equal (list w h) (list gw gh)))
        (let ((bad 0))
          (dotimes (p (* w h))
            (when (cl-loop for c below 4 thereis (> (abs (- (aref a (+ (* 4 p) c)) (aref b (+ (* 4 p) c)))) 48))
              (cl-incf bad)))
          (should (< (/ bad (float (* w h))) 0.02)))))))

(ert-deftest eas-output-templates-match-their-png-goldens ()
  (eas-output-test--require-raster)
  (unless (zlib-available-p) (eas-test-skip "this Emacs has no zlib to read PNG goldens"))
  (let ((eas-views (make-hash-table :test 'equal)))
    (dolist (name '("line" "bars" "heatmap"))
      (let* ((view (eas-view-open name :bindings (eas-template-example name) :size '(480 . 240) :target 'svg))
             (svg (eas-svg-render (eas-view-scene view))))
        (eas-output-test--png-golden name (eas-output-png svg 480 240))))))

(ert-deftest eas-output-module-encodes-kitty-and-sixel ()
  (eas-output-test--require-raster)
  (let* ((svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\"><rect width=\"40\" height=\"20\" fill=\"#ff0000\"/></svg>")
         (kitty (eas-output-encode svg 40 20 'kitty 77))
         (sixel (eas-output-encode svg 40 20 'sixel nil "#000000")))
    (should (string-prefix-p "\e_Ga=t,f=32,o=z,s=40,v=20,i=77,q=2,m=0;" kitty))
    (should (string-suffix-p "\e\\" kitty))
    (should (string-prefix-p "\eP0;1;0q\"1;1;40;20#0;2;100;0;0" sixel))
    (should (string-suffix-p "\e\\" sixel))
    (should-error (eas-raster-module-encode "<nope" 10 10 "kitty"))))

(provide 'eas-output-test)
;;; eas-output-test.el ends here
