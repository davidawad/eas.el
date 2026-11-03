;;; eas-output.el --- output modes: svg, text, kitty and sixel per window -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  How a chart reaches the screen, chosen per window and
;; declared, never guessed behind the user's back
;; (docs/design/output-modes.md):
;;
;;   svg    an SVG image, in graphical frames
;;   text   the text backend (eas-text), anywhere
;;   kitty  the SVG as pixels through the Kitty graphics protocol
;;          (Kitty, WezTerm, Ghostty; tmux with allow-passthrough)
;;   sixel  the SVG as pixels in DEC sixel (DA1 reports 4)
;;   auto   the best of these the window can show; never an error
;;
;; The option `eas-output' (global or buffer-local) and a spec's
;; x-eas.output (a string or an array) name a mode or a preference
;; list; precedence is spec > buffer > global.  A mode asked for
;; explicitly that the window cannot show signals
;; `eas-output-unsupported' (OUTPUT_UNSUPPORTED, a `user-error') before
;; a byte reaches the terminal, naming the chart, the mode and where it
;; was asked for, the terminal facts, the reason and the fix.  A list
;; ending in text or auto never errors.  M-x eas-output-describe says
;; what this window would do and why.
;;
;; Pixel modes rasterize the chart's SVG at the window's pixel size:
;; with the optional native module built with its raster feature
;; (resvg; `make module FEATURES=raster'), else rsvg-convert (kitty as
;; PNG; sixel with img2sixel).  The chart buffer keeps blank cells that
;; never change, so redisplay paints nothing over them; after redisplay
;; the image is placed over them (`eas-output-sync'): sent again only
;; when it changed, moved when the window scrolls or resizes, deleted
;; when the window or buffer goes away.
;;
;; Everything here is public (eas-output-*) so other packages can share
;; the detection, the strict errors and the placement bookkeeping.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eas-core)
(require 'eas-geo-native)

(defcustom eas-output '(svg text)
  "How eas charts reach the screen: a mode or a preference list of modes.
A mode is one of the symbols (or strings) `svg', `text', `kitty',
`sixel' and `auto':

  svg    an SVG image; needs a graphical frame
  text   text cells; works in every frame
  kitty  SVG as pixels via the Kitty graphics protocol (Kitty,
         WezTerm, Ghostty, tmux with allow-passthrough)
  sixel  SVG as pixels via DEC sixel
  auto   svg in graphical frames, else kitty, else sixel, else text

A list is tried in order and the first mode the window can show wins,
e.g. (kitty sixel text).  A mode, or a list, the window cannot show
signals `eas-output-unsupported' before anything is drawn; a list that
ends in `text' or `auto' never does.

The default, (svg text), is eas's behaviour before output modes:
images in graphical frames, text in terminals.  Set it to `auto' to
get pixels in terminals that can show them.  It may be set
buffer-locally; a spec's x-eas.output overrides both."
  :type '(choice (const :tag "Best available" auto)
                 (const :tag "SVG image (graphical frames)" svg)
                 (const :tag "Text" text)
                 (const :tag "Kitty graphics" kitty)
                 (const :tag "Sixel" sixel)
                 (repeat :tag "Preference list"
                         (choice (const auto) (const svg) (const text) (const kitty) (const sixel))))
  :group 'eas)

(defconst eas-output-modes '(svg text kitty sixel auto)
  "The values a mode in `eas-output' may take.")

(defcustom eas-output-rasterizer 'auto
  "What draws SVG as pixels for the kitty and sixel modes.
`auto' prefers the native module (built with its raster feature) and
falls back to rsvg-convert; `module' and `rsvg-convert' use only that."
  :type '(choice (const auto) (const module) (const rsvg-convert))
  :group 'eas)

(defcustom eas-output-cell-size nil
  "Pixel size of a terminal cell as (WIDTH . HEIGHT), or nil to ask.
nil asks the terminal (CSI 16t, else CSI 14t over the text area) and
falls back to `eas-output-default-cell-size'."
  :type '(choice (const :tag "Ask the terminal" nil) (cons integer integer))
  :group 'eas)

(defconst eas-output-default-cell-size '(10 . 20)
  "The cell size assumed when the terminal does not say.")

(defcustom eas-output-query-terminal t
  "Non-nil asks the terminal what it supports when a pixel mode is considered.
The query sends a Kitty graphics query, CSI 16t, CSI 14t and DA1, and
reads every reply up to DA1's (`eas-output-query-timeout').  It runs
once per terminal, only for `auto', `kitty' or `sixel', and never when
the environment says Kitty graphics, from a timer or with input pending."
  :type 'boolean
  :group 'eas)

(defcustom eas-output-query-timeout 0.4
  "Seconds to wait for the terminal's replies to the capability query."
  :type 'number
  :group 'eas)

(defconst eas-output-kitty-programs '("WezTerm" "ghostty" "kitty")
  "TERM_PROGRAM values of terminals that speak the Kitty graphics protocol.")

(defconst eas-output-kitty-terms "\\`xterm-\\(kitty\\|ghostty\\)\\|\\`wezterm"
  "TERM values of terminals that speak the Kitty graphics protocol.")

;;; Capabilities

(defvar eas-output-capabilities-function #'eas-output-detect
  "Function of WINDOW and LEVEL returning the capabilities plist.
See `eas-output-detect'.  Tests stub it.")

(defvar eas-output--cache (make-hash-table :test 'eq :weakness 'key)
  "Detected capabilities per terminal object.")

(defun eas-output-forget ()
  "Forget detected terminal capabilities; the next chart detects again."
  (interactive)
  (clrhash eas-output--cache))

(defconst eas-output-call-timeout 1.0
  "Seconds `eas-output--call' waits for a program (tmux, ps) to exit.
A hung tmux server must not hang Emacs: past this the program is killed.")

(defun eas-output--call (program &rest args)
  "Output lines of PROGRAM with ARGS, or nil when it fails.
PROGRAM is killed, and nil returned, after `eas-output-call-timeout'."
  (when (executable-find program)
    (with-temp-buffer
      (let* ((err (generate-new-buffer " *eas-output-stderr*"))
             (proc (ignore-errors
                     (make-process :name "eas-output" :buffer (current-buffer)
                                   :command (cons program args) :connection-type 'pipe
                                   :noquery t :stderr err :sentinel #'ignore)))
             (deadline (+ (float-time) eas-output-call-timeout)))
        (unwind-protect
            (when proc
              (ignore-errors (process-send-eof proc))
              (while (and (process-live-p proc) (< (float-time) deadline))
                (accept-process-output proc 0.05 nil t))
              (if (process-live-p proc)
                  (progn (delete-process proc) nil)
                (while (accept-process-output proc 0 nil t))
                (when (eql 0 (process-exit-status proc))
                  (split-string (buffer-string) "\n" t))))
          (when (process-live-p proc) (delete-process proc))
          (let ((p (get-buffer-process err))) (when p (delete-process p)))
          (kill-buffer err))))))

(defun eas-output--frame-tty (frame)
  "The tty device FRAME's terminal is on, e.g. \"/dev/pts/3\", or nil.
The initial terminal of Emacs -nw is named \"/dev/tty\"; then the
device comes from /proc or ps."
  (let ((name (ignore-errors (terminal-name (frame-terminal frame)))))
    (cond ((and (stringp name) (string-prefix-p "/dev/" name) (not (equal name "/dev/tty"))) name)
          ((file-exists-p "/proc/self/fd/0")
           (let ((true (file-truename "/proc/self/fd/0")))
             (and (string-prefix-p "/dev/" true) true)))
          (t (when-let* ((tty (car (eas-output--call "ps" "-o" "tty=" "-p" (number-to-string (emacs-pid)))))
                         ((not (string-match-p "\\`[ ?]*\\'" tty))))
               (concat "/dev/" (string-trim tty)))))))

(defun eas-output--tmux (frame tty)
  "The tmux facts of FRAME on TTY: a plist, or nil when not in tmux.
:pane is the pane whose #{pane_tty} is TTY (nil when TMUX is set but
TTY is no pane of that server: a stray TMUX must not count),
:passthrough its allow-passthrough, :client-term and :client-type the
outer terminal."
  (when-let* ((env (getenv "TMUX" frame)))
    (let* ((socket (car (split-string env ",")))
           (tmux (lambda (&rest args) (apply #'eas-output--call "tmux" "-S" socket args)))
           (pane (and tty (seq-some (lambda (line)
                                      (pcase (split-string line "\t")
                                        (`(,ptty ,id) (and (equal ptty tty) id))))
                                    (funcall tmux "list-panes" "-a" "-F" "#{pane_tty}\t#{pane_id}")))))
      (list :env env :pane pane :socket socket
            :passthrough (and pane
                              (let ((p (car (funcall tmux "show-options" "-pqv" "-t" pane "allow-passthrough")))
                                    (g (car (funcall tmux "show-options" "-gqv" "allow-passthrough"))))
                                (or (and p (not (string-empty-p p)) p) g "off")))
            :client-term (and pane (car (funcall tmux "display-message" "-p" "-t" pane "#{client_termname}")))
            :client-type (and pane (car (funcall tmux "display-message" "-p" "-t" pane "#{client_termtype}")))))))

(defun eas-output--pane-offset (caps)
  "The screen cell (TOP . LEFT) of the tmux pane CAPS run in, else (0 . 0).
Escapes passed through tmux reach the outer terminal, whose cursor
counts from the top of the screen, not of the pane.  Asked at every
sync: panes move."
  (let ((tmux (plist-get caps :tmux)))
    (or (when-let* ((pane (plist-get tmux :pane))
                    (line (car (eas-output--call "tmux" "-S" (plist-get tmux :socket) "display-message"
                                                 "-p" "-t" pane "#{pane_top} #{pane_left}"))))
          (pcase (mapcar #'string-to-number (split-string line))
            (`(,top ,left) (cons top left))))
        '(0 . 0))))

(defun eas-output--passthrough-p (tmux)
  "Non-nil when TMUX (from `eas-output--tmux') passes escapes through."
  (member (plist-get tmux :passthrough) '("on" "all")))

(defun eas-output-wrap (string caps)
  "STRING wrapped for tmux passthrough when CAPS put Emacs in a tmux pane."
  (if (plist-get (plist-get caps :tmux) :pane)
      (concat "\ePtmux;" (replace-regexp-in-string "\e" "\e\e" string t t) "\e\\")
    string))

(defconst eas-output-query-drain 2.0
  "Seconds to keep reading once the terminal began to answer the query.
A terminal that answered anything answers DA1 too; reading on to it
leaves no reply byte behind as input.")

(defvar eas-output--querying nil
  "Non-nil while the capability query reads the terminal's replies.")

(defun eas-output--in-timer-p ()
  "Non-nil when running from a timer, where a command may be reading keys."
  (catch 'found
    (mapbacktrace (lambda (_evald fn _args _flags)
                    (when (eq fn 'timer-event-handler) (throw 'found t))))
    nil))

(defun eas-output--in-command-p ()
  "Non-nil when running inside a command of the command loop.
Elsewhere (a timer, a process filter such as the server's, or an Emacs
whose timers do not show `timer-event-handler') the query is not sent:
a command may be reading keys, the server is waiting on the reply."
  (catch 'found
    (mapbacktrace (lambda (_evald fn _args _flags)
                    (when (memq fn '(command-execute call-interactively)) (throw 'found t))))
    nil))

(defun eas-output--query-blocker (caps)
  "Why the terminal of CAPS (level `env') must not be queried now, or nil.
The environment may already answer (Kitty graphics); a query may never
run nested, from a timer or outside a command (a command may be reading
a key sequence, the replies would become its keys; a process filter
such as the server's must not wait on the terminal) or with input
pending (it would be swallowed with the replies).  Only the first
reason is permanent."
  (let ((tmux (plist-get caps :tmux)))
    (cond ((eas-output-kitty-p caps) 'env)
          ((or (not eas-output-query-terminal) noninteractive) 'off)
          ((and (plist-get tmux :pane) (not (eas-output--passthrough-p tmux))) 'tmux)
          (eas-output--querying 'nested)
          ((or (eas-output--in-timer-p) (not (eas-output--in-command-p))) 'timer)
          ((input-pending-p) 'input))))

(defun eas-output--read-replies (timeout)
  "Characters the terminal sends until the DA1 reply ends, or a deadline.
The deadline is TIMEOUT seconds when nothing arrives, else
`eas-output-query-drain' from the first byte: DA1 was asked last, so
its reply means every reply was read.  Characters are read raw
\(`read-event' skips `input-decode-map'; keyboard translation is off);
other events are queued again in order.

The bound is wall-clock and never `read-event''s own timeout (an Emacs
may ignore it and wait for a key): `read-event' runs only with input
pending, the loop checks `float-time', and `with-timeout' backs it up.
Nothing waits longer than TIMEOUT plus `eas-output-query-drain'."
  (let* ((start (float-time)) (deadline (+ start timeout))
         (hard (+ start timeout eas-output-query-drain))
         (chars nil) (others nil) (done nil)
         (keyboard-translate-table nil) (extra-keyboard-modifiers 0)
         (inhibit-quit t))
    (with-timeout ((+ timeout eas-output-query-drain 0.1))
      (while (and (not done) (< (float-time) (min deadline hard)))
        (if (not (input-pending-p))
            (sleep-for 0.01)
          (let ((ev (read-event nil nil 0.01)))
            (cond ((null ev))
                  ((characterp ev)
                   (unless chars (setq deadline (max deadline (+ (float-time) eas-output-query-drain))))
                   (push ev chars)
                   ;; DA1's reply, CSI ? ... c, comes last: it ends the read.
                   (setq done (and (eq ev ?c)
                                   (string-match-p "\e\\[\\?[0-9;]*c\\'" (concat (reverse chars))))))
                  (t (push ev others)))))))
    (setq unread-command-events (append unread-command-events (nreverse others)))
    ;; Without the DA1 reply the bytes are a partial answer: discarded.
    (if done (concat (nreverse chars)) "")))

(defun eas-output-parse-replies (string)
  "The facts in the terminal's query replies STRING, as a plist.
:kitty is t when the Kitty graphics query was answered OK, :da1 the
DA1 attributes (integers), :cell (W . H) from CSI 16t or, with :text
from CSI 14t, the text area's pixel size."
  (let (out)
    (when (string-match "\e_Gi=31;\\([^\e]*\\)\e\\\\" string)
      (setq out (plist-put out :kitty (equal (match-string 1 string) "OK"))))
    (when (string-match "\e\\[\\?\\([0-9;]*\\)c" string)
      (setq out (plist-put out :da1 (mapcar #'string-to-number
                                            (split-string (match-string 1 string) ";" t)))))
    (when (string-match "\e\\[6;\\([0-9]+\\);\\([0-9]+\\)t" string)
      (setq out (plist-put out :cell (cons (string-to-number (match-string 2 string))
                                           (string-to-number (match-string 1 string))))))
    (when (string-match "\e\\[4;\\([0-9]+\\);\\([0-9]+\\)t" string)
      (setq out (plist-put out :text (cons (string-to-number (match-string 2 string))
                                           (string-to-number (match-string 1 string))))))
    out))

(defun eas-output--query (frame caps)
  "Ask FRAME's terminal (CAPS so far) what it supports; return the reply facts."
  (let ((terminal (frame-terminal frame)) (eas-output--querying t))
    (send-string-to-terminal
     ;; One query, DA1 last as its sentinel.  The Kitty query is not
     ;; sent through tmux: the outer terminal's reply would come after
     ;; tmux's own DA1 and reach Emacs as keys.
     (concat (unless (plist-get (plist-get caps :tmux) :pane)
               "\e_Gi=31,s=1,v=1,a=q,t=d,f=24;AAAA\e\\")
             "\e[16t\e[14t\e[c")
     terminal)
    (eas-output-parse-replies (eas-output--read-replies eas-output-query-timeout))))

(defun eas-output-detect (&optional window level)
  "Capabilities of WINDOW's frame (default selected), as a plist.
LEVEL nil reads only the frame and its environment: :gui, :svg (SVG
images display), :term, :term-program, :kitty-window-id.  `env' adds
:tty (the device) and :tmux (see `eas-output--tmux'), which run ps and
tmux.  `query' also asks a terminal frame (`eas-output-query-terminal')
for :kitty-reply, :da1 and :cell (W . H).  Each level runs once per
terminal and is cached until `eas-output-forget'.  At level `query'
:rasterizer is current (`eas-output-rasterizer-find'); below it, nil."
  (let* ((frame (window-frame (or window (selected-window))))
         (gui (display-graphic-p frame))
         (terminal (frame-terminal frame))
         (rank (lambda (l) (pcase l ('query 2) ('env 1) (_ 0))))
         (caps (gethash terminal eas-output--cache)))
    (unless caps
      (setq caps (list :level nil :gui gui
                       :svg (and gui (image-type-available-p 'svg) t)
                       :term (or (and (not gui) (ignore-errors (tty-type terminal))) (getenv "TERM" frame))
                       :term-program (getenv "TERM_PROGRAM" frame)
                       :kitty-window-id (getenv "KITTY_WINDOW_ID" frame))))
    (when (and (not gui) (>= (funcall rank level) 1) (< (funcall rank (plist-get caps :level)) 1))
      (let ((tty (eas-output--frame-tty frame)))
        (setq caps (append (list :level 'env :tty tty :tmux (eas-output--tmux frame tty))
                           (eas--plist-without caps :level)))))
    (when (and (not gui) (eq level 'query) (< (funcall rank (plist-get caps :level)) 2))
      (let* ((blocker (eas-output--query-blocker caps))
             (reply (and (null blocker) (eas-output--query frame caps))))
        ;; Not now is not never: a later chart asks again.
        (when (memq blocker '(nested timer input))
          (puthash terminal caps eas-output--cache))
        (setq caps (append (list :level 'query :queried (and reply t) :blocker blocker
                                 :kitty-reply (plist-get reply :kitty) :da1 (plist-get reply :da1)
                                 :cell (or (plist-get reply :cell)
                                           (when-let* ((text (plist-get reply :text)))
                                             (cons (/ (car text) (frame-width frame))
                                                   (/ (cdr text) (frame-height frame))))))
                           (eas--plist-without caps :level)))))
    (unless (memq (plist-get caps :blocker) '(nested timer input))
      (puthash terminal caps eas-output--cache))
    ;; Finding the rasterizer may load the module: only for pixels.
    (append (list :rasterizer (and (eq level 'query) (eas-output-rasterizer-find))) caps)))

(defun eas-output-capabilities (&optional window level)
  "Capabilities of WINDOW through `eas-output-capabilities-function'.
LEVEL is as in `eas-output-detect'."
  (funcall eas-output-capabilities-function window level))

(defun eas-output-cell (caps)
  "The cell size (W . H) for CAPS: `eas-output-cell-size', the reply, the default."
  (let ((cell (plist-get caps :cell)))
    (or eas-output-cell-size
        (and (consp cell) (> (car cell) 0) (> (cdr cell) 0) cell)
        eas-output-default-cell-size)))

;;; Rasterizers

(defun eas-output-rasterizer-find ()
  "The rasterizer available now: `module', `rsvg-convert' or nil.
Honours `eas-output-rasterizer'.  The module counts only when it was
built with the raster feature (it defines `eas-raster-module-encode')."
  (let ((module (and (memq eas-output-rasterizer '(auto module))
                     (or (fboundp 'eas-raster-module-encode)
                         (and (ignore-errors (eas-geo-native--load))
                              (fboundp 'eas-raster-module-encode)))
                     'module))
        (rsvg (and (memq eas-output-rasterizer '(auto rsvg-convert))
                   (executable-find "rsvg-convert") 'rsvg-convert)))
    (or module rsvg)))

(declare-function eas-raster-module-encode "eas-geo-module")

(defun eas-output-kitty-transmit (control id payload)
  "The Kitty transmit (a=t) of image ID: CONTROL keys and PAYLOAD bytes.
PAYLOAD (a unibyte string) goes base64 in chunks of 4096, q=2 so the
terminal sends no reply.  The module's encoder writes the same bytes."
  (let* ((b64 (base64-encode-string payload t)) (n (length b64)) (i 0) (out nil))
    (while (or (< i n) (null out))
      (let ((end (min n (+ i 4096))))
        (push (concat "\e_G" (if (zerop i) (format "a=t,%s,i=%d,q=2," control id) "")
                      (format "m=%d;" (if (< end n) 1 0)) (substring b64 i end) "\e\\")
              out)
        (setq i end)))
    (apply #'concat (nreverse out))))

(defun eas-output--rsvg (svg w h)
  "PNG bytes of SVG at W x H pixels from rsvg-convert."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-write 'utf-8) (coding-system-for-read 'binary))
      (insert (encode-coding-string svg 'utf-8))
      (unless (eql 0 (call-process-region (point-min) (point-max) "rsvg-convert" t '(t nil) nil
                                          "-w" (number-to-string w) "-h" (number-to-string h)
                                          "-f" "png"))
        (error "Rsvg-convert failed"))
      (buffer-string))))

(defun eas-output-encode (svg w h mode &optional id bg)
  "Bytes that draw SVG at W x H pixels in MODE (`kitty' or `sixel').
ID is the Kitty image id; BG the sixel background \"#rrggbb\".  Uses
`eas-output-rasterizer-find'; signals an error when there is none."
  (pcase (eas-output-rasterizer-find)
    ('module (eas-raster-module-encode svg w h (if (eq mode 'kitty) "kitty" "sixel") (or id 1) bg))
    ('rsvg-convert
     (let ((png (eas-output--rsvg svg w h)))
       (if (eq mode 'kitty)
           (eas-output-kitty-transmit "f=100" (or id 1) png)
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert png)
           (unless (eql 0 (let ((coding-system-for-read 'binary) (coding-system-for-write 'binary))
                            (call-process-region (point-min) (point-max) "img2sixel" t '(t nil) nil)))
             (error "Img2sixel failed"))
           (buffer-string)))))
    (_ (error "No rasterizer: build the module with its raster feature"))))

(defun eas-output-png (svg w h)
  "PNG bytes (a unibyte string) of SVG drawn at W x H pixels.
The pixel modes' rasterizer draws it (`eas-output-rasterizer-find'):
goldens and benchmarks see what the terminal is sent."
  (pcase (eas-output-rasterizer-find)
    ('module (base64-decode-string (eas-raster-module-encode svg w h "png")))
    ('rsvg-convert (eas-output--rsvg svg w h))
    (_ (error "No rasterizer: build the module with its raster feature"))))

;;; Resolution

(defun eas-output-normalize (value &optional source)
  "VALUE (a mode or a list or vector of modes, symbols or strings) as a list.
Signal `eas-output-invalid' naming SOURCE for anything that is not a mode."
  (let* ((items (cond ((and value (symbolp value)) (list value))
                      ((stringp value) (list value))
                      ((vectorp value) (append value nil))
                      ((and (consp value) (proper-list-p value)) value)
                      (t (list value))))
         (modes (mapcar (lambda (m) (cond ((stringp m) (intern-soft (downcase m))) ((symbolp m) m)))
                        items)))
    (cl-loop for m in modes for item in items
             unless (and m (memq m eas-output-modes))
             do (eas-signal "OUTPUT_INVALID"
                            (format "eas-output: %S is not an output mode (from %s); use one of %s or a list of them"
                                    item (or source "eas-output")
                                    (mapconcat #'symbol-name eas-output-modes ", "))
                            :value item :source (or source "eas-output")))
    (unless modes
      (eas-signal "OUTPUT_INVALID" (format "eas-output: an empty list (from %s) names no mode" (or source "eas-output"))
                  :value value :source (or source "eas-output")))
    modes))

(defun eas-output-check (mode caps)
  "Whether MODE can show under CAPS: nil when it can, else (REASON . FIX)."
  (let* ((tmux (plist-get caps :tmux))
         (pane (plist-get tmux :pane))
         (outer (or (plist-get tmux :client-type) "")))
    (pcase mode
      ('text nil)
      ('auto nil)
      ('svg (cond ((not (plist-get caps :gui))
                   (cons "SVG images need a graphical frame and this is a terminal"
                         "use auto, kitty or sixel for pixels here, or text"))
                  ((not (plist-get caps :svg))
                   (cons "this Emacs cannot display SVG images (no librsvg)"
                         "use text, or an Emacs built with librsvg"))))
      ((or 'kitty 'sixel)
       (cond
        ((plist-get caps :gui)
         (cons "this is a graphical frame; terminal pixel protocols apply only to terminals"
               "use svg (or auto) in graphical frames"))
        ((and pane (not (eas-output--passthrough-p tmux)))
         (cons (format "tmux (pane %s) does not pass escapes through: allow-passthrough is %s"
                       pane (or (plist-get tmux :passthrough) "off"))
               "run: tmux set -g allow-passthrough on"))
        ((and (eq mode 'kitty) (not (eas-output-kitty-p caps)))
         (cons (format "the terminal does not speak the Kitty graphics protocol (TERM=%s, TERM_PROGRAM=%s%s%s)"
                       (or (plist-get caps :term) "unset") (or (plist-get caps :term-program) "unset")
                       (if pane (format ", tmux client %s" outer) "")
                       (if (plist-get caps :queried) ", no reply to the graphics query" ""))
               "run Emacs in Kitty, WezTerm or Ghostty, or ask for (kitty sixel text) or auto"))
        ((and (eq mode 'sixel) (not (eas-output-sixel-p caps)))
         (cons (format "the terminal does not report sixel (DA1 %s)"
                       (if (plist-get caps :da1) (mapconcat #'number-to-string (plist-get caps :da1) ";")
                         (if (plist-get caps :queried) "gave no reply" "was not asked")))
               "run Emacs in a sixel terminal (foot, mlterm, xterm -ti vt340, WezTerm), or ask for (sixel text) or auto"))
        ((not (plist-get caps :rasterizer))
         (cons "nothing can draw SVG as pixels: the native module lacks its raster feature and rsvg-convert is not installed"
               "build the module with resvg: make module FEATURES=raster (or set eas-geo-build-features to (\"raster\") and M-x eas-geo-build-module), or install rsvg-convert"))
        ((and (eq mode 'sixel) (eq (plist-get caps :rasterizer) 'rsvg-convert)
              (not (executable-find "img2sixel")))
         (cons "sixel through rsvg-convert needs img2sixel, which is not installed"
               "build the module with resvg: make module FEATURES=raster, or install libsixel's img2sixel")))))))

(defun eas-output-kitty-p (caps)
  "Non-nil when CAPS describe a terminal that speaks Kitty graphics."
  (let ((tmux (plist-get caps :tmux)))
    (or (plist-get caps :kitty-reply)
        (and (plist-get caps :kitty-window-id) (not (plist-get tmux :pane)))
        (member (plist-get caps :term-program) eas-output-kitty-programs)
        (let ((term (if (plist-get tmux :pane) (plist-get tmux :client-term) (plist-get caps :term))))
          (and (stringp term) (string-match-p eas-output-kitty-terms term)))
        (let ((type (plist-get tmux :client-type)))
          (and (stringp type) (string-match-p "\\`\\(kitty\\|WezTerm\\|ghostty\\)" type))))))

(defun eas-output-sixel-p (caps)
  "Non-nil when CAPS describe a terminal that speaks sixel.
It reported sixel in DA1, or it is WezTerm (which is not queried)."
  (or (memq 4 (plist-get caps :da1))
      (and (equal (plist-get caps :term-program) "WezTerm")
           (not (plist-get (plist-get caps :tmux) :pane)))))

(defun eas-output--auto (caps)
  "The mode auto picks under CAPS."
  (cond ((plist-get caps :gui) (if (plist-get caps :svg) 'svg 'text))
        ((null (eas-output-check 'kitty caps)) 'kitty)
        ((null (eas-output-check 'sixel caps)) 'sixel)
        (t 'text)))

(defun eas-output-facts (caps)
  "The terminal facts in CAPS as one line, for messages."
  (let ((tmux (plist-get caps :tmux)))
    (if (plist-get caps :gui) "graphical frame"
      (format "terminal TERM=%s TERM_PROGRAM=%s; tmux: %s"
              (or (plist-get caps :term) "unset") (or (plist-get caps :term-program) "unset")
              (cond ((null tmux) "no")
                    ((null (plist-get tmux :pane))
                     (format "TMUX is set but %s is not a pane of that server (ignored)"
                             (or (plist-get caps :tty) "this tty")))
                    (t (format "pane %s, allow-passthrough %s, client %s"
                               (plist-get tmux :pane) (or (plist-get tmux :passthrough) "off")
                               (or (plist-get tmux :client-term) "?"))))))))

(cl-defun eas-output-resolve (value &key source caps window id)
  "The mode to draw with for VALUE (an `eas-output' value) under CAPS.
SOURCE says where VALUE came from, ID names the chart, both for the
error.  CAPS default to WINDOW's (`eas-output-capabilities').  Return
\(:mode MODE :asked VALUE :source SOURCE :caps CAPS :skipped SKIPPED),
SKIPPED the modes passed over with their reasons.  Signal
`eas-output-invalid' for a value that is not a mode and
`eas-output-unsupported' when no mode of VALUE can show, unless VALUE
ends in text or auto."
  (let* ((source (or source "eas-output"))
         (modes (eas-output-normalize value source))
         (pixels (seq-some (lambda (m) (memq m '(auto kitty sixel))) modes))
         ;; Only a pixel mode needs ps, tmux and the terminal query.
         (caps (or caps (eas-output-capabilities window (and pixels 'query))))
         (skipped nil)
         (mode (cl-loop for m in modes
                        for why = (eas-output-check m caps)
                        if (eq m 'auto) return (eas-output--auto caps)
                        else if (null why) return m
                        else do (push (cons m why) skipped))))
    (setq skipped (nreverse skipped))
    (unless mode
      (let* ((first (car skipped))
             (asked (mapconcat #'symbol-name modes " ")))
        (eas-signal "OUTPUT_UNSUPPORTED"
                    (format "eas: chart %s cannot use output %s (from %s): %s; fix: %s"
                            (or id "?") asked source
                            (mapconcat (lambda (s) (format "%s: %s" (car s) (cadr s))) skipped "; ")
                            (cddr first))
                    :view (or id :null) :mode asked :source source
                    :facts (eas-output-facts caps)
                    :reasons (mapcar (lambda (s) (list :mode (symbol-name (car s)) :reason (cadr s) :fix (cddr s)))
                                     skipped))))
    (list :mode mode :asked value :source source :caps caps :skipped skipped)))

(defun eas-output-error-text (err)
  "The text of output error ERR, as a chart buffer displays it."
  (let ((p (eas-error-plist err)))
    (if (equal (plist-get p :code) "OUTPUT_INVALID")
        (concat (plist-get p :message) "\n")
      (concat
       (format "eas: chart %s cannot be drawn with output %s\n\n" (plist-get p :view) (plist-get p :mode))
       (format "  asked:    %s (from %s)\n" (plist-get p :mode) (plist-get p :source))
       (format "  frame:    %s\n" (plist-get p :facts))
       (mapconcat (lambda (r) (format "  %-8s  %s\n            fix: %s\n"
                                      (concat (plist-get r :mode) ":") (plist-get r :reason) (plist-get r :fix)))
                  (plist-get p :reasons) "")
       "\nNothing was sent to the terminal.  End the list with text or auto to fall\n"
       "back instead of failing, e.g. (kitty sixel text).  M-x eas-output-describe\n"
       "shows what this window supports.\n"))))

;;; Placement

(cl-defstruct (eas-output-surface (:constructor eas-output-surface--make) (:copier nil))
  "A buffer whose blank cells carry an image in a pixel mode."
  buffer mode caps
  ;; The image to show: SVG, its hash, pixel and cell size, first line.
  svg hash width height cols rows (line 1)
  ;; The Kitty image id on the terminal and the hash it was sent with;
  ;; the sixel bytes of that hash.
  image sent bytes
  ;; Window -> the placement (TOP LEFT Y0 VISIBLE-ROWS) last drawn there.
  (placements nil))

(defvar eas-output--surfaces (make-hash-table :test 'eq)
  "Live surfaces by buffer.")

(defvar eas-output-write-function #'eas-output--send
  "Function of STRING and CAPS writing STRING to the terminal of CAPS.
Tests stub it to capture bytes.")

(defvar eas-output--next-id #x45410000
  "The next Kitty image id (#x4541 is \"EA\", away from other users).")

(defvar eas-output--timer nil "Pending `eas-output-sync'.")

(defun eas-output--send (string caps)
  "Write STRING, wrapped for tmux under CAPS, to the selected terminal."
  (send-string-to-terminal (eas-output-wrap string caps)))

(defun eas-output--write (string caps)
  "Write STRING for CAPS through `eas-output-write-function'."
  (funcall eas-output-write-function string caps))

(defun eas-output-surface (buffer)
  "BUFFER's surface, or nil."
  (gethash buffer eas-output--surfaces))

(defun eas-output-attach (buffer mode caps)
  "Make BUFFER carry images in pixel MODE (`kitty' or `sixel') under CAPS.
Return its surface.  Killing BUFFER detaches it."
  (let ((s (eas-output-surface buffer)))
    (unless (and s (eq (eas-output-surface-mode s) mode))
      (eas-output-detach buffer)
      (setq s (puthash buffer (eas-output-surface--make :buffer buffer :mode mode)
                       eas-output--surfaces)))
    (setf (eas-output-surface-caps s) caps)
    ;; A major mode run again kills local hooks: add them every time.
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'eas-output--kill nil t)
      (add-hook 'post-command-hook #'eas-output--after-command nil t))
    s))

(defun eas-output--kill ()
  "Detach the current buffer as it is killed."
  (eas-output-detach (current-buffer)))

(defun eas-output-cells (cols rows)
  "COLS x ROWS blank cells: ROWS lines of COLS spaces, each ending in a newline."
  (let ((line (concat (make-string cols ?\s) "\n")))
    (apply #'concat (make-list rows line))))

(defun eas-output-set-image (buffer svg cols rows &optional line)
  "Show SVG over COLS x ROWS blank cells of BUFFER from LINE (default 1).
BUFFER must be attached.  The image is drawn at the cells' pixel size
\(`eas-output-cell') after the next redisplay, and only sent when SVG
changed (`eas-output-sync')."
  (let* ((s (or (eas-output-surface buffer) (error "Buffer %s has no output surface" buffer)))
         (cell (eas-output-cell (eas-output-surface-caps s))))
    (setf (eas-output-surface-svg s) svg
          (eas-output-surface-hash s) (sxhash-equal svg)
          (eas-output-surface-cols s) cols (eas-output-surface-rows s) rows
          (eas-output-surface-width s) (* cols (car cell))
          (eas-output-surface-height s) (* rows (cdr cell))
          (eas-output-surface-line s) (or line 1))
    (eas-output-schedule)
    s))

(defun eas-output-schedule ()
  "Run `eas-output-sync' once Emacs has redisplayed."
  (unless (or eas-output--timer noninteractive)
    (setq eas-output--timer (run-at-time 0 nil #'eas-output--timer-sync))))

(defun eas-output--timer-sync ()
  "Redisplay, then `eas-output-sync' (from the timer)."
  (setq eas-output--timer nil)
  (when (> (hash-table-count eas-output--surfaces) 0)
    (redisplay)
    (eas-output-sync)))

(defun eas-output-geometry (s window)
  "The placement of surface S's image in WINDOW: (TOP LEFT Y0 ROWS), or nil.
TOP and LEFT are the screen cell of its first visible row, Y0 its
first visible image row, ROWS how many rows show."
  (with-current-buffer (eas-output-surface-buffer s)
    (pcase-let* ((`(,left ,top ,_right ,_bottom) (window-body-edges window))
                 (start (line-number-at-pos (window-start window)))
                 (line (eas-output-surface-line s))
                 (rows (eas-output-surface-rows s))
                 (y0 (max 0 (- start line)))
                 (screen (+ top (max 0 (- line start))))
                 (visible (min (- rows y0) (- (window-body-height window) (max 0 (- line start))))))
      (and (> visible 0) (zerop (window-hscroll window))
           (list screen left y0 visible)))))

(defun eas-output--kitty-place (s geom pid)
  "Kitty bytes placing S's image at GEOM with placement id PID."
  (pcase-let* ((`(,top ,left ,y0 ,rows) geom)
               (ch (/ (eas-output-surface-height s) (eas-output-surface-rows s))))
    (format "\e7\e[%d;%dH\e_Ga=p,i=%d,p=%d,x=0,y=%d,w=%d,h=%d,c=%d,r=%d,C=1,q=2\e\\\e8"
            (1+ top) (1+ left) (eas-output-surface-image s) pid
            (* y0 ch) (eas-output-surface-width s) (* rows ch)
            (eas-output-surface-cols s) rows)))

(defun eas-output--kitty-delete (id)
  "Kitty bytes deleting image ID and freeing its data."
  (format "\e_Ga=d,d=I,i=%d,q=2\e\\" id))

(defun eas-output--pid (window)
  "The Kitty placement id for WINDOW (stable for its life)."
  (1+ (mod (sxhash-eq window) #xfffff)))

(defun eas-output-sync ()
  "Bring every surface's images up to date on the terminal.
Send images that changed, place them where their windows show them,
and delete placements in windows that no longer show them."
  (maphash (lambda (buffer s)
             (if (buffer-live-p buffer) (eas-output--sync-surface s)
               (eas-output-detach buffer)))
           eas-output--surfaces))

(defun eas-output--sync-surface (s)
  "Bring surface S up to date (see `eas-output-sync')."
  (when (eas-output-surface-svg s)
    (let* ((buffer (eas-output-surface-buffer s))
           (caps (eas-output-surface-caps s))
           (windows (seq-filter (lambda (w) (not (display-graphic-p (window-frame w))))
                                (get-buffer-window-list buffer nil t)))
           (geoms (delq nil (mapcar (lambda (w) (when-let* ((g (eas-output-geometry s w))) (cons w g)))
                                    windows)))
           ;; Screen cells, not pane cells, under tmux: a moved pane re-places.
           (offset (if geoms (eas-output--pane-offset caps) '(0 . 0)))
           (geoms (mapcar (pcase-lambda (`(,w ,top ,left . ,rest))
                            (cons w (cons (+ top (car offset)) (cons (+ left (cdr offset)) rest))))
                          geoms))
           (fresh (not (eql (eas-output-surface-sent s) (eas-output-surface-hash s))))
           (old (eas-output-surface-placements s))
           (out nil))
      (when (and fresh geoms)
        (let ((prev (eas-output-surface-image s))
              (id (cl-incf eas-output--next-id)))
          (setf (eas-output-surface-image s) id
                (eas-output-surface-sent s) (eas-output-surface-hash s)
                (eas-output-surface-bytes s)
                (eas-output-encode (eas-output-surface-svg s) (eas-output-surface-width s)
                                   (eas-output-surface-height s) (eas-output-surface-mode s) id
                                   (eas-output--background)))
          (when (eq (eas-output-surface-mode s) 'kitty)
            (push (eas-output-surface-bytes s) out)
            ;; Placed before the old image goes: a live frame never blinks.
            (dolist (g geoms) (push (eas-output--kitty-place s (cdr g) (eas-output--pid (car g))) out))
            (when prev (push (eas-output--kitty-delete prev) out)))))
      (pcase (eas-output-surface-mode s)
        ('kitty
         (unless (and fresh geoms)
           (dolist (g geoms)
             (unless (equal (cdr (assq (car g) old)) (cdr g))
               (push (eas-output--kitty-place s (cdr g) (eas-output--pid (car g))) out))))
         (dolist (o old)
           (unless (assq (car o) geoms)
             (push (format "\e_Ga=d,d=i,i=%d,p=%d,q=2\e\\" (eas-output-surface-image s)
                           (eas-output--pid (car o)))
                   out))))
        ('sixel
         (let ((gone (seq-filter (lambda (o) (not (equal (cdr (assq (car o) geoms)) (cdr o)))) old)))
           ;; Sixel pixels live in the cells: only a repaint clears them.
           (when gone (eas-output--repaint (mapcar #'car gone)))
           (dolist (g geoms)
             (when (or fresh gone (not (equal (cdr (assq (car g) old)) (cdr g))))
               (pcase-let ((`(,top ,left ,y0 ,rows) (cdr g)))
                 ;; Sixel cannot crop: a partly scrolled image is not drawn.
                 (when (and (zerop y0) (= rows (eas-output-surface-rows s)))
                   (push (format "\e7\e[%d;%dH%s\e8" (1+ top) (1+ left) (eas-output-surface-bytes s))
                         out))))))))
      (setf (eas-output-surface-placements s) geoms)
      (when out (eas-output--write (apply #'concat (nreverse out)) caps)))))

(defun eas-output--repaint (windows)
  "Make Emacs repaint the frames of WINDOWS, clearing sixel pixels."
  (unless noninteractive
    (dolist (f (delete-dups (mapcar #'window-frame (seq-filter #'window-live-p windows))))
      (redraw-frame f))))

(defun eas-output--background ()
  "The default face's background as \"#rrggbb\", or nil."
  (let ((bg (face-attribute 'default :background nil t)))
    (when-let* (((stringp bg)) ((not (string-prefix-p "unspecified" bg))) (rgb (color-values bg)))
      (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)))))

(defun eas-output-detach (buffer)
  "Remove BUFFER's images from the terminal and forget its surface."
  (when-let* ((s (eas-output-surface buffer)))
    (remhash buffer eas-output--surfaces)
    (pcase (eas-output-surface-mode s)
      ('kitty (when (eas-output-surface-image s)
                (eas-output--write (eas-output--kitty-delete (eas-output-surface-image s))
                                   (eas-output-surface-caps s))))
      ('sixel (eas-output--repaint (mapcar #'car (eas-output-surface-placements s)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook #'eas-output--kill t)
        (remove-hook 'post-command-hook #'eas-output--after-command t)))))

(defconst eas-output-repaint-commands '(recenter-top-bottom recenter redraw-display)
  "Commands after which the terminal was cleared: images are placed again.")

(defun eas-output--after-command ()
  "Place this buffer's images again after a command that cleared the screen."
  (when-let* ((s (eas-output-surface (current-buffer))))
    (when (memq this-command eas-output-repaint-commands)
      (setf (eas-output-surface-placements s) nil)
      (eas-output-schedule))))

(defun eas-output--windows-changed (&rest _)
  "Schedule a sync when windows changed while surfaces are live."
  (when (> (hash-table-count eas-output--surfaces) 0)
    (eas-output-schedule)))

(add-hook 'window-state-change-hook #'eas-output--windows-changed)
(add-hook 'window-scroll-functions #'eas-output--windows-changed)

;;; Describe

(defun eas-output-describe (&optional window)
  "Describe the output modes WINDOW (default selected) can show, and why.
Shows the value in effect and its source, the terminal facts, each
mode's verdict and the mode an eas chart would draw with."
  (interactive)
  (let* ((window (or window (selected-window)))
         (buffer (window-buffer window))
         (setting (with-current-buffer buffer
                    (if (fboundp 'eas-mode-output-setting) (eas-mode-output-setting)
                      (cons eas-output "eas-output"))))
         (caps (eas-output-capabilities window 'query)))
    (with-help-window "*eas-output*"
      (princ (format "eas-output %S (from %s)\n\n" (car setting) (cdr setting)))
      (princ (format "Frame:      %s\n" (eas-output-facts caps)))
      (princ (format "Queried:    %s%s\n" (if (plist-get caps :queried) "yes" "no")
                     (if (plist-get caps :queried)
                         (format " (kitty reply %s, DA1 %s)" (if (plist-get caps :kitty-reply) "OK" "none")
                                 (or (plist-get caps :da1) "none"))
                       (pcase (plist-get caps :blocker)
                         ('env " (the environment says Kitty graphics)")
                         ('nested " (another query was reading)")
                         ('timer " (only from a command, not a timer or the server; the next chart asks)")
                         ('input " (input was pending; the next chart asks)")
                         (_ "")))))
      (princ (format "Cell:       %s x %s pixels\n" (car (eas-output-cell caps)) (cdr (eas-output-cell caps))))
      (princ (format "Rasterizer: %s\n\n" (or (plist-get caps :rasterizer) "none")))
      (dolist (m '(svg text kitty sixel))
        (let ((why (eas-output-check m caps)))
          (princ (if why (format "  %-6s no: %s\n         fix: %s\n" m (car why) (cdr why))
                   (format "  %-6s yes\n" m)))))
      (princ (format "\nauto picks %s.\n" (eas-output--auto caps)))
      (princ (condition-case err
                 (format "This setting draws with %s.\n"
                         (plist-get (eas-output-resolve (car setting) :source (cdr setting) :caps caps) :mode))
               (eas-error (format "\nThis setting fails:\n%s" (eas-output-error-text err))))))))

(provide 'eas-output)
;;; eas-output.el ends here
