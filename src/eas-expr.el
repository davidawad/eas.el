;;; eas-expr.el --- the safe Vega expression subset -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; filter and calculate take Vega expression strings.  eas parses a
;; small subset into an AST and interprets it; it never calls `eval'.
;;
;;   literals    numbers, 'strings', "strings", true false null, [a, b],
;;               {key: value}
;;   names       datum, datum.f, datum['f'], param names, PI, E
;;   operators   ?: || && == != === !== < <= > >= + - * / % ! unary -
;;   functions   `eas-expr-functions' (math, type tests, UTC date parts,
;;               d3 number format)
;;
;; Semantics follow JavaScript where it matters to charts: + joins
;; strings, a missing field is null, truthiness is JS truthiness, and
;; Vega's month() is 0-based.  Anything else fails as data:
;; PARSE_ERROR with :position, or UNSUPPORTED_FEATURE naming the call.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-format)

(defconst eas-expr--token-regexp
  (concat "[ \t\n]*\\(?:"
          "\\([0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)"      ; 1 number
          "\\|'\\(\\(?:[^'\\\\]\\|\\\\.\\)*\\)'"                    ; 2 'string'
          "\\|\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\""                 ; 3 "string"
          "\\|\\([A-Za-z_$][A-Za-z0-9_$]*\\)"                       ; 4 name
          "\\|\\(===\\|!==\\|==\\|!=\\|<=\\|>=\\|&&\\|||\\|[]+*/%<>!?:(){}[.,-]\\)" ; 5 op
          "\\)")
  "One token of the expression subset.")

(defun eas-expr--tokenize (string)
  "Return the tokens of STRING as (TYPE VALUE POSITION) lists."
  (let ((pos 0) (tokens nil) (len (length string)))
    (while (progn (when (string-match "\\`[ \t\n]+" (substring string pos))
                    (setq pos (+ pos (match-end 0))))
                  (< pos len))
      (unless (eq (string-match eas-expr--token-regexp string pos) pos)
        (eas-signal "PARSE_ERROR"
                      (format "Unexpected character %S at %d in expression %S"
                              (substring string pos (1+ pos)) pos string)
                      :expr string :position pos))
      (push (cond ((match-beginning 1) (list 'num (string-to-number (match-string 1 string)) pos))
                  ((match-beginning 2) (list 'str (eas-expr--unescape (match-string 2 string)) pos))
                  ((match-beginning 3) (list 'str (eas-expr--unescape (match-string 3 string)) pos))
                  ((match-beginning 4) (list 'name (match-string 4 string) pos))
                  (t (list 'op (match-string 5 string) pos)))
            tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))

(defun eas-expr--unescape (text)
  "Remove backslash escapes from TEXT."
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" text))

;;; Parser (precedence climbing over a token list)

(defvar eas-expr--tokens nil "Remaining tokens while parsing.")
(defvar eas-expr--source nil "The expression being parsed.")

(defun eas-expr--fail (message)
  "Signal PARSE_ERROR with MESSAGE at the current token."
  (let ((pos (if eas-expr--tokens (nth 2 (car eas-expr--tokens)) (length eas-expr--source))))
    (eas-signal "PARSE_ERROR" (format "%s at %d in expression %S" message pos eas-expr--source)
                  :expr eas-expr--source :position pos)))

(defun eas-expr--peek-op (&rest ops)
  "Return the next token's operator when it is one of OPS."
  (let ((tok (car eas-expr--tokens)))
    (and (eq (car tok) 'op) (member (nth 1 tok) ops) (nth 1 tok))))

(defun eas-expr--expect (op)
  "Consume operator OP or fail."
  (unless (eas-expr--peek-op op) (eas-expr--fail (format "Expected %s" op)))
  (pop eas-expr--tokens))

(defconst eas-expr--binary-levels
  '(("||") ("&&") ("==" "!=" "===" "!==") ("<" "<=" ">" ">=") ("+" "-") ("*" "/" "%"))
  "Binary operators from loosest to tightest.")

(defun eas-expr--ternary ()
  "Parse a conditional expression."
  (let ((test (eas-expr--binary 0)))
    (if (not (eas-expr--peek-op "?"))
        test
      (pop eas-expr--tokens)
      (let ((then (eas-expr--ternary)))
        (eas-expr--expect ":")
        (list :cond test then (eas-expr--ternary))))))

(defun eas-expr--binary (level)
  "Parse binary operators at LEVEL and tighter."
  (if (>= level (length eas-expr--binary-levels))
      (eas-expr--unary)
    (let ((left (eas-expr--binary (1+ level))) op)
      (while (setq op (apply #'eas-expr--peek-op (nth level eas-expr--binary-levels)))
        (pop eas-expr--tokens)
        (setq left (list :binary op left (eas-expr--binary (1+ level)))))
      left)))

(defun eas-expr--unary ()
  "Parse a unary expression."
  (if-let* ((op (eas-expr--peek-op "!" "-" "+")))
      (progn (pop eas-expr--tokens) (list :unary op (eas-expr--unary)))
    (eas-expr--postfix (eas-expr--primary))))

(defun eas-expr--args (close)
  "Parse comma-separated expressions up to CLOSE."
  (let (args)
    (unless (eas-expr--peek-op close)
      (push (eas-expr--ternary) args)
      (while (eas-expr--peek-op ",")
        (pop eas-expr--tokens)
        (push (eas-expr--ternary) args)))
    (eas-expr--expect close)
    (nreverse args)))

(defun eas-expr--primary ()
  "Parse a literal, name, array or parenthesized expression."
  (let ((tok (pop eas-expr--tokens)))
    (pcase tok
      ('nil (eas-expr--fail "Unexpected end"))
      (`(num ,n ,_) (list :lit n))
      (`(str ,s ,_) (list :lit s))
      (`(name "true" ,_) (list :lit t))
      (`(name "false" ,_) (list :lit :false))
      (`(name "null" ,_) (list :lit :null))
      (`(name ,name ,_) (list :var name))
      (`(op "(" ,_) (prog1 (eas-expr--ternary) (eas-expr--expect ")")))
      (`(op "[" ,_) (list :array (eas-expr--args "]")))
      (`(op "{" ,_) (eas-expr--object))
      (_ (push tok eas-expr--tokens) (eas-expr--fail "Unexpected token")))))

(defun eas-expr--object ()
  "Parse an object literal's {key: value, ...} after its brace."
  (let (pairs)
    (unless (eas-expr--peek-op "}")
      (while (progn
               (let ((key (pop eas-expr--tokens)))
                 (unless (memq (car key) '(str name num)) (eas-expr--fail "Expected an object key"))
                 (eas-expr--expect ":")
                 (push (cons (eas-expr--string (nth 1 key)) (eas-expr--ternary)) pairs))
               (when (eas-expr--peek-op ",") (pop eas-expr--tokens) t))))
    (eas-expr--expect "}")
    (list :object (nreverse pairs))))

(defun eas-expr--postfix (node)
  "Parse member access and calls following NODE."
  (let (op)
    (while (setq op (eas-expr--peek-op "." "[" "("))
      (pop eas-expr--tokens)
      (setq node
            (pcase op
              ("." (let ((tok (pop eas-expr--tokens)))
                     (unless (eq (car tok) 'name) (eas-expr--fail "Expected a field name"))
                     (list :member node (list :lit (nth 1 tok)))))
              ("[" (prog1 (list :member node (eas-expr--ternary)) (eas-expr--expect "]")))
              ("(" (unless (eq (car node) :var)
                     (eas-expr--fail "Only named functions can be called"))
               (let ((name (nth 1 node)))
                 (unless (assoc name eas-expr-functions)
                   (eas-signal "UNSUPPORTED_FEATURE"
                                 (format "Function %s() is not in the expression subset; available: %s"
                                         name (mapconcat #'car eas-expr-functions " "))
                                 :expr eas-expr--source :function name))
                 (list :call name (eas-expr--args ")")))))))
    node))

(defvar eas-expr--cache (make-hash-table :test 'equal)
  "Parsed expressions keyed by source string.")

(defun eas-expr-parse (string)
  "Parse expression STRING into an AST (cached)."
  (or (gethash string eas-expr--cache)
      (let* ((eas-expr--source string)
             (eas-expr--tokens (eas-expr--tokenize string))
             (ast (eas-expr--ternary)))
        (when eas-expr--tokens (eas-expr--fail "Unexpected trailing input"))
        (puthash string ast eas-expr--cache))))

;;; Evaluation

(defun eas-expr-truthy (value)
  "JavaScript truthiness of VALUE."
  (not (or (memq value '(nil :false :null)) (equal value "")
           (and (numberp value) (or (zerop value) (isnan (float value)))))))

(defun eas-expr--number (value)
  "Coerce VALUE to a number the way JavaScript's unary + does."
  (cond ((numberp value) value)
        ((eq value t) 1) ((memq value '(:false :null nil)) 0)
        ((and (stringp value) (string-match-p "\\`[ \t]*[-+]?[0-9.]+\\([eE][-+]?[0-9]+\\)?[ \t]*\\'" value))
         (string-to-number value))
        ((stringp value) (or (eas-time-parse value) 0.0e+NaN))
        (t 0.0e+NaN)))

(defun eas-expr--string (value)
  "Coerce VALUE to a string the way JavaScript does."
  (cond ((stringp value) value)
        ((eq value t) "true") ((eq value :false) "false") ((memq value '(:null nil)) "null")
        ((and (floatp value) (= value (ffloor value)) (< (abs value) 1e15))
         (number-to-string (truncate value)))
        (t (format "%s" value))))

(defun eas-expr--equal (a b)
  "Loose equality of A and B."
  (cond ((and (numberp a) (numberp b)) (= a b))
        ((or (numberp a) (numberp b)) (ignore-errors (= (eas-expr--number a) (eas-expr--number b))))
        (t (equal a b))))

(defun eas-expr--compare (op a b)
  "Apply comparison OP to A and B."
  (let ((result (if (and (stringp a) (stringp b))
                    (pcase op ("<" (string< a b)) (">" (string< b a))
                           ("<=" (not (string< b a))) (">=" (not (string< a b))))
                  (let ((x (eas-expr--number a)) (y (eas-expr--number b)))
                    (pcase op ("<" (< x y)) (">" (> x y)) ("<=" (<= x y)) (">=" (>= x y)))))))
    (if result t :false)))

(defun eas-expr--arith (op a b)
  "Apply arithmetic OP to A and B."
  (if (and (equal op "+") (or (stringp a) (stringp b)))
      (concat (eas-expr--string a) (eas-expr--string b))
    (let ((x (eas-expr--number a)) (y (eas-expr--number b)))
      (pcase op
        ("+" (+ x y)) ("-" (- x y)) ("*" (* x y))
        ("/" (if (and (zerop y) (integerp x)) (/ (float x) y) (/ (float x) y)))
        ("%" (if (zerop y) 0.0e+NaN (let ((r (mod (float x) (float y))))
                                       (if (and (< x 0) (/= r 0)) (- r (abs y)) r))))))))

(defun eas-expr--member (object key)
  "Return field KEY of OBJECT (a row plist or param value)."
  (cond ((and (vectorp object) (numberp key))
         (if (< -1 key (length object)) (aref object (truncate key)) :null))
        ((and (eas-object-p object) (stringp key))
         (let ((cell (plist-member object (eas-key key))))
           (if cell (cadr cell) :null)))
        (t :null)))

(defun eas-expr-eval (ast datum &optional env)
  "Evaluate AST for row DATUM.  ENV is a plist of param values by name key."
  (pcase ast
    (`(:lit ,v) v)
    (`(:array ,items) (vconcat (mapcar (lambda (i) (eas-expr-eval i datum env)) items)))
    (`(:object ,pairs) (cl-loop for (k . v) in pairs append (list (eas-key k) (eas-expr-eval v datum env))))
    (`(:var ,name)
     (cond ((equal name "datum") datum)
           ((plist-member env (eas-key name)) (plist-get env (eas-key name)))
           ((equal name "PI") float-pi) ((equal name "E") float-e)
           (t (eas-signal "INVALID_INPUT"
                            (format "Unknown name %s in expression; fields are datum.%s, params by name"
                                    name name)
                            :name name))))
    (`(:member ,object ,key)
     (eas-expr--member (eas-expr-eval object datum env) (eas-expr-eval key datum env)))
    (`(:unary ,op ,a)
     (let ((v (eas-expr-eval a datum env)))
       (pcase op ("!" (if (eas-expr-truthy v) :false t))
              ("-" (- (eas-expr--number v))) ("+" (eas-expr--number v)))))
    (`(:cond ,test ,then ,else)
     (eas-expr-eval (if (eas-expr-truthy (eas-expr-eval test datum env)) then else) datum env))
    (`(:binary ,op ,a ,b)
     (pcase op
       ("&&" (let ((x (eas-expr-eval a datum env)))
               (if (eas-expr-truthy x) (eas-expr-eval b datum env) x)))
       ("||" (let ((x (eas-expr-eval a datum env)))
               (if (eas-expr-truthy x) x (eas-expr-eval b datum env))))
       (_ (let ((x (eas-expr-eval a datum env)) (y (eas-expr-eval b datum env)))
            (pcase op
              ((or "==" "===") (if (eas-expr--equal x y) t :false))
              ((or "!=" "!==") (if (eas-expr--equal x y) :false t))
              ((or "<" "<=" ">" ">=") (eas-expr--compare op x y))
              (_ (eas-expr--arith op x y)))))))
    (`(:call ,name ,args)
     (apply (cdr (assoc name eas-expr-functions))
            (mapcar (lambda (arg) (eas-expr-eval arg datum env)) args)))))

(defun eas-expr-evaluate (string datum &optional env)
  "Parse (cached) and evaluate expression STRING for DATUM with ENV."
  (eas-expr-eval (eas-expr-parse string) datum env))

(defun eas-expr--date-part (key &optional offset)
  "Return a function extracting date field KEY (plus OFFSET) from a date."
  (lambda (value)
    (let ((ms (eas-time-parse value)))
      (if ms (+ (or offset 0) (plist-get (eas-time-fields ms) key)) :null))))

(defun eas-expr--num-fn (fn)
  "Wrap numeric FN so its arguments are coerced to numbers."
  (lambda (&rest args) (apply fn (mapcar #'eas-expr--number args))))

(defconst eas-expr-functions
  `(("abs" . ,(eas-expr--num-fn #'abs))
    ("ceil" . ,(eas-expr--num-fn (lambda (x) (float (ceiling x)))))
    ("floor" . ,(eas-expr--num-fn (lambda (x) (float (floor x)))))
    ("round" . ,(eas-expr--num-fn (lambda (x) (float (floor (+ x 0.5))))))
    ("sqrt" . ,(eas-expr--num-fn #'sqrt))
    ("log" . ,(eas-expr--num-fn #'log))
    ("exp" . ,(eas-expr--num-fn #'exp))
    ("pow" . ,(eas-expr--num-fn #'expt))
    ("min" . ,(eas-expr--num-fn #'min))
    ("max" . ,(eas-expr--num-fn #'max))
    ("isValid" . ,(lambda (v) (if (memq v '(nil :null)) :false t)))
    ("isNumber" . ,(lambda (v) (if (numberp v) t :false)))
    ("isString" . ,(lambda (v) (if (stringp v) t :false)))
    ("isDate" . ,(lambda (v) (if (eas-time-string-p v) t :false)))
    ("toNumber" . eas-expr--number)
    ("toString" . eas-expr--string)
    ("toBoolean" . ,(lambda (v) (if (eas-expr-truthy v) t :false)))
    ("toDate" . ,(lambda (v) (or (eas-time-parse v) :null)))
    ("time" . ,(lambda (v) (or (eas-time-parse v) :null)))
    ("year" . ,(eas-expr--date-part :year))
    ("month" . ,(eas-expr--date-part :month -1))
    ("date" . ,(eas-expr--date-part :day))
    ("day" . ,(eas-expr--date-part :weekday))
    ("hours" . ,(eas-expr--date-part :hours))
    ("minutes" . ,(eas-expr--date-part :minutes))
    ("seconds" . ,(eas-expr--date-part :seconds))
    ("datetime" . ,(lambda (y &optional m d h mi s ms)
                     (eas-time-ms y (1+ (or m 0)) (or d 1) h mi s ms)))
    ("length" . ,(lambda (v) (length v)))
    ("upper" . ,(lambda (s) (upcase (eas-expr--string s))))
    ("lower" . ,(lambda (s) (downcase (eas-expr--string s))))
    ("inrange" . ,(lambda (v range)
                    (let ((lo (min (aref range 0) (aref range 1)))
                          (hi (max (aref range 0) (aref range 1))))
                      (if (<= lo (eas-expr--number v) hi) t :false))))
    ("if" . ,(lambda (test a b) (if (eas-expr-truthy test) a b)))
    ("format" . ,(lambda (v spec) (eas-format-number (eas-expr--string spec) v))))
  "Functions callable from expressions: (NAME . FUNCTION).")

(provide 'eas-expr)
;;; eas-expr.el ends here
