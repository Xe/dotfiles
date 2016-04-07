;;; lua2-mode.el --- a semantic highlighting extension for lua-mode

;; Copyright (C) 2010 Florian Weimer <fw@deneb.enyo.de>

;; Author: Florian Weimer <fw@deneb.enyo.de>
;; Version: 0.2
;; Maintainer: Florian Weimer
;; Keywords: languages, lua

;; This file is NOT part of Emacs.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; This library provides semantic highlighting for Lua scripts.  In
;; particular, global and local variable references are clearly
;; distinguished.  This distinction is a property of the program text,
;; and not of the execution environment, so it is possible to extract
;; it with a parser and relatively straightforward semantic analysis.
;;
;; This should be used for demonstration purposes only.  There are
;; speed issues even for medium-sized source files, and the whole
;; approach is a dead end: Emacs has no plugin API, so it is not
;; possible to embed the Lua interpreter without creating a custom
;; Emacs fork.  If we want to present further semantic information, we
;; will have to execute Lua code in a sandbox, and this requires
;; access to some Lua interpreter.


;;; Customization

(defface lua2-error
  '((t :inherit font-lock-warning-face))
  "Lua syntax errors"
  :group 'lua2-mode)

(defface lua2-bind-variable
  '((t :inherit (font-lock-variable-name-face italic)))
  "Local Lua variable at the point where it is bound"
  :group 'lua2-mode)

(defface lua2-reference-variable
  '((t :inherit font-lock-variable-name-face))
  "Reference to a local Lua varriable"
  :group 'lua2-mode)
(make-variable-buffer-local 'lua2-errors)

(defface lua2-assign-variable
  '((t :inherit (font-lock-variable-name-face underline)))
  "Assignment to a local Lua varriable"
  :group 'lua2-mode)

(defface lua2-reference-global-variable
  '((t :inherit (bold)))
  "Reference to a global Lua varriable"
  :group 'lua2-mode)

(defface lua2-assign-global-variable
  '((t :inherit (font-lock-variable-name-face bold-italic underline)))
  "Assignment to a global Lua varriable"
  :group 'lua2-mode)


;;; Lexer functions

(defvar lua2-errors nil
  "This buffer-local variable indicates whether errors have been
  encountered during lexing or parsing.")

(defvar lua2-lex-current-value nil
  "Set by the lexers to the last value encountered.")

(defvar lua2-lex-current-beginning nil
  "Set by the lexers to the beginning of the current token.")

(defvar lua2-lex-current-end nil
  "Set by the lexers to the end of the current token.")

(defvar lua2-lex-current-overlay nil
  "Set by the lexers to an overlay over the last parsed token.")

(defvar lua2-lex-put-token nil
  "Set this to true if you want to debug Lua tokens.")

(defun lua2-lex-put-token (begin end token)
  "Sets `lua2-token' overlay on BEGIN til END if TOKEN is `name',
or if in debug mode."
  (setq lua2-lex-current-end end)
  (when lua2-lex-put-token
    (setq lua2-lex-current-overlay (make-overlay begin end))
    (overlay-put lua2-lex-current-overlay 'lua2-token token))
  token)

(defun lua2-lex-make-overlay ()
  "Creates an overlay for the current token."
  (or lua2-lex-current-overlay
      (setq lua2-lex-current-overlay
	    (make-overlay lua2-lex-current-beginning lua2-lex-current-end))))

(defmacro lua2-lex-peek (&rest body)
  "Hides the effect of BODY in terms of lexing.
Still sets overlays in front of us as a side effect, but
those will simply be written again."
  `(save-excursion
    (save-match-data
      (let (lua2-lex-current-value
	    lua2-lex-current-beginning
	    lua2-lex-current-end
	    lua2-lex-current-overlay)
	,@body))))

(defun lua2-overlay-put-current (symbol value)
  (overlay-put (lua2-lex-make-overlay) symbol value))

(defun lua2-overlay-put (begin end symbol value)
  (setq lua2-lex-current-overlay (make-overlay begin end))
  (lua2-overlay-put-current symbol value))

(defun lua2-lex-set-error-region (begin end message &rest args)
  (setq lua2-errors t)
  (lua2-overlay-put begin end 'lua2-error
		    (apply 'format message args))
  nil)

(defun lua2-lex-set-error (message &rest args)
  (apply 'lua2-lex-set-error-region
	 lua2-lex-current-beginning (point) message args))

(defun lua2-lex-set-property (property)
  (let ((end (match-end 0)))
    (lua2-lex-put-token (match-beginning 0) end property)
    (goto-char end)
    property))

(defvar lua2-lex-name-hash (make-hash-table :test 'equal :size 21)
  "Hash table of Lua names.")
(let ((l '(and       break     do        else      elseif
           end       false     for       function  if
	   in        local               not       or
	   repeat    return    then      true      until     while)))
  (while l
    (puthash (symbol-name (car l)) (car l) lua2-lex-name-hash)
    (setq l (cdr l))))
;; We do not want to map Lua's nil to a Lisp nil; tokens must
;; be true values.
(puthash "nil" 'Nil lua2-lex-name-hash)

(defvar lua2-lex-name-regexp "[a-zA-Z_][a-zA-Z_0-9]*"
  "Regular expression matching Lua names.")

(defun lua2-lex-name ()
  (if (looking-at lua2-lex-name-regexp)
      (let* ((id (buffer-substring-no-properties
		  (match-beginning 0) (match-end 0)))
	     (kind (or (gethash id lua2-lex-name-hash) 
		       'name)))
	(setq lua2-lex-current-value id)
	(lua2-lex-set-property kind))
    (error "Internal error")))

(defun lua2-lex-number ()
  (if (looking-at
       "0x[0-9a-fA-F]+\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE]-?[0-9]+\\)?")
      (progn
	(setq lua2-lex-current-value 
	    (string-to-number
	     (buffer-substring-no-properties
	      (match-beginning 0) (match-end 0))))
	(lua2-lex-set-property 'number))
    (error "Internal error")))

(defun lua2-lex-string (ch)
  (let ((start (point))
	kind)
    (forward-char)			; skip CH
    (while (not kind)
      (if (re-search-forward "[\"'\\\\]" nil t)
	  (cond
	   ((eq (char-before) ch) (setq kind 'string))
	   ((eq (char-before) ?\\)
	    (when (or (eq (char-after) ?\\)
		      (eq (char-after) ch))
	      (forward-char))))
	(setq kind 'error)))
    (if (eq kind 'error)
	(lua2-lex-set-error-region start (point) "unterminated string")
      (setq lua2-lex-current-value (buffer-substring-no-properties
				    start (point)))
      (lua2-lex-put-token start (point) 'string))
    kind))

(defun lua2-lex-bracket ()
  (if (looking-at "\\[\\(=*\\)\\[")
      (let ((start (point))
	    (equal-signs (buffer-substring-no-properties
			  (match-beginning 1) (match-end 1))))
	  (if (re-search-forward (concat "\\]" equal-signs "\\]") nil t)
		(lua2-lex-put-token start (point) 'string)
	    (lua2-lex-set-error-region start (point-max)
				       "unterminated string")
	    (goto-char (point-max))))
    (lua2-lex-symbol)))

(defvar lua2-lex-symbol-regexp
  (regexp-opt '(
     "+"    "-"    "*"     "/"     "%"     "^"     "#"
     "=="   "~="   "<="    ">="    "<"     ">"     "="
     "("    ")"    "{"     "}"     "["     "]"
     ";"    ":"    ","     "."     ".."    "...")))

(defun lua2-lex-symbol()
  (if (looking-at lua2-lex-symbol-regexp)
      (let ((sym (buffer-substring-no-properties
		  (match-beginning 0) (match-end 0))))
	(when lua2-lex-put-token
	  (lua2-overlay-put (match-beginning 0) (match-end 0)
			    'lua2-token-value sym))
	(setq lua2-lex-current-value sym)
	(lua2-lex-set-property 'symbol))
    (lua2-lex-set-error-region (point) (1+ (point)) "illegal character")
    (forward-char)
    nil))

(defun lua2-lex-looking-at-comma ()
  (save-match-data
    (or
     ;; Speed up the common cases.  Keep in sync with lua2-lex.
     (looking-at "[ \t\n\r]*,")
     (and
      (looking-at "[ \t\n\r]*--")
      (lua2-lex-peek
       (and (eq (lua2-lex) 'symbol) (equal lua2-lex-current-value ",")))))))

(defun lua2-lex-looking-at-postargs ()
  "True if the lexer is looking at the post-args part of a prefixexp."
  (save-match-data
    (or
     (looking-at "[ \t\n\r]*[,({.:\"\\[]")
     (and
      (looking-at "[ \t\n\r]*--")
      (lua2-lex-peek
       (let ((token (lua2-lex)))
	 (or
	  (eq token 'string)
	  (and (eq token 'symbol)
	       (member lua2-lex-current-value
		       '("," "(" "{" "." ":" "["))))))))))

(defun lua2-lex-is-binop (token)
  (or
   (memq token '(and or string))
   (and (eq token 'symbol)
	(member lua2-lex-current-value
		'(;; Numerous extensions for parser simplification
		  "." ":" "(" "[" "{"
		  "+" "-" "*" "/" "^" "%" ".."
		  "<" "<=" ">" ">=" "==" "~=")))))

(defun lua2-lex-looking-at-binop ()
  (save-match-data
    (or
     ;; Speed up the common cases.  Keep in sync with lua2-lex.
     (looking-at
      "[ \t\n\r]*\\([+*/^%<>~:({'\"]\\|==\\|\\.\\. \\|\\.[^.]\\|-[^-]\\|and \\|or \\)")
     (and 
      (looking-at "[ \t\n\r]*[-ao.=\\[]") ;; --, and, or etc. are ambigous
      (lua2-lex-peek
       (lua2-lex-is-binop (lua2-lex)))))))

(defun lua2-lex ()
  (setq lua2-lex-current-value nil)
  ;; Keep whitespace regexp in sync with lua2-lex-looking-at-comma,
  ;; in lua2-lex-looking-at-binop.
  (when (looking-at "\\([ \t\n\r]+\\)")
    (goto-char (match-end 0)))
  (setq lua2-lex-current-beginning (point))
  (setq lua2-lex-current-overlay nil)
  (let ((ch (char-after)))
    (cond
     ((not ch)
      (setq lua2-lex-current-value 'eof)
      'eof)
     ((and (<= ?0 ch) (<= ch ?9)) (lua2-lex-number))
     ((or (and (<= ?A ch) (<= ch ?Z))
	  (and (<= ?a ch) (<= ch ?z)) (= ch ?_))
      (lua2-lex-name))
     ((memq ch '(?\" ?\')) (lua2-lex-string ch))
     ((eq ch ?\[) (lua2-lex-bracket))
     ((eq ch ?.)
      (if (looking-at "\\.[0-9]")
	  (lua2-lex-number)
	(lua2-lex-symbol)))
     ((and (eq ch ?-) (looking-at "--"))
      (lua2-lex-skip-comments)
      (lua2-lex))
     (t (lua2-lex-symbol)))))

(defun lua2-lex-skip-comments ()
  (let ((scan t))
    (while scan
      (let ((start (point)))
	(if (looking-at "--\\[\\(=*\\)\\[")
	    (let ((equal-signs (buffer-substring-no-properties
				(match-beginning 1) (match-end 1))))
	      (if (re-search-forward (concat "\\]" equal-signs "\\]") nil t)
		  (lua2-lex-put-token start (point) 'comment)
		(lua2-lex-set-error-region start (point-max)
					   "unterminated comment")
		(goto-char (point-max))))
	  (move-end-of-line 1)
	  (lua2-lex-put-token start (point) 'comment)))
      (setq scan (looking-at "\\([ \t\n\r]*\\)--"))
      (when scan
	(goto-char (match-end 1))))))


;;;; Parser functions

(defvar lua2-parse-block-list nil
  "List of variable mappings for open blocks during parsing.")

(defun lua2-parse-lookup (var)
  "Returns the declaration information for VAR, a string.
If this is not a declared variable, returns nil."
  (let ((l lua2-parse-block-list)
	result)
    (while l
      (setq result (gethash var (car l)))
      (if result
	  (setq l nil)
	(setq l (cdr l))))
    (or result 'global)))

(defun lua2-parse-open-block ()
  (setq lua2-parse-block-list 
	(cons (make-hash-table :test 'equal) lua2-parse-block-list))
  nil)

(defun lua2-parse-close-block ()
  (setq lua2-parse-block-list (cdr lua2-parse-block-list))
  nil)

(defun lua2-parse-bind-name (var info)
  "Binds VAR to INFO in the top-most open block.
Returns VAR."
  (puthash var info (car lua2-parse-block-list))
  var)

(defun lua2-parse-bind-current-name ()
  "Binds the last lexed name as a variable, in the current block."
  (lua2-overlay-put-current 'lua2-reference 'bind)
  (lua2-parse-bind-name lua2-lex-current-value lua2-lex-current-beginning))

(defun lua2-parse-reference-current-name ()
  (lua2-overlay-put-current 'lua2-reference 
			    (lua2-parse-lookup lua2-lex-current-value))
  lua2-lex-current-value)

(defun lua2-parse-keyword (keyword)
  (unless (eq (lua2-lex) keyword)
    (lua2-lex-set-error "expected %S" keyword)))

(defun lua2-parse-stat ()
  "Parses a statement or block exit keyword.
If a block exit keyword is encountered, it is returned.
Otherwise, return nil"
  (let ((token (lua2-lex)))
    (cond
     ((eq token 'name) (lua2-parse-prefixexp (lua2-lex-make-overlay) t))
     ((eq token 'do)
      (lua2-parse-block '(end))
      nil)
     ((eq token 'while) (lua2-parse-while))
     ((eq token 'repeat) (lua2-parse-repeat))
     ((eq token 'break) (lua2-parse-break))
     ((eq token 'if) (lua2-parse-if))
     ((eq token 'for) (lua2-parse-for))
     ((eq token 'function) (lua2-parse-function))
     ((eq token 'local) (lua2-parse-local))
     ((eq token 'return) (lua2-parse-return))
     ((lua2-parse-symbol "(" token)
      (lua2-parse-exp)
      (lua2-parse-symbol ")")
      (lua2-parse-prefixexp nil nil))
     (t token))))

(defvar lua2-parse-in-loop nil
  "Bound to true if the parser is in a loop.")

(defvar lua2-parse-unreachable-code nil
  "Bound to true if the parser is in a loop, at unreachable code.")

(defun lua2-parse-while ()
  (lua2-parse-open-block)
  (let ((lua2-parse-in-loop t))
    (lua2-parse-exp)
    (lua2-parse-keyword 'do)
    (lua2-parse-block-no-open '(end)))
  (lua2-parse-close-block))

(defun lua2-parse-repeat ()
  (lua2-parse-open-block)
  (let ((lua2-parse-in-loop t))
    (lua2-parse-block-no-open '(until))
    (lua2-parse-exp))
  (lua2-parse-close-block))

(defun lua2-parse-break ()
  (if lua2-parse-in-loop
      (setq lua2-parse-unreachable-code t)
    (lua2-lex-set-error "break outside loop"))
  nil)

(defun lua2-parse-if ()
  (lua2-parse-exp)
  (lua2-parse-keyword 'then)
  (let ((exit (lua2-parse-block '(end else elseif))))
    (cond
     ;; nothing to do for "end"
     ((eq exit 'else)
      (lua2-parse-block '(end))
      nil)
     ((eq exit 'elseif)
      (lua2-parse-if)))))

(defun lua2-parse-for ()
  (lua2-parse-open-block)
  ;; first name
  (let ((token (lua2-lex)))
    (if (eq token 'name)
	(lua2-parse-bind-current-name)
      (lua2-lex-set-error "name expected")))
  (if (lua2-lex-looking-at-comma)
      (progn
	(lua2-lex)			; comma
	(lua2-parse-name-list)
	(lua2-parse-keyword 'in)
	(lua2-parse-explist))
    (let ((token (lua2-lex)))
      (cond
       ;; FIXME: incorrectly accepts: for x = 1, 2, 3, 4 do
       ((lua2-parse-symbol "=" token) (lua2-parse-explist))
       ((eq token 'in) (lua2-parse-explist))
       (t (lua2-lex-set-error "expected \"=\" or \"in\"")))))
  (lua2-parse-symbol "do")
  (let ((lua2-parse-in-loop t))
    (lua2-parse-block-no-open '(end)))
  (lua2-parse-close-block)
  nil)

(defun lua2-parse-function-name (after-local)
  (let ((token (lua2-lex)))
    (if (eq token 'name)
	(progn
	  (if after-local
	      (lua2-parse-bind-current-name)
	    (lua2-parse-reference-current-name)
	    (lua2-overlay-put-current 'lua2-assign t))
	  t)
      (lua2-lex-set-error "name expected")
      nil)))

(defun lua2-parse-function (&optional after-local)
  (when (lua2-parse-function-name after-local)
    (let ((scan t))
      (while scan
	(let ((token (lua2-lex)))
	  (cond
	   ((eq token 'symbol)
	    (cond
	     ((member lua2-lex-current-value
		      (if after-local '(".") '(":" ".")))
	      (unless (eq (lua2-lex) 'name)
		(lua2-lex-set-error "name expected")))
	     ((equal lua2-lex-current-value "(")
	      (setq scan nil)
	      (lua2-parse-function-parlist token))
	     (t 
	      (lua2-lex-set-error "\"(\" expected")
	      (setq scan nil))))
	   ((eq token 'eof)
	    (lua2-lex-set-error "unexpected end of file")
	    (setq scan nil))
	   (t 
	    (lua2-lex-set-error "\"(\" expected")
	    (setq scan nil)))))))
  nil)

(defun lua2-parse-return ()
  (let ((scan t) token)
    (while scan
      (setq token (lua2-lex))
      (cond
       ((memq token '(end until eof else elseif)) (setq scan nil))
       (t
	(lua2-parse-exp nil nil token)
	(if (lua2-lex-looking-at-comma)
	    (lua2-lex)
	  (setq scan nil
		token nil)))))
    token))

(defun lua2-parse-symbol (symbol &optional lexed)
  (let ((token (or lexed (lua2-lex))))
    (and (eq token 'symbol) (equal lua2-lex-current-value symbol))))

(defun lua2-parse-symbol-with-error (symbol &optional lexed)
  (if (lua2-parse-symbol symbol lexed)
      t
    (lua2-lex-set-error "expected %S" symbol)))

(defun lua2-parse-function-parlist (token)
  (unless (lua2-parse-symbol "(" token)
    (lua2-lex-set-error "expected \"(\", got %S" token))
  (lua2-parse-open-block)
  (let ((scan t))
    (while scan
      (let ((token (lua2-lex)))
	(cond
	 ((eq token 'name)
	  (lua2-parse-bind-current-name)
	  (let ((token (lua2-lex)))
	    (cond
	     ((lua2-parse-symbol "," token)) ; nothing to do
	     ((lua2-parse-symbol ")" token)
	      (setq scan nil))
	     ((and (eq token 'symbol) (equal lua2-lex-current-value "..."))
	      (when (lua2-parse-symbol-with-error ")")
		(setq scan nil)))
	     (t (lua2-lex-set-error "expected \")\"")))))
	 ((lua2-parse-symbol "..." token)
	  (when (lua2-parse-symbol-with-error ")")
	    (setq scan nil)))
	 ((lua2-parse-symbol ")" token) (setq scan nil))
	 (t 
	  (lua2-lex-set-error "expected \")\"")
	  (setq scan nil))))))
  (let (lua2-parse-in-loop)
    (lua2-parse-block-no-open '(end)))
  (lua2-parse-close-block))

(defun lua2-parse-name-list ()
  "Parses function definition or name list after a \"local\" token."
  (let ((scan t) token name-seen)
    (while scan
      (setq token (lua2-lex))
	(cond
	 ((eq token 'name)
	  (lua2-parse-bind-current-name)
	  (setq name-seen t)
	  (if (lua2-lex-looking-at-comma)
	      (lua2-lex)
	    (setq scan nil)))
	 (t
	  (when name-seen
	    (lua2-lex-set-error "name expected"))
	  (setq scan nil))))
    token))
	 
(defun lua2-parse-local ()
  (let (token block)
    ;; Delay local bindings.
    (lua2-parse-open-block)
    (setq token (lua2-parse-name-list))
    (setq block (car lua2-parse-block-list))
    (lua2-parse-close-block)
    (cond
     ((eq token 'name)
      (when (lua2-lex-peek (lua2-parse-symbol "="))
	(lua2-lex)
	(lua2-parse-explist))
      ;; Install delayed bindings.
      (maphash (lambda (key value)
		 (puthash key value (car lua2-parse-block-list)))
	       block))
     ((eq token 'function) (lua2-parse-function t))
     (t (lua2-lex-set-error "expected name"))))
  nil)

(defun lua2-parse-explist (&optional terminator)
  (let ((scan t))
    (while scan
      (if (lua2-parse-exp nil terminator)
	  (setq scan nil
		terminator nil)
	(if (lua2-lex-looking-at-comma)
	    (lua2-lex)
	  (setq scan nil)))))
  (when terminator
    (lua2-parse-symbol-with-error terminator)))

(defun lua2-parse-exp (&optional follower terminator token)
  "Parses a Lua expression.
Returns true if TERMINATOR (a string) has been found."
  (unless token
    (setq token (lua2-lex)))
  (let ((scan t) terminator-seen)
    (while scan
      (cond
       ;; Nothing to do for atoms.
       ((memq token '(Nil false true number string)))
       ((lua2-parse-symbol "..." token))
       ((eq token 'function)
	(lua2-parse-function-parlist (lua2-lex)))
       ((eq token 'name)
	(unless follower
	  (lua2-parse-reference-current-name)))
       ((lua2-parse-symbol "{" token)
	(lua2-parse-tableconstructor))
       ((lua2-parse-symbol "(" token)
	(lua2-parse-exp)
	(lua2-parse-symbol ")"))
       ((and terminator (lua2-parse-symbol terminator token))
	(setq terminator-seen t))
       ((or (eq token 'not)
	    (lua2-parse-symbol "-" token)
	    (lua2-parse-symbol "#" token))
	(lua2-parse-exp))
       (t (lua2-lex-set-error "expression expected")))
      (if terminator-seen
	  (setq scan nil)
	(setq scan (lua2-parse-exp-post))
	(setq follower (eq scan 'follower))
	(when scan
	  (setq token (lua2-lex)))))
    terminator-seen))

(defun lua2-parse-exp-post ()
  "Parse post-fix expression constituents while at a generalized binop."
  (let (result)
    (while (and (not result) (lua2-lex-looking-at-binop))
      (let ((token (lua2-lex)))
	(cond
	 ((eq token 'string) nil)
	 ((lua2-parse-symbol "(" token)
	  (lua2-parse-explist ")"))
	 ((lua2-parse-symbol "[" token)
	  (lua2-parse-exp)
	  (lua2-parse-symbol-with-error "]"))
	 ((lua2-parse-symbol "{" token)
	  (lua2-parse-tableconstructor))
	 ((or (lua2-parse-symbol "." token)
	      (lua2-parse-symbol ":" token))
	  (setq result 'follower))
	 (t (setq result t)))))
    result))

(defun lua2-parse-tableconstructor ()
  (let ((scan t)
	(expect-separator t))
    (while scan
      (let ((token (lua2-lex)))
	(cond
	 ((eq token 'name)
	  (cond
	   ((lua2-lex-looking-at-binop)
	    (lua2-parse-exp nil nil token))
	   ((lua2-lex-peek (lua2-parse-symbol "="))
	    (lua2-lex)
	    (lua2-parse-exp))
	   (t (lua2-parse-reference-current-name))))
	 ((lua2-parse-symbol "}" token)
	  (setq scan nil))
	 ((lua2-parse-symbol "[" token)
	  (lua2-parse-exp)
	  (lua2-parse-symbol-with-error "]")
	  (lua2-parse-symbol-with-error "=")
	  (lua2-parse-exp))
	 ((eq token 'eof)
	  (lua2-lex-set-error "end of file in table constructor")
	  (setq scan nil))
	 (t (lua2-parse-exp nil nil token)))
	(when scan
	  (let ((token (lua2-lex)))
	    (cond
	     ((lua2-parse-symbol "}" token)
	      (setq scan nil))
	     ((or (lua2-parse-symbol "," token)
		  (lua2-parse-symbol ";" token)))
	     (t (lua2-lex-set-error "field separator expected"))))))))
  nil)
  
(defun lua2-parse-prefixexp (name-overlay in-var &optional comma-seen)
  "Parse a varlist or a prefixexp.
If NAME-OVERLAY is not nil, it is an overlay for which an
assignment flag may have to be set.  The flag IN-VAR indicates
whether we are in a potential var/varlist syntax element.  The
flag COMMA-SEEEN is true if a comma has been observed previously
in this sequence."
  ;; We need an arbitrary amount of look-ahead to decide whether this
  ;; is an assignment or a function call.
  ;;
  ;; The original grammar is:
  ;;
  ;; stat         ::= varlist "=" explist
  ;;                  functioncall
  ;; varlist      ::= var {"," var}
  ;; var          ::= Name 
  ;;                  prefixexp "[" exp "]"
  ;;                  prefixexp "." Name
  ;; prefixexp    ::= var
  ;;                  functioncall
  ;;                  "(" exp ")"
  ;; functioncall ::= prefixexp args
  ;;                  prefixexp ":" Name args
  ;; args         ::= "(" [explist] ")"
  ;;                  tableconstructor
  ;;                  String
  ;;
  ;; We transform this to (with var and args as above):
  ;;
  ;; stat         ::= var {"," var} "=" explist
  ;;                  prefixexp args
  ;;                  prefixexp ":" Name args
  ;; prefixexp    ::= Name 
  ;;                  prefixexp "[" exp "]"
  ;;                  prefixexp "." Name
  ;;                  prefixexp args
  ;;                  prefixexp ":" Name args
  ;;                  "(" exp ")"
  ;;
  ;; We use in-var and comma-seen to disambiguate between var and and
  ;; prefixexp.
  (when name-overlay
    (let ((name (buffer-substring-no-properties
		 (overlay-start name-overlay) (overlay-end name-overlay))))
      (overlay-put name-overlay
		   'lua2-reference (lua2-parse-lookup name))))
  (let ((scan t))
    (while scan
      (let ((token (lua2-lex)))
	(cond
	 ;; varlist
	 ((lua2-parse-symbol "=" token)
	  (unless in-var
	    (lua2-lex-set-error "\"=\" unexpected"))
	  (when name-overlay
	    (overlay-put name-overlay 'lua2-assign t))
	  (lua2-parse-explist)
	  (setq scan nil))
	 ((lua2-parse-symbol "," token)
	  (unless in-var
	    (lua2-lex-set-error "\"=\" expected"))
	  (when name-overlay
	    (overlay-put name-overlay 'lua2-assign t))
	  (setq comma-seen t)
	  (let ((token (lua2-lex)))
	    (cond
	     ((eq token 'name)
	      (lua2-parse-reference-current-name)
	      (setq name-overlay (lua2-lex-make-overlay)))
	     ((lua2-parse-symbol "(" token)
	      (lua2-parse-explist ")")
	      (setq in-var nil))
	     (t
	      (lua2-lex-set-error "name expected")
	      (setq scan nil)))))
	 ;; functioncall
	 ((lua2-parse-symbol "(" token)
	  (lua2-parse-explist ")")
	  (setq in-var nil)
	  (setq scan (lua2-lex-looking-at-postargs)))
	 ((lua2-parse-symbol "{" token)
	  (lua2-parse-tableconstructor)
	  (setq in-var nil)
	  (setq scan (lua2-lex-looking-at-postargs)))
	 ((eq token 'string)
	  (setq in-var nil)
	  (setq scan (lua2-lex-looking-at-postargs)))
	 ((lua2-parse-symbol ":" token)
	  (unless (eq (lua2-lex) 'name)
	    (lua2-lex-set-error "name expected"))
	  (let ((token (lua2-lex)))
	    (cond
	     ((lua2-parse-symbol "(" token)
	      (lua2-parse-explist ")"))
	     ((lua2-parse-symbol "{" token)
	      (lua2-parse-tableconstructor))
	     ((eq token 'string))
	     (t (lua2-lex-set-error "function call arguments expected"))))
	  (setq in-var nil)
	  (setq scan (lua2-lex-looking-at-postargs)))
	 ;; postfix operations
	 ((lua2-parse-symbol "[" token)
	  (lua2-parse-exp)
	  (lua2-parse-symbol-with-error "]")
	  (setq name-overlay nil)
	  (setq in-var t))
	 ((lua2-parse-symbol "." token)
	  (unless (eq (lua2-lex) 'name)
	    (lua2-lex-set-error "name expected"))
	  (setq name-overlay nil)
	  (setq in-var t))
	 (t
	  (lua2-lex-set-error "\"=\" expected")
	  (setq scan nil))))))
    nil)

(defun lua2-parse-block-no-open (end-tokens)
  (let ((scan t)
	lua2-parse-unreachable-code token)
    (while scan
      (let ((unreachable lua2-parse-unreachable-code)
	    (start (point)))
	(setq token (lua2-parse-stat))
	(if token
	    (progn
	      (setq scan nil)
	      (unless (memq token end-tokens)
		(lua2-lex-set-error "statement expected, got %S" token)))
	  (when unreachable
	    (lua2-lex-set-error-region start (point) "unreachable code")))
	(when (looking-at "[ \t\n\r]*;")
	  (goto-char (match-end 0)))))
    token))

(defun lua2-parse-block (end-tokens)
  (lua2-parse-open-block)
  (prog1
      (lua2-parse-block-no-open end-tokens)
    (lua2-parse-close-block)))
    
(defun lua2-parse-chunk ()
  (lua2-parse-block '(eof))
  nil)

(defun lua2-clean-buffer ()
  (interactive)
  (setq lua2-errors nil)
  ;; FIXME: Removes other overlays, too.
  (remove-overlays))

(defun lua2-parse-buffer ()
  (interactive)
  (let (lua2-lex-current-value 
	lua2-lex-current-beginninlg
	lua2-parse-block-list)
    (save-excursion
      (lua2-clean-buffer)
      (goto-char (point-min))
      (when (looking-at "# ?!")
	(forward-line))
      (save-match-data
	(lua2-parse-chunk)))))

(defun lua2-highlight-region (overlay reference)
  (cond
   ((eq reference 'bind)
    (overlay-put overlay 'face 'lua2-bind-variable))
   ((eq reference 'global)
    (overlay-put overlay 'face
		 (if (get-char-property (overlay-start overlay) 'lua2-assign)
		     'lua2-assign-global-variable
		   'lua2-reference-global-variable)))
   ((numberp reference)
    (overlay-put overlay 'face 
		 (if (get-char-property (overlay-start overlay) 'lua2-assign)
		     'lua2-assign-variable
		   'lua2-reference-variable)))))

(defun lua2-next-single-overlay-change (pos symbol)
  (let ((old-value (get-char-property pos symbol))
	result)
    (while pos
      (setq pos (next-overlay-change pos))
      (if (< pos (point-max))
	  (when (not (eq (get-char-property pos symbol) old-value))
	    (setq result pos)
	    (setq pos nil))
	(setq pos nil)))
    result))

(defun lua2-foreach-text-property (property function &optional pos)
  "Iterates over all overlays with PROPERTY, starting at POS.
POS defaults to the first position in the buffer.
FUNCTION is called with two arguments: the overlay and the value
of the property."
  (setq pos (or pos (point-min)))
  (while pos
    (let ((reference (get-char-property pos property)))
      (when (or reference
		(and (setq pos (lua2-next-single-overlay-change
				pos property))
		     (setq reference (get-char-property pos property))))
	(let
	    ((end (lua2-next-single-overlay-change pos property)))
	  (funcall function (make-overlay pos (or end (point-max)))
		   reference)
	  (setq pos end))))))

(defun lua2-highlight-buffer ()
  (interactive)
  (lua2-foreach-text-property 'lua2-reference 'lua2-highlight-region)
  (when lua2-errors
    (lua2-foreach-text-property 'lua2-error
				(lambda (overlay value)
				  (overlay-put overlay 'face
					       'compilation-error)))))

(defvar lua2-inhibit-updates nil
  "This variable is bound to true if semantic information for the
buffer should not be updated during edits.")

(defun lua2-update-buffer ()
  (unless lua2-inhibit-updates
    ;; Increase the Lisp eval depth so that we do not have to rely on
    ;; bytecode compilation (and tail recursion eliminate) to parse
    ;; moderately complex expressions.
    (let ((max-lisp-eval-depth (max max-lisp-eval-depth 5000)))
      (lua2-clean-buffer)
      (lua2-parse-buffer)
      (lua2-highlight-buffer))))


;;; Buffer navigation

(defun lua2-goto-definition (pos)
  "Jumps to the definition of the variable reference at POS.
If called interactively, jumps to the definition of the reference at point."
  (interactive "d")
  (lua2-update-buffer)
  (let ((reference (get-char-property pos 'lua2-reference)))
    (cond 
     ((numberp reference) (goto-char reference))
     ((eq reference 'global) (error "This is a global varible"))
     (t (error "Point not at variable reference")))))

(defun lua2-goto-next-error (pos)
  "Jumps to the next error after POS, or point if called interactively.
Returns true if an error was found.  Errors out in interactive mode
when no more errors are found."
  (interactive "d")
  (catch 'lua2-goto-next-error
    (when lua2-errors
      (lua2-foreach-text-property 'lua2-error
				  (lambda (overlay value)
				    (goto-char (overlay-start overlay))
				    (throw 'lua2-goto-next-error t))))
    (when (called-interactively-p)
      (error "No more errors"))))


;;; Refactoring

(defun lua2-rename-variable (pos new-name)
  "Renames the local variable at POS to NEW-NAME.
If called interactively, rename the variable at point and prompt
for the new name."
  (interactive "*d
MNew name: ")
  (save-excursion
    (let* ((reference (get-char-property pos 'lua2-reference))
	   (old-name 
	    (cond 
	     ((numberp reference)
	      (goto-char reference)
	      (if (looking-at lua2-lex-name-regexp)
		  (buffer-substring-no-properties
		   (match-beginning 0) (match-end 0))
		(error "Internal error: no name binding")))
	     ((eq reference 'global) (error "This is a global varible"))
	     (t (error "Point not at variable reference"))))
	   (lua2-inhibit-updates t))
      (goto-char (point-max))
      (while (re-search-backward old-name (1- reference) t)
	(when (or (eq reference (point))
		  (eq reference (get-char-property (point) 'lua2-reference)))
	  (replace-match new-name)
	  (goto-char (match-beginning 0)))
	(backward-char))))
  (lua2-update-buffer))


;;; Setup code

(require 'lua-mode)

(defun lua2-on-buffer-change (begin end old-length)
  (lua2-update-buffer))

(defun lua2-on-lua-mode ()
  (define-key lua-mode-map "\C-c\C-d" 'lua2-goto-definition)
  (define-key lua-mode-map "\C-c\C-r" 'lua2-rename-variable)
  (add-hook 'after-change-functions 'lua2-on-buffer-change nil t)
  (lua2-on-buffer-change (point-min) (point-max) 0))
 
(add-hook 'lua-mode-hook 'lua2-on-lua-mode)

(when nil
  (let (list)
    (mapatoms (lambda (sym)
		(when (and (fboundp sym)
			   (string-match "^lua2-" (symbol-name sym)))
		  (setq list (cons sym list)))))
    (mapc 'trace-function list)))

(provide 'lua2-mode)
