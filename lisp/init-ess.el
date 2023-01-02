;;; init-ess.el --- set R env. -*- lexical-binding: t -*-

;;; Commentary:

;; If we want to close the [R/none] showed in minibuffer

;; You can simply set mode-line-process to nil in ess-mode-hook and/or inferior-ess-mode-hook:
;; (setq-local mode-line-process nil)

;;; Code:

(require 'treesit)
(defvar r--treesit-keywords
  '("if" "else" "repeat" "while" "function" "for" "in" "next" "break"
    "switch" "function" "return" "on.exit" "stop" ".Defunct" "tryCatch"
    "withRestarts" "invokeRestart"
    "recover" "browser")
  "Reserved words or special functions in the R language.")

(defvar r--treesit-builtins
  '("abs" "all" "any" "ascii" "bin" "bool" "breakpoint" "bytearray"
    "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"))


(defvar r--treesit-special-attributes
  '("__annotations__" "__closure__" "__code__"
    "__defaults__" "__dict__" "__doc__" "__globals__"
    "__kwdefaults__" "__name__" "__module__" "__package__"
    "__qualname__" "__all__"))

(defvar r--treesit-operators
  '("-" "-=" "!=" "*" "**" "**=" "*=" "/" "//" "//=" "/=" "&" "%" "%="
    "^" "+" "->" "+=" "<" "<<" "<=" "<>" "=" ":=" "==" ">" ">=" ">>" "|"
    "~" "@" "@="))


(defvar r--treesit-exceptions
  '(;; Python 2 and 3:
    "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
    "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
    "GeneratorExit" "IOError" "ImportError" "ImportWarning"
    "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
    "LookupError" "MemoryError" "NameError" "NotImplementedError"
    "OSError" "OverflowError" "PendingDeprecationWarning"
    "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
    "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError"
    "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
    "UnicodeWarning" "UserWarning" "ValueError" "Warning"
    "ZeroDivisionError"
    ;; Python 2:
    "StandardError"
    ;; Python 3:
    "BlockingIOError" "BrokenPipeError" "ChildProcessError"
    "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
    "ConnectionResetError" "FileExistsError" "FileNotFoundError"
    "InterruptedError" "IsADirectoryError" "NotADirectoryError"
    "PermissionError" "ProcessLookupError" "RecursionError"
    "ResourceWarning" "StopAsyncIteration" "TimeoutError"
    ;; OS specific
    "VMSError" "WindowsError"
    ))

(defun r--treesit-fontify-string (node override start end &rest _)
  "Fontify string.
NODE is the string node.  Do not fontify the initial f for
f-strings.  OVERRIDE is the override flag described in
`treesit-font-lock-rules'.  START and END mark the region to be
fontified."
  (let* ((string-beg (treesit-node-start node))
         (string-end (treesit-node-end node))
         (maybe-expression (treesit-node-parent node))
         (grandparent (treesit-node-parent
                       (treesit-node-parent
                        maybe-expression)))
         (maybe-defun grandparent)
         (face (if (and (or (member (treesit-node-type maybe-defun)
                                    '("function_definition"
                                      "class_definition"))
                            ;; If the grandparent is null, meaning the
                            ;; string is top-level, and the string has
                            ;; no node or only comment preceding it,
                            ;; it's a BOF docstring.
                            (and (null grandparent)
                                 (cl-loop
                                  for prev = (treesit-node-prev-sibling
                                              maybe-expression)
                                  then (treesit-node-prev-sibling prev)
                                  while prev
                                  if (not (equal (treesit-node-type prev)
                                                 "comment"))
                                  return nil
                                  finally return t)))
                        ;; This check filters out this case:
                        ;; def function():
                        ;;     return "some string"
                        (equal (treesit-node-type maybe-expression)
                               "expression_statement"))
                   'font-lock-doc-face
                 'font-lock-string-face)))
    (when (eq (char-after string-beg) ?f)
      (cl-incf string-beg))
    (treesit-fontify-with-override
     string-beg string-end face override start end)))



(defvar r--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'r
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'r
   :override t
   '((string) @r--treesit-fontify-string)

   :feature 'string-interpolation
   :language 'r
   :override t
   '((interpolation (identifier) @font-lock-variable-name-face))

   :feature 'definition
   :language 'r
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (class_definition
      name: (identifier) @font-lock-type-face))

   :feature 'function
   :language 'r
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (call function: (identifier) @font-lock-function-name-face)
     (call function: (attribute
                      attribute: (identifier) @font-lock-function-name-face)))

   :feature 'keyword
   :language 'r
   `([,@r--treesit-keywords] @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "^self$" @font-lock-keyword-face)))

   :feature 'builtin
   :language 'r
   `(((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(seq bol
                      (or ,@r--treesit-builtins
                          ,@r--treesit-special-attributes)
                      eol))
              @font-lock-builtin-face)))

   :feature 'constant
   :language 'r
   '([(true) (false) (none)] @font-lock-constant-face)

   :feature 'assignment
   :language 'r
   `(;; Variable names and LHS.
     (assignment left: (identifier)
                 @font-lock-variable-name-face)
     (assignment left: (attribute
                        attribute: (identifier)
                        @font-lock-variable-name-face))
     (pattern_list (identifier)
                   @font-lock-variable-name-face)
     (tuple_pattern (identifier)
                    @font-lock-variable-name-face)
     (list_pattern (identifier)
                   @font-lock-variable-name-face)
     (list_splat_pattern (identifier)
                         @font-lock-variable-name-face))

   :feature 'decorator
   :language 'r
   '((decorator "@" @font-lock-type-face)
     (decorator (call function: (identifier) @font-lock-type-face))
     (decorator (identifier) @font-lock-type-face))

   :feature 'type
   :language 'r
   `(((identifier) @font-lock-type-face
      (:match ,(rx-to-string
                `(seq bol (or ,@r--treesit-exceptions)
                      eol))
              @font-lock-type-face))
     (type (identifier) @font-lock-type-face))

   :feature 'escape-sequence
   :language 'r
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'number
   :language 'r
   :override t
   '([(integer) (float)] @font-lock-number-face)

   :feature 'property
   :language 'r
   :override t
   '((attribute
      attribute: (identifier) @font-lock-property-face)
     (class_definition
      body: (block
             (expression_statement
              (assignment left:
                          (identifier) @font-lock-property-face)))))

   :feature 'operator
   :language 'r
   :override t
   `([,@r--treesit-operators] @font-lock-operator-face)

   :feature 'bracket
   :language 'r
   :override t
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'r
   :override t
   '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face))
  "Tree-sitter font-lock settings.")



(use-package ess
  :delight
  :ensure t
  :pin melpa
  :config
  (setq
   ess-style 'RStudio-
   ess-indent-offset 2
   ess-indent-level 2
   ess-fancy-comments nil
   ess-offset-arguments-newline '(prev-line 2)
   ess-offset-block '(prev-line 2)
   ess-offset-arguments '(prev-line 2)
   ess-indent-from-lhs '(argument fun-decl-opening)
   ess-indent-from-chain-start t
   ess-use-flymake nil
   ess-startup-directory 'default-directory
   )
  ;; :hook (ess-r-mode . lsp)
  :general
  (:states '(normal visual)
           :keymaps 'ess-r-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sl" '(ess-eval-line-and-step :which-key "eval send line")
           "sf" '(ess-eval-function :which-key "eval send function")
           "sr" '(ess-eval-region :which-key "eval send region")
           "gg" '(lsp-find-definition :which-key "lsp find definition")
           "gf" '(helm-semantic-or-imenu :which-key "helm search semantic")
           "go" '(helm-occur :which-key "helm occur")
           "gm" '(helm-all-mark-rings :which-key "helm all mark rings")
           "rn" '(lsp-rename :which-key "lsp rename")
           "rb" '(lsp-format-buffer :which-key "lsp buffer")
           "rr" '(lsp-format-region :which-key "lsp region")
           "'" '(R :which-key "start repl")
           )
  (:states '(insert emacs)
           :keymaps 'ess-r-mode-map
           "-" '(ess-insert-assign :which-key "ess-assign")
           )
  (:keymaps 'inferior-ess-r-mode-map
            "C-l" '(comint-clear-buffer :which-key "clear console")
            "-" '(ess-insert-assign :which-key "ess-assign")
            )
  (:keymaps 'ess-r-help-mode-map
            "w" nil)
  (when *is-a-mac*
    (setq inferior-R-program "/usr/local/bin/R"))
  (define-derived-mode r-ts-mode ess-r-mode "r"
    "Major mode for editing r files, using tree-sitter library.

\\{r-ts-mode-map}"
    :syntax-table ess-r-mode-syntax-table
    (when (treesit-ready-p 'r)
      (treesit-parser-create 'r)
      (setq-local treesit-font-lock-feature-list
                  '(( comment definition)
                    ( keyword string type)
                    ( assignment builtin constant decorator
                      escape-sequence number property string-interpolation )
                    ( function bracket delimiter operator)))
      (setq-local treesit-font-lock-settings r--treesit-settings)
      (setq-local treesit-defun-type-regexp (rx (or "function" "class")
                                                "_definition"))
      (setq-local treesit-defun-name-function
                  #'r--treesit-defun-name)
      (treesit-major-mode-setup)
      )
    )
  )

(provide 'init-ess)
;;; init-ess.el ends here
