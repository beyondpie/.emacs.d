;;; init-scala.el --- Initialize scala configurations. -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(use-package ammonite-term-repl
  :init
  (slot/vc-install :fetcher "github" :repo "zwild/ammonite-term-repl")
  (setq ammonite-term-repl-auto-detect-predef-file nil)
  )

;; use treesit-install-language-grammar to install scala tree-sitter
(use-package scala-ts-mode
  :init
  (slot/vc-install :fetcher "github" :repo "KaranAhlawat/scala-ts-mode")
  :interpreter
  ("scala" . scala-ts-mode)
  :hook
  (
   (scala-ts-mode . (lambda() (ammonite-term-repl-minor-mode t)))
   )
  :general
  (:states '(normal)
           :keymaps 'scala-ts-mode-map
           :prefix beyondpie/major-mode-leader-key
           "'" '(ammonite-term-repl :which-key "scala repl")
           "sf" '(ammonite-term-repl-send-defun :which-key "eval fun")
           "sr" '(ammonite-term-repl-send-region :which-key "eval region")
           "lf" '(ammonite-term-repl-load-file :which-key "load file"))

  )

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :hook
  (
   (scala-mode . (lambda() (ammonite-term-repl-minor-mode t)))
   )
  :general
  (:states '(normal)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "'" '(ammonite-term-repl :which-key "scala repl")
           "sf" '(ammonite-term-repl-send-defun :which-key "eval fun")
           "sr" '(ammonite-term-repl-send-region :which-key "eval region")
           "lf" '(ammonite-term-repl-load-file :which-key "load file"))
  )

(provide 'init-scala)
;;; init-scala.el ends here


