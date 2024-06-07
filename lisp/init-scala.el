;;; init-scala.el --- Initialize scala configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref:
;; https://scalameta.org/metals/docs/editors/emacs/
;;; Code:

(use-package ensime-mode
  :ensure nil
  :load-path "~/softwares/ensime-tng-3.0.15/lisp"
  :commands ensime-mode
  :bind
  (:map ensime-mode-map
        ("M-." . ensime-jump-to-definition)
        ("C-c C-i t" . ensime-type-at-point)
        ("C-c C-i s" . ensime-symbol-at-point)
        ("C-c C-r i" . ensime-import-symbol-at-point)))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :hook (scala-mode . ensime-mode)
  :general
  (:states '(nomrmal visual)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sr" '(sbt-send-region :which-key "sbt-send-region"))
  )


;; ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;; ;; allows using SPACE when in the minibuffer
;; (substitute-key-definition
;;  'minibuffer-complete-word
;;  'self-insert-command
;; minibuffer-local-completion-map)
;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )


(provide 'init-scala)
;;; init-scala.el ends here

