;;; init-scala.el --- Initialize scala configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref:
;; https://scalameta.org/metals/docs/editors/emacs/
;;; Code:


;; we can load ensime but no response as expected
;; (use-package ensime-mode
;;   :ensure nil
;;   :load-path "~/softwares/ensime-tng-3.0.15/lisp"
;;   :commands ensime-mode
;;   :bind
;;   (:map ensime-mode-map
;;         ("M-." . ensime-jump-to-definition)
;;         ("C-c C-i t" . ensime-type-at-point)
;;         ("C-c C-i s" . ensime-symbol-at-point)
;;         ("C-c C-r i" . ensime-import-symbol-at-point)))

(use-package lsp-metals
  :init
  (setq lsp-metals-java-home (if (string-match "mediator" (system-name))
                                 (expand-file-name "~/miniforge3")
                               (expand-file-name "~/miniforge3/lib/jvm")))
  (setq lsp-metals-install-scala-version "3.4.2")
  (setq lsp-metals-install-version "1.3.2")
  )

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :hook (scala-mode . tree-sitter-hl-mode)
  :general
  (:states '(normal visual)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sr" '(sbt-send-region :which-key "sbt-send-region")
           "sl" '(sbt-send-line :which-key "sbt-send-line")
           "gg" '(lsp-find-implementation :which-key "lsp find imp")
           "rn" '(lsp-rename :which-key "lsp rename")
           "rf" '(lsp-format-region :which-key "lsp format")
           "rb" '(lsp-format-buffer :which-key "lsp format buffer")
           )
  )


;; ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;; ;; allows using SPACE when in the minibuffer
;; (substitute-key-definition
;;  'minibuffer-complete-word
;;  'self-insert-command
;; minibuffer-local-completion-map)
;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
(use-package sbt-mode
  :commands sbt-start sbt-command sbt-send-region sbt-send-line
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (defun sbt-send-line ()
    (interactive)
    (sbt:send-region (line-beginning-position)
                     (line-end-position)))
  )


(provide 'init-scala)
;;; init-scala.el ends here

