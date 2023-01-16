;;; init-lsp.el --- Initialize LSP configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Seagle
;; Ref: https://emacs-lsp.github.io/lsp-mode/page/main-features/
;; Ref: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/


;;; Code:

(use-package lsp-mode
  ;; :load-path ("~/.emacs.d/lsp-mode" "~/.emacs.d/lsp-mode/clients")
  :delight
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
	      lsp-keep-workspace-alive t
        lsp-enable-snippet t
        lsp-enable-xref t
	      lsp-signature-auto-activate t
        lsp-signature-render-documentation t
	      lsp-modeline-code-actions-enable t
        lsp-diagnostics-provider :flymake
	      lsp-modeline-diagnostics-enable t
	      lsp-modeline-workspace-status-enable nil
        ;; https://emacs-china.org/t/spacemacs-c-c/15695
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	      lsp-enable-file-watchers nil
	      lsp-enable-folding t
        lsp-semantic-tokens-enable t
	      lsp-enable-symbol-highlighting t
	      lsp-enable-text-document-color t
        lsp-enable-imenu t
        lsp-eldoc-enable-hover t
	      lsp-enable-indentation t
	      lsp-enable-on-type-formatting nil
        lsp-idle-delay 0.200
        lsp-log-io nil
        )
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  lsp
  )

(use-package helm-lsp
  :commands (helm-lsp-workspace-symbol
             helm-lsp-global-workspace-symbol
             helm-lsp-code-actions
             helm-lsp-switch-project
             )
  :config
  (define-key lsp-mode-map
    [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
