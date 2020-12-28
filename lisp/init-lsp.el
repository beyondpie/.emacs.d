;;; init-lsp.el --- Initialize LSP configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Seagle
;; Ref: https://emacs-lsp.github.io/lsp-mode/page/main-features/


;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	      lsp-keep-workspace-alive t
        lsp-enable-snippet t
        lsp-enable-xref t
	      lsp-signature-auto-activate nil
	      lsp-modeline-code-actions-enable nil
	      lsp-modeline-diagnostics-enable nil
	      lsp-modeline-workspace-status-enable nil
        ;; https://emacs-china.org/t/spacemacs-c-c/15695
        lsp-headerline-breadcrumb-enable nil

	      lsp-enable-file-watchers nil
	      lsp-enable-folding nil
	      lsp-enable-semantic-highlighting t
	      lsp-enable-symbol-highlighting t
	      lsp-enable-text-document-color nil
        lsp-enable-imenu t

	      lsp-enable-indentation nil
	      lsp-enable-on-type-formatting nil
        read-process-output-max (* 1024 1024))
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

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

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
