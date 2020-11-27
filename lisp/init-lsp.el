;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;; ref seagle

;; have to use require to install package, use-pacakge not work
;; not know why
(require-package 'lsp-mode)
(use-package lsp-mode
			 :commands (lsp-enable-which-key-integration
									lsp-format-buffer
									lsp-organize-imports
									lsp-install-server
									lsp-deferred
									lsp)
			 :init
			 (setq lsp-keymap-prefix "C-c l"
						 lsp-keep-workspace-alive nil
						 lsp-signature-auto-activate nil
						 lsp-modeline-code-actions-enable nil
						 lsp-modeline-diagnostics-enable nil
						 lsp-modeline-workspace-status-enable nil

						 lsp-enable-file-watchers nil
						 lsp-enable-folding nil
						 lsp-enable-semantic-highlighting nil
						 lsp-enable-symbol-highlighting nil
						 lsp-enable-text-document-color nil

						 lsp-enable-indentation nil
						 lsp-enable-on-type-formatting nil)

			 (setq read-process-output-max (* 1024 1024))
			 :hook
			 ((prog-mode . (lambda() (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
																 (lsp))))
				(lsp-mode . (lambda()
											(lsp-enable-which-key-integration)))
				))

(use-package helm-lsp
	:commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
	:commands lsp-treemacs-errors-list)

(provide 'init-lsp)
