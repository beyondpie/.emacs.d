;;; init-lsp.el --- Initialize LSP configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Seagle
;; Ref: https://emacs-lsp.github.io/lsp-mode/page/main-features/
;; Ref: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/


;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t
        lsp-lens-enable nil
	      lsp-keep-workspace-alive t
        lsp-enable-snippet t
        lsp-enable-xref t
	      lsp-signature-auto-activate t
        lsp-signature-render-documentation t
	      lsp-modeline-code-actions-enable nil
        lsp-diagnostics-provider :flycheck
	      lsp-modeline-diagnostics-enable t
	      lsp-modeline-workspace-status-enable nil
        ;; https://emacs-china.org/t/spacemacs-c-c/15695
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	      lsp-enable-file-watchers nil
	      lsp-enable-folding t
	      lsp-enable-semantic-highlighting t
	      lsp-enable-symbol-highlighting t
	      lsp-enable-text-document-color t
        lsp-enable-imenu t

        lsp-eldoc-enable-hover t

	      lsp-enable-indentation t
	      lsp-enable-on-type-formatting nil
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.800
        lsp-log-io nil
        )
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq-default citre-enable-capf-intergration nil)
  
  (defun lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let ((lsp-result (lsp-completion-at-point)))
      (if (try-completion
           (buffer-substring (nth 0 lsp-result)
                             (nth 1 lsp-result))
           (nth 2 lsp-result))
          lsp-result
        (citre-completion-at-point))))
  
  (defun enable-lsp-citre-capf-backend ()
    "Enable the lsp + Citre capf backend in current buffer."
    (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))
  (define-advice lsp (:around (fn &rest args) lsp-citre-capf)
    (apply fn args)
    (enable-lsp-citre-capf-backend))
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

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
