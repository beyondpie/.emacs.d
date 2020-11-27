;; init-ess.el --- set R env. -*- lexical-binding: t -*-

(require-package 'ess)
(require 'lsp-mode)
(add-hook 'ess-r-mode-hook (lambda() (lsp)))

;; use of stan
(require-package 'stan-mode)
(require-package 'company-stan)
(require-package 'eldoc-stan)
(require-package 'flycheck-stan)
(require-package 'stan-snippets)

(defun beyondpie/init-stan-mode ()
  (use-package stan-mode
    ;; Uncomment if directly loading from your development repo
    ;; :load-path "your-path/stan-mode/stan-mode"
    :mode ("\\.stan\\'" . stan-mode)
    :hook (stan-mode . stan-mode-setup)
    ;;
    :config
    ;; The officially recommended offset is
    (setq stan-indentation-offset 2))
  )

(defun beyondpie/init-company-stan ()
  (use-package company-stan
    ;; Uncomment if directly loading from your development repo
    ;; :load-path "your-path/stan-mode/company-stan/"
    :hook (stan-mode . company-stan-setup)
    ;;
    :config
    ;; Whether to use fuzzy matching in `company-stan'
    (setq company-stan-fuzzy nil))
  )

(defun beyondpie/init-eldoc-stan ()
  (use-package eldoc-stan
    ;; Uncomment if directly loading from your development repo
    ;; :load-path "your-path/stan-mode/eldoc-stan/"
    :hook (stan-mode . eldoc-stan-setup)
    ;;
    :config
    ;; No configuration options as of now.
    )
  )


(defun beyondpie/init-flycheck-stan ()
  (use-package flycheck-stan
    ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
    :hook ((stan-mode . flycheck-stan-stanc2-setup)
           (stan-mode . flycheck-stan-stanc3-setup))
    :config
    ;; A string containing the name or the path of the stanc2 executable
    ;; If nil, defaults to `stanc2'
    (setq flycheck-stanc-executable nil)
    ;; A string containing the name or the path of the stanc2 executable
    ;; If nil, defaults to `stanc3'
    (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
  (use-package stan-snippets
    ;; Uncomment if directly loading from your development repo
    ;; :load-path "your-path/stan-mode/stan-snippets/"
    :hook (stan-mode . stan-snippets-initialize)
    ;;
    :config
    ;; No configuration options as of now.
    )
  )


(defun beyondpie/init-stan-snippets ()
  ;;; stan-snippets.el
  (use-package stan-snippets
    ;; Uncomment if directly loading from your development repo
    ;; :load-path "your-path/stan-mode/stan-snippets/"
    :hook (stan-mode . stan-snippets-initialize)
    ;;
    :config
    ;; No configuration options as of now.
    )

  )

(provide 'init-ess)
