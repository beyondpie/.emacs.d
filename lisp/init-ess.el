;;; init-ess.el --- set R env. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; TODO: have to use require lsp-mode
(require 'lsp-mode)
(use-package ess
  :ensure t
  :pin melpa
  :init
  :requires lsp-mode
  :config
  (setq ess-indent-offset 2
        ess-style 'RStudio
        ess-fancy-comments nil)
  :hook (ess-r-mode . lsp)
  :general
  (:states '(normal visual)
   :keymaps 'ess-r-mode-map
   :prefix beyondpie/major-mode-leader-key
   "sl" '(ess-eval-line :which-key "eval send line")
   "sf" '(ess-eval-function :which-key "eval send function")
   "sr" '(ess-eval-region :which-key "eval send region")
   "gg" '(lsp-find-definition :which-key "lsp find definition")
   "gf" '(helm-semantic-or-imenu :which-key "helm search semantic")
   "go" '(helm-occur :which-key "helm occur")
   "gm" '(helm-all-mark-rings :which-key "helm all mark rings")
   "rn" '(lsp-rename :which-key "lsp rename")
   "'" '(R :which-key "start repl"))
  (:states '(insert emacs)
   :keymaps 'ess-r-mode-map
   "-" '(ess-insert-assign :which-key "ess-assign")
   )
  (:keymaps 'inferior-ess-r-mode-map
   "C-l" '(comint-clear-buffer :which-key "clear console")))


;; use of stan
(use-package stan-mode
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
  :ensure t
  :pin melpa
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is
  (setq stan-indentation-offset 2))

(use-package company-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/company-stan/"
  :ensure t
  :pin melpa
  :after stan-mode
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

(use-package eldoc-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/eldoc-stan/"
  :ensure t
  :pin melpa
  :after stan-mode
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )


(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :ensure t
  :pin melpa
  :after stan-mode
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
  :ensure t
  :pin melpa
  :after stan-mode
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

(provide 'init-ess)
;;; init-ess.el ends here
