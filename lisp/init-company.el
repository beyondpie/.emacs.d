;;; init-company.el --- Completion with company -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package company
  :hook ((emacs-lisp-mode . company-mode)
         (ess-r-mode . company-mode)
         (inferior-ess-r-mode . company-mode)
         (python-mode . company-mode)
         (eshell-mode . company-mode)
         (scala-mode . company-mode)
         )  
  :delight
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :bind (("M-/" . company-complete))
  :init
  ;; this will cover magit in my macOS GUI Emacs.
  ;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations nil
        company-tooltip-limit 10
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords
                                                 company-files)
                           company-dabbrev))
  (add-to-list 'completion-styles 'initials t)
  :config
  (add-to-list 'company-transformers #'delete-dups)
  ;;(setq lsp-completion-provider :capf)
  )
(provide 'init-company)
;;; init-company.el ends here
