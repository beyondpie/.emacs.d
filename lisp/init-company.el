;;; init-company.el --- Completion with company -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; - Major: Seagle init-company
;;   https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el
;; - Minor: Lazycat: https://manateelazycat.github.io/emacs/2021/06/30/company-multiple-backends.html

;;; Code:

(use-package company
  :hook ((emacs-lisp-mode . company-mode)
         (ess-r-mode . company-mode)
         (inferior-ess-r-mode . company-mode)
         (python-mode . company-mode)
         (eshell-mode . company-mode)
         )  
  :delight
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
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
  )

(provide 'init-company)
;;; init-company.el ends here
