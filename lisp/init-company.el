;;; init-company.el --- Completion with company -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; - Seagle init-company
;; - Lazycat: https://manateelazycat.github.io/emacs/2021/06/30/company-multiple-backends.html

;;; Code:

(use-package company-tabnine
  :ensure t)

(use-package company
  :diminish
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
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.2
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((
                            ;;company-tabnine
                            company-keywords
                            company-files
                            company-dabbrev
                            company-capf))
        tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))
  ;; quickhelp
  (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 5))
  )

(provide 'init-company)
;;; init-company.el ends here
