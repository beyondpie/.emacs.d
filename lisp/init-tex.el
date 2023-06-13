;;; init-tex.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(defun my-auctex-company-setup ()
  "Setup auctex company backend."
  (add-to-list (make-local-variable 'company-backends)
               '(company-auctex-macros company-auctex-symbols
                                       company-auctex-environments))
  (add-to-list (make-local-variable 'company-backends)
               'company-auctex-labels)
  (add-to-list (make-local-variable 'company-backends)
               'company-auctex-bibs)
  )

(defun my-reftex-company-setup ()
  "Setup reftex company backend."
  (add-to-list (make-local-variable 'company-backends)
               'company-reftex-labels)
  (add-to-list (make-local-variable 'company-backends)
               'company-reftex-citations)
  )

(use-package tex
  :ensure auctex
  :pin melpa
  :init
  (progn
    (setq TeX-command-default "LaTeX"
          TeX-auto-save t
          TeX-parse-self t
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil
          ;; prevent the title showing big fonts
          font-latex-fontify-script nil
          font-latex-fontify-sectioning 'color
          )
    )
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . auto-fill-mode)
         ;; https://emacs.stackexchange.com/questions/30147/disabling-electric-indent-mode-for-one-mode-latex-mode-only/30148#30148
         (LaTeX-mode . (lambda () (electric-indent-local-mode -1)))
         )
  :config
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-view-program-list '(("pdf-tools", "TeX-pdf-tools-sync-view"))
        )
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq-default TeX-engine 'xetex)
  )

(use-package magic-latex-buffer
  :ensure t
  :pin melpa
  :hook (TeX-update-style . magic-latex-buffer)
  :init
  (setq magic-latex-enable-block-highlight nil
        magic-latex-enable-suscript t
        magic-latex-enable-pretty-symbols t
        magic-latex-enable-block-align nil
        magic-latex-enable-inline-image nil
        magic-latex-enable-minibuffer-echo t)
  )

(use-package company-auctex
  :defer t
  :ensure t
  :pin melpa
  :hook (LaTeX-mode . my-auctex-company-setup)
  )

(use-package company-reftex
  :defer t
  :ensure t
  :pin melpa
  :hook (LaTeX-mode . my-reftex-company-setup))

(provide 'init-tex)
;;; init-tex.el ends here
