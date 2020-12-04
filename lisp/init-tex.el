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
          LaTeX-fill-break-at-separators nil)
    )
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . turn-on-reftex)
         )
  )

(use-package magic-latex-buffer
  :ensure t
  :pin melpa
  :hook (TeX-update-style . magic-latex-buffer)
  :init
  (setq magic-latex-enable-block-highlight t
        magic-latex-enable-suscript t
        magic-latex-enable-pretty-symbols t
        magic-latex-enable-block-align nil
        magic-latex-enable-inline-image nil)
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

(use-package pdf-tools
  :ensure t
  :pin melpa
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))




(provide 'init-tex)
;;; init-tex.el ends here
