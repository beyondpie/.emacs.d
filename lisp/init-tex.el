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
         (LaTeX-mode . auto-fill-mode)
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
  (setq magic-latex-enable-block-highlight t
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

(use-package pdf-tools
  :ensure t
  :straight (pdf-tools :type git :host github :rep "politza/pdf-tools"
                      :fork (:host github :repo "beyondpie/pdf-tools"))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq auto-revert-interval 0.5)
  :general
  (:states '(normal)
           :keymaps 'pdf-view-mode-map
           "d" '(pdf-view-scroll-up-or-next-page :which-key "scroll-next")
           "u" '(pdf-view-scroll-down-or-previous-page :which-key "scroll-back")
           "b" '(pdf-view-scroll-down-or-previous-page :which-key "scroll-back")
           )
  )
(provide 'init-tex)
;;; init-tex.el ends here
