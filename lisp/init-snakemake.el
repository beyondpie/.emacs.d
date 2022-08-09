;;; init-snakemake.el --- Initialize snakemake configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: pacemacs

;;; Code:
(use-package snakemake-mode
  :defer t
  :pin melpa
  :ensure t
  :mode (("snakefile\\'" . snakemake-mode)
         ("Snakefile\\'" . snakemake-mode)
         ("Snakefile.template\\'" . snakemake-mode)
         ("snakefile.template\\'" . snakemake-mode))
  )

(provide 'init-snakemake)
;;; init-snakemake.el ends here
