;;; init-git.el --- Git support -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Purcell

;;; Code:
(use-package magit
  :ensure t
  :pin melpa
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "gf" '(magit-file-dispatch :which-key "magit-file-dispatch")
           "gs" '(magit-status :which-key "magit-status")
           "gd" '(magit-dispatch :which-key "magit-dispatch")
           )
  :bind ("C-x g" . magit-status)
        )
(provide 'init-git)
;;; init-git.el ends here
