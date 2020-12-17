;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell

;;; Code:

(use-package ibuffer
  :ensure t
  :pin melpa
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))
  :bind ("C-x C-b" . ibuffer)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "bb" '(ibuffer :which-key "ibuffer"))
  )

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
