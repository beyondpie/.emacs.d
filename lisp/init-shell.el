;;; init-shell.el --- Set shells

;;; Commentary:
;; Official website: https://github.com/akermu/emacs-libvterm
;;; Code:

(use-package vterm
  :ensure t
  :pin melpa)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 "'" '(eshell :which-key "eshell")
)

(provide 'init-shell)
;;; init-shell.el ends here
