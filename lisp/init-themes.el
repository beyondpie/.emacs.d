;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42

;;; Code:

(use-package spacemacs-common
  :ensure spacemacs-theme
  :pin melpa
  :defer t
  :hook (after-init . (lambda ()
                        (load-theme 'spacemacs-dark t)
                        (add-to-list 'default-frame-alist
                                     '(font . "Monaco-16"))
                        ))
  )
(provide 'init-themes)
;;; init-themes.el ends here
