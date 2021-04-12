;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; modus setting
;; https://protesilaos.com/modus-themes


;;; Code:

(use-package spacemacs-common
  :ensure spacemacs-theme
  :pin melpa
  :defer t
  )

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax nil
        modus-themes-line '3d
        modus-themes-subtle-line-numbers t
        modus-themes-paren-match 'subtle-bold
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :bind ("<f5>" . modus-themes-toggle))

(add-hook 'after-init-hook
          (lambda ()
            ;; load spacemacs-dark
            (load-theme 'spacemacs-dark t)
            ;; load  modus-vivendi
            ;; (modus-themes-load-vivendi)
            (add-to-list 'default-frame-alist
                         '(font . "Monaco-16"))
            ))

(defun enable-transparent ()
  "set frame transparent"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  )

(provide 'init-themes)
;;; init-themes.el ends here
