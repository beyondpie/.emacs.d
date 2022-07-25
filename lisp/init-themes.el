;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; https://github.com/nashamri/spacemacs-theme
;; modus setting
;; https://protesilaos.com/modus-themes

;;; Code:
(set-face-attribute 'default nil :height 200)

<<<<<<< HEAD
(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme)
=======
(use-package spacemacs-common
  :ensure spacemacs-theme
  :pin melpa
  :defer t
  :init
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '((bg2 . "#292b2e")
                            (bg1 . "#212026"))))
>>>>>>> 926ea54fb369363506494458e8a682b9766d0e40
  )

  ;; Add all your customizations prior to loading the themes
<<<<<<< HEAD
(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax nil
      modus-themes-line '3d
      modus-themes-subtle-line-numbers t
      ;; modus-themes-paren-match 'subtle-bold
      )
=======
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax nil
        ;; modus-themes-line '3d
        modus-themes-subtle-line-numbers t
        ;; modus-themes-paren-match 'subtle-bold
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :bind ("<f5>" . modus-themes-toggle))
>>>>>>> 926ea54fb369363506494458e8a682b9766d0e40

(add-hook 'after-init-hook
          (lambda ()
            ;; load spacemacs-dark
<<<<<<< HEAD
	    (if (display-graphic-p)
		(load-theme 'spacemacs-dark t)
	      (load-theme 'modus-vivendi t)
	      )
=======
            (load-theme 'spacemacs-dark t)
            ;; load  modus-vivendi
            ;; (modus-themes-load-vivendi)
            ;; (modus-themes-load-operandi)
            (if *is-a-mac* 
              (add-to-list 'default-frame-alist
                           '(font . "Monaco-18"))
              (set-face-attribute 'default nil :height 150)
              )
            (beyondpie/set-evil-insert-state-cursor)
>>>>>>> 926ea54fb369363506494458e8a682b9766d0e40
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            ))

(defun enable-transparent ()
  "set frame transparent"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(x x))
  )

(defun disable-transparent ()
  "set frame transparent"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  )

(provide 'init-themes)
;;; init-themes.el ends here
