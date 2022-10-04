;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; https://github.com/nashamri/spacemacs-theme
;; modus setting
;; https://protesilaos.com/modus-themes

;;; Code:

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax nil
        ;; modus-themes-line '3d
        modus-themes-subtle-line-numbers nil
        ;; modus-themes-paren-match 'subtle-bold
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :bind ("<f5>" . modus-themes-toggle))

(add-hook 'after-init-hook
          (lambda ()
            ;; load  modus-vivendi
            (modus-themes-load-vivendi)
            (if *is-a-mac* 
              (add-to-list 'default-frame-alist
                           '(font . "Monaco-18"))
              (set-face-attribute 'default nil :height 150)
              )
            (beyondpie/set-evil-insert-state-cursor)
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            ))

(defun enable-transparent (alpha)
  "Set current frame transparency.
ALPHA is numeric value from [0, 100]."
  (interactive "nalpha:")
  (set-frame-parameter (selected-frame) 'alpha alpha)
  )

(provide 'init-themes)
;;; init-themes.el ends here
