;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; modus setting
;; https://protesilaos.com/modus-themes


;;; Code:
(set-face-attribute 'default nil :height 200)

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme)
  )

  ;; Add all your customizations prior to loading the themes
(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax nil
      modus-themes-line '3d
      modus-themes-subtle-line-numbers t
      ;; modus-themes-paren-match 'subtle-bold
      )

(add-hook 'after-init-hook
          (lambda ()
            ;; load spacemacs-dark
	    (if (display-graphic-p)
		(load-theme 'spacemacs-dark t)
	      (load-theme 'modus-vivendi t)
	      )
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            ))

(defun enable-transparent ()
  "set frame transparent"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  )

(defun disable-transparent ()
  "set frame transparent"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  )

(provide 'init-themes)
;;; init-themes.el ends here
