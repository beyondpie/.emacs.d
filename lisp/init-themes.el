;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;; my favourite themes
(require-package 'spacemacs-theme)
(require-package 'modus-operandi-theme)
(require-package 'modus-vivendi-theme)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(spacemacs-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-dark))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-light))
  (reapply-themes))

(set-frame-font "Monaco-16" nil t)
(provide 'init-themes)
