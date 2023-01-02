;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; https://github.com/nashamri/spacemacs-theme
;; modus setting
;; https://protesilaos.com/modus-themes

;;; Code:

;; (use-package modus-themes
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-slanted-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-syntax nil
;;         ;; modus-themes-line '3d
;;         modus-themes-subtle-line-numbers nil
;;         ;; modus-themes-paren-match 'subtle-bold
;;         )

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package ef-themes)

(defun enable-transparent (alpha)
  "Set current frame transparency.
ALPHA is numeric value from [0, 100]."
  (interactive "nalpha:")
  (set-frame-parameter (selected-frame) 'alpha alpha)
  )

(defun default-light-theme ()
  "Use EMACS default light theme."
  (interactive)
  (set-foreground-color "black")
  (set-background-color "white")
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (beyondpie/set-evil-insert-state-cursor)
  )
(defun default-dark-theme ()
  "Use EMACS default dark theme."
  (interactive)
  (set-foreground-color "white")
  (set-background-color "black")
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (beyondpie/set-evil-insert-state-cursor)
  )

;; (defun load-modus-dark-theme ()
;;   "Load modus dark theme."
;;   (interactive)
;;   (modus-themes-load-vivendi)
;;   (beyondpie/set-evil-insert-state-cursor)
;;   )

(defun update-evil-cursor (&rest args)
  "Update evil cursor color after selecting a theme: ARGS."
  (print args)
  (beyondpie/set-evil-insert-state-cursor)
  )
(advice-add 'ef-themes-select :after #'update-evil-cursor)
;; (advice-remove 'ef-themes-select #'update-evil-cursor)
;; (advice-remove 'ef-themes-select #'beyondpie/set-evil-insert-state-cursor)

(provide 'init-themes)
;;; init-themes.el ends here
