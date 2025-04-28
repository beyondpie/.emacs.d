;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; - spacemacs-theme setting
;; https://github.com/nashamri/spacemacs-theme/issues/42
;; https://github.com/nashamri/spacemacs-theme
;; - modus setting: https://protesilaos.com/modus-themes
;; NOTE: Under TUI(terminal, for example, iTerm2), cursors are controled
;; by it not by emacs.

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
;;   (modus-themes-load-themes))

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

(defun load-modus-dark-theme ()
  "Load modus dark theme."
  (interactive)
  (modus-themes-load-vivendi)
  (beyondpie/set-evil-insert-state-cursor)
  )

(defun update-evil-cursor (&rest args)
  "Update evil cursor color after selecting a theme: ARGS."
  (print args)
  (beyondpie/set-evil-insert-state-cursor)
  )
(advice-add 'ef-themes-select :after #'update-evil-cursor)
(advice-add 'load-theme :after #'update-evil-cursor)
;; (advice-remove 'ef-themes-select #'update-evil-cursor)
;; (advice-remove 'ef-themes-select #'beyondpie/set-evil-insert-state-cursor)

;; http://xahlee.info/emacs/emacs/emacs_customize_default_window_size.html
(defun beyondpie/setgui ()
  (interactive)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq-default
   window-resize-pixelwise t
   frame-resize-pixelwise t)

  (scroll-bar-mode -1)
  (defun my/disable-scroll-bars (frame)
    (modify-frame-parameters frame
                             '((vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil))))
  (add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  (progn
    (set-face-attribute 'default nil :font "Monaco-16")
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))
    )

  ;; Non-zero values for `line-spacing' can mess up ansi-term and co,
  ;; so we zero it explicitly in those cases.
  (add-hook 'term-mode-hook
            (lambda ()
              (setq line-spacing 0)))
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font "Monaco-16")
        (setq initial-frame-alist
              '(
                (tool-bar-lines . 0)
                (width . 106) ; chars
                (height . 60) ; lines
                (ns-transparent-titlebar . t)
                ))
        (setq default-frame-alist
              '(
                (tool-bar-lines . 0)
                (width . 106)
                (height . 60)
                (ns-transparent-titlebar . t)
                )))
    )
  (beyondpie/set-evil-insert-state-cursor)
  )
(provide 'init-themes)
;;; init-themes.el ends here
