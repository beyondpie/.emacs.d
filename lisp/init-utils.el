;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; some packages
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)
;; package management
(require-package 'use-package)
;; key bindings
(require-package 'general)
(require-package 'hydra)
(require-package 'major-mode-hydra)



;; for tabs
(setq-default indent-tabs-mode t
              default-tab-width 2)

;; ** language
(ispell-change-dictionary "american" t)
(define-coding-system-alias 'UTF-8 'utf-8)


;; https://github.com/justbur/emacs-which-key
(require-package 'which-key)
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)

;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.01)

(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'bottom)
(setq which-key-side-window-max-width 0.33)
(setq which-key-side-window-max-height 0.25)
(setq which-key-max-description-length 30)

(which-key-mode)

;; evil mode
(setq evil-disable-insert-state-bindings t)
(require-package 'evil)
(evil-mode 1)

;; undo-tree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; doom-mode line

(require-package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon nil)

;; for major-mode
(global-set-key (kbd "M-SPC") #'major-mode-hydra)



(provide 'init-utils)
