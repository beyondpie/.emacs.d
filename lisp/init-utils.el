;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

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

(provide 'init-utils)
