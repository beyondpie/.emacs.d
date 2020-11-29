;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;; ref from seagle and purcell

;;; Code:
(require-package 'scratch)
(require-package 'command-log-mode)
(use-package general)
(require 'general)
(use-package hydra)
(use-package major-mode-hydra)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.01)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-max-description-length 30)
)

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode)
  :config
  ;; (evil-mode 1)
  (with-eval-after-load 'dired
  (evil-make-overriding-map dired-mode-map 'normal))
  :bind
  (:map evil-normal-state-map
  ("M-." . nil)
;; https://github.com/noctuid/evil-guide#use-some-emacs-keybindings
;; Note that at any time you can use evil-toggle-key (C-z by default;
;; bound to evil-emacs-state) to enter emacs state or \ (bound to
;; evil-execute-in-emacs-state) to execute the next command in emacs
;; state. In emacs state, evil-toggle-key is bound to switch to the
;; previous state. This may not be what you want if you’ve entered emacs
;; state from insert state, so you may want to also bind ESC to enter
;; normal state
;; Note that in this case, attempting to rebind (kbd "ESC") will not work
;; in GUI Emacs (and will prevent meta from working if used in the
;; terminal). Currently it is not possible to bind
;; escape in emacs state for terminal Emacs (see issue #14).
  ("C-e" . move-end-of-line)
  :map evil-emacs-state-map
  ([escape] . evil-normal-state)
  )
  )

;; undo-tree
(use-package undo-tree
  :init
  (setq global-undo-tree-mode t)
)

;; doom-mode line
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t)
)

;; for major-mode
(require 'init-const)
(global-set-key (kbd beyondpie/non-normal-leader-key) #'major-mode-hydra)

;; for mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;; for tabs
(setq-default indent-tabs-mode nil
              default-tab-width 2)
;; ** language
(ispell-change-dictionary "american" t)
(define-coding-system-alias 'UTF-8 'utf-8)

;; for text edit
(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 "ir" '(indent-region :which-key "indent region")
 )

(provide 'init-utils)
;;; init-utils.el ends here
