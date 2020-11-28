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

;; for mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;; for tabs
(setq-default indent-tabs-mode nil
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
;; (setq evil-want-keybinding nil)
(require-package 'evil)
(evil-mode 1)

;; evil-collection make SPC setting failed ...

;; https://github.com/emacs-evil/evil-collection
;; (require-package 'evil-collection)
;; (when (require 'evil-collection nil t)
;;   (evil-collection-init))

;; remove M-. binded by evil normal state.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-.") nil))

;;  override normal state with dired’s keybindings, you could do this:
;; The latter is what evil does by default (followed by an evil-add-hjkl-bindings).
(with-eval-after-load 'dired
  (evil-make-overriding-map dired-mode-map 'normal)
  )

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

(define-key evil-emacs-state-map [escape] 'evil-normal-state)

(setq evil-mode-line-format nil
      evil-insert-state-cursor '(bar "White")
      evil-visual-state-cursor '(box "#F86155")
      evil-normal-state-cursor '(box "Orange"))

;; undo-tree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; doom-mode line

(require-package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon nil)

;; for major-mode
(require 'init-const)
(global-set-key (kbd beyondpie/non-normal-leader-key) #'major-mode-hydra)



(provide 'init-utils)
