;;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;; ref from seagle and purcell

;;; Code:
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
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t
        evil-mode-line-format nil
        evil-want-integration t
        evil-want-keybinding nil
        evil-visual-state-cursor '(box "#F86155")
        evil-normal-state-cursor '(box "Orange"))
  (defun set-evil-insert-state-cursor ()
    "change evil insert state cusor color based on theme"
    (interactive)
    (if (string= (frame-parameter nil 'background-mode) "light")
        (setq evil-insert-state-cursor '(bar "Black"))
      (setq evil-insert-state-cursor '(bar "White"))))
  :hook ((after-init . evil-mode)
         (after-init . set-evil-insert-state-cursor)
         )
  :config
  (set-evil-insert-state-cursor)
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
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           ;; https://github.com/noctuid/general.el/issues/99
           "wh" '(evil-window-left :which-key "left window")
           "wl" '(evil-window-right :which-key "right window")
           "wj" '(evil-window-down :which-key "down window")
           "wk" '(evil-window-up :which-key "up window")
           "fj" '(dired-jump :which-key "jump dired")
           "ff" '(helm-find-files :which-key "find file")
           )
  )


;; undo-tree
(use-package undo-tree
  :ensure t
  :pin melpa
  :hook (after-init . global-undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize)
)

;; doom-mode line
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t)
)

;; for major-mode
;; (global-set-key (kbd beyondpie/non-normal-leader-key) #'major-mode-hydra)

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
 "rw" '(delete-trailing-whitespace :which-key "delete trailing whitespace")
 "sr" '(eval-region :which-key "elisp eval-region")
 )

;; files
;; from "writing GNU Emacs Extensions"
(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn (setq buffer-read-only t)
             (message "File is a symlink."))
    ))

(add-hook 'find-file-hooks 'read-only-if-symlink)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-utils)
;;; init-utils.el ends here
