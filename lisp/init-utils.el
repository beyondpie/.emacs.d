;;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package general
  :ensure t
  )
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

;; undo-tree
(use-package undo-tree
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
 :keymaps 'override
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

;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; use repeat-mode

;; https://emacs-china.org/t/emacs-builtin-mode/11937/115
;; conflict with evil-mode, and evil-collection or evil supports the repeat.

;; (use-package repeat
;;   :ensure nil
;;   :hook (after-init . repeat-mode)
;;   :custom
;;  (repeat-exit-key (kbd "RET")))

(provide 'init-utils)
;;; init-utils.el ends here
