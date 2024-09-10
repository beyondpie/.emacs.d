;;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package general
  :ensure t
  )
(require 'general)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :hook (after-init . which-key-mode)
  :delight
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

;; vundo
(use-package vundo
  :commands (vundo)
  :bind ("C-x u" . vundo)
  :delight
  :config
  (setq vundo-compact-display t))

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

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  )

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
)

(defun beyondpie/simplify-minibatch-emacs ()
  (interactive)
  (use-package emacs
    :delight
    (auto-fill-function " AF")
    (visual-line-mode)
    (eldoc-mode)
    (auto-revert-mode)
    (dired-mode)
    (winner-mode)
    (ess-r-mode "R")
    (windmove-mode)
    (evil-collection-unimpaired-mode)
    (Evil-Collection-unimparied-mode)
    (global-evil-collection-unimpaired-mode)
    (helm-mode)
    (tree-sitter-mode)
    (flymake-mode)
    (helm-minibuffer-history-mode)
    )
  )
(use-package minions
  :pin melpa
  :hook (after-init . minions-mode)
  )

;; hide/show modeline
;; Ref: https://bzg.fr/en/emacs-hide-mode-line/
(defvar-local hidden-mode-line-mode nil)
(setq mode-line-percent-position nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
(defun stat-keyfreq ()
  "Stat common commands I use."
  (interactive)
  (require 'keyfreq)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          forward-char
          backward-char
          previous-line
          next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

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
            (width . 106) ; chars
            (height . 60) ; lines
            (ns-transparent-titlebar . t)
            ))
    )

  ;; Non-zero values for `line-spacing' can mess up ansi-term and co,
  ;; so we zero it explicitly in those cases.
  (add-hook 'term-mode-hook
            (lambda ()
              (setq line-spacing 0)))
  )

;; === file-related ===
;; from "writing GNU Emacs Extensions"
(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn (setq buffer-read-only t)
             (message "File is a symlink."))
    ))

(add-hook 'find-file-hooks 'read-only-if-symlink)

;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)
;; view large file
(use-package vlf
  :ensure t
  :hook (after-init . (lambda () (require 'vlf-setup)))
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "fl" '(vlf :which-key "visualize large file"))
  )
;; temp stop asking file is too large when open it
(defvar my-original-large-file-threshold 10000000
  "Original value of large-file-warning-threshold.")

(defun toggle-large-file-warning ()
  "Toggle the large file warning threshold."
  (interactive)
  (if large-file-warning-threshold
      (progn
        (setq my-original-large-file-threshold large-file-warning-threshold)
        (setq large-file-warning-threshold nil)
        (message "Large file warning disabled."))
    (setq large-file-warning-threshold my-original-large-file-threshold)
    (message "Large file warning enabled.")))
(global-set-key (kbd "C-c t") 'toggle-large-file-warning)

;; === end of file-related ===

;; eww
;; Auto-rename new eww buffers
;; C-u M-x eww

(provide 'init-utils)
;;; init-utils.el ends here
