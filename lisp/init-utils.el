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


;; for mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;; for tabs
(setq-default indent-tabs-mode nil
              default-tab-width 2
              tab-width 2)
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

;; remove compling in the mode-line
;; ref: https://emacs.stackexchange.com/questions/61957/mode-line-always-shows-compiling-after-compile-a-tex-file-with-typos?newreg=6aa1e0e4e19b423a9bce34c66bacc1e4
(setq compilation-in-progress nil)

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
    ;; (flymake-mode)
    (evil-collection-unimpaired-mode)
    (Evil-Collection-unimparied-mode)
    (global-evil-collection-unimpaired-mode)
    (helm-mode)
    (tree-sitter-mode)
    (flymake-mode)
    (flycheck-mode)
    (helm-minibuffer-history-mode)
    )
  )
(use-package minions
  :pin melpa
  :hook (after-init . minions-mode)
  )

;; remove up/down case keys due to they usually make my codes typo
;; upcase-region
(global-unset-key (kbd "C-x C-u"))
;; upcase-word
(global-unset-key (kbd "M-u"))
;; downcase-word
(global-unset-key (kbd "M-l"))
;; downcase-region
(global-unset-key (kbd "C-x C-l"))

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

;; delete other windows while keeping imenu
;; https://emacs-china.org/t/c-x-1-imenu-list-buffer/23162/4
(require 'dash)
(defun delete-other-windows-exclude-imenu-list  ()
  (interactive)
  (->> (window-list)
       (-filter
        (lambda (win)
          (and
           (not (equal "*Ilist*" (buffer-name (window-buffer win))))
           (not (eq  (selected-window)  win))
           )
          )
        )
       (-map
        (lambda (bekill-win)
          (delete-window bekill-win)
          )
        )
       )
  )

(defun delete-other-windows-R-repl ()
  (interactive)
  (->> (window-list)
       (-filter
        (lambda (win)
          (and
           (not (string-match "*R:" (buffer-name (window-buffer win))))
           (not (eq  (selected-window)  win))

           )
          )
        )
       (-map
        (lambda (bekill-win)
          (delete-window bekill-win)
          )
        )
       )
  )


;; https://stackoverflow.com/questions/18325973/a-smarter-alternative-to-delete-window
(defun delete-extra-windows ()
  (interactive)
  (let* ((selwin  (selected-window))
         (buf     (window-buffer selwin)))
    (walk-windows (lambda (ww)
                    (unless (eq ww selwin)
                      (when (eq (window-buffer ww) buf)
                        (delete-window ww))))
                  'NO-MINI 'THIS-FRAME)))

;; (global-set-key (kbd "C-x 1") '
;; delete-other-windows-exclude-imenu-list)

(eval-after-load "flymake"
  '(progn
     (defun flymake-after-change-function (start stop len)
      "Start syntax check for current buffer if it isn't already running."
      ;; Do nothing, don't want to run checks until I save.
      )))

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

;; eww
;; Auto-rename new eww buffers
;; C-u M-x eww

;;; Commentary:
;; ref:
;; https://github.com/necaris/conda.el
(use-package conda
  :delight
  :init
  (if (eq system-type 'darwin)
      (setq conda-anaconda-home (expand-file-name "~/mambaforge"))
    (if (string= system-name "mediator.sdsc.edu")
        (setq conda-anaconda-home (expand-file-name "~/miniforge3"))
      )
    )
  :hook
  ((find-file . (lambda ()
                  (when (bound-and-true-p conda-project-env-path)
                    (conda-env-activate-for-buffer))))
   (prog-mode . (lambda ()
                  (setq mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))))
   )
  :config
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)
  :commands
  (conda-env-activate
   conda-env-deactivate
   conda-env-activate-for-buffer
   )
  )

(define-key global-map (kbd "M-j") nil)
(define-key global-map (kbd "M-k") nil)

(provide 'init-utils)
;;; init-utils.el ends here
