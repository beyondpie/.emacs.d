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
  :ensure t
  :init
  (when (display-graphic-p)
    (setq doom-modeline-icon t)
    (setq doom-modeline-hud t)
    (setq doom-modeline-window-width-limit fill-column)
    (setq doom-modeline-buffer-file-name-style 'relative-from-project)
    (setq doom-modeline-height 20)
    (setq doom-modeline-checker-simple-format nil)
    (setq doom-modeline-buffer-encoding nil)
    (setq doom-modeline-lsp nil)
    (setq doom-modeline-gnus-timer -1)
    (setq doom-modeline-env-version nil)
    (setq doom-modeline-env-enable-python nil)
    (setq doom-modeline-env-python-executable "")
    )
  (doom-modeline-mode 1)
  )

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

;; use nyan-mode
;; (use-package nyan-mode
;;   :init
;;   (setq-default nyan-animate-nyancat nil
;;                 nyan-wavy-trail nil))

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

(provide 'init-utils)
;;; init-utils.el ends here
