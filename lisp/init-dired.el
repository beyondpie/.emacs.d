;;; init-dired.el --- Configure dired -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:

(with-eval-after-load 'dired
  (setq-default dired-dwim-target t)
  ;; https://emacs-china.org/t/emacs/23850/8
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (require 'dired-x)
  ;; Hook up dired-x global bindings without loading it up-front
  (define-key ctl-x-map "\C-j" 'dired-jump)
  (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'dired-mode-map
   "RET" 'dired-find-file
   "m" 'dired-mark
   "D" nil
   "d" nil
   "g" 'revert-buffer
   "u" 'dired-unmark
   "+" 'dired-create-directory
   "C" 'dired-do-copy
   "R" 'dired-do-rename
   "(" 'dired-hide-details-mode
   "!" 'dired-do-shell-command
   "S" 'hydra-dired-quick-sort/body
   ))

(use-package diff-hl
  :pin melpa
  :hook (dired-mode . diff-hl-dired-mode)
  )

(use-package dired-quick-sort
  :pin melpa
  :init
  (setq dired-quick-sort-suppress-setup-warning t)
  :config
  (with-eval-after-load 'dired
    (dired-quick-sort-setup))
  )

(use-package all-the-icons
  :pin melpa
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :diminish
  :pin melpa
  :init
  (setq all-the-icons-dired-monochrome nil)
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(provide 'init-dired)
;;; init-dired.el ends here
