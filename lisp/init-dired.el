;;; init-dired.el --- Configure dired -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:

(with-eval-after-load 'dired
  (setq-default dired-dwim-target t)
  (require 'dired-x)
  ;; Hook up dired-x global bindings without loading it up-front
  (define-key ctl-x-map "\C-j" 'dired-jump)
  (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
  ;; otherwise, when using evil norm state in dired-mode
  ;; enter will be move to the next line.
  
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'dired-mode-map
   "RET" 'dired-find-file
   "m" 'dired-mark
   "D" 'dired-do-delete
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
  :config
  (with-eval-after-load 'dired
    (dired-quick-sort-setup))
  )

(provide 'init-dired)
;;; init-dired.el ends here
