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
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   "fj" '(dired-jump :which-key "jump dired")
   "ff" '(helm-find-files :which-key "find file")
   )
  ;; install gnu ls
  (when *is-a-mac*
    (setq dired-use-ls-dired t
	  insert-directory-program "/usr/local/bin/gls"
	  dired-listing-switches "-aBhl --group-directories-first"
	  )
    )
  ;; key bindings
  ;; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
  ;; SPC in normal state
  ;; mimic spacemacs
  ;; close SPC key in dire-mode-map
  (define-key dired-mode-map (kbd "SPC") nil)
  )

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
