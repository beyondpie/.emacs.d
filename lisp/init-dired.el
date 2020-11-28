;; init-dired.el ---configure dired -*- lexical-binding: t -*-

(setq-default dired-dwim-target t)

(when (maybe-require-package 'diredfl)
  (with-eval-after-load 'dired
    ;; close this mode, too many colors
    ;; (diredfl-global-mode)
    (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)


(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    (define-key dired-mode-map (kbd "SPC") nil)
    ))

(require 'general)
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

(require-package 'dired-quick-sort)
(dired-quick-sort-setup)

(provide 'init-dired)
