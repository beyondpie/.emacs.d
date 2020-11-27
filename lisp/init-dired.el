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
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(when *is-a-mac*
  (setq dired-use-ls-dired nil))

(require-package 'dired-quick-sort)
(dired-quick-sort-setup)

(provide 'init-dired)
