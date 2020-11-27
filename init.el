;;; init.el ---start emacs configuration  -*- lexical-binding: t -*-

;; load the full configuration
(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))

;; garbage collection during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(dolist (dir '("site-lisp" "lisp"))
	(push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(require 'init-elpa)
(require 'init-path)

(require 'init-utils)
(require 'init-helm)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-ivy)
(require 'init-company)
(require 'init-windows)
(require 'init-git)
(require 'init-projectile)
(require 'init-dashboard)
(require 'init-treemacs)
(require 'init-osx-keys)

(require 'init-prog)
(require 'init-lsp)
(require 'init-ess)
(require 'init-python)


(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; multiple major mode
(require-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)


(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))


(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
