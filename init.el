;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell

;;; Code:

;; this fix error when gpg no public key on rc centos 
(setq package-check-signature nil)
(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))
;; make echo area showing message for 10s
(setq suggest-key-bindings 10)

;; garbage collection during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(dolist (dir '("site-lisp" "lisp"))
	(push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; close the warnings of gccemacs when compiling packages.
(setq native-comp-async-report-warnings-errors nil)

;; use straight
;; https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; stop use backup files
(setq make-backup-files nil)

(require 'init-elpa)
(require 'init-path)
(require 'init-const)
(require 'init-utils)
(require 'init-tramp)
(require 'init-evil)
(require 'init-helm)
(when *is-a-mac*
  (require 'init-macos)
  )
(require 'init-themes)
(if (display-graphic-p)
    (require 'init-gui-frames)
    )
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-company)
;; use yasnippet since lsp and company depend on it
;; though it occupies some time.
(require 'init-yasnippet)
(require 'init-windows)
(require 'init-git)
(require 'init-dashboard)
(require 'init-treemacs)
(require 'init-project)

(require 'init-prog)
(require 'init-shell)
(require 'init-lsp)
(require 'init-ess)
(require 'init-python)
(require 'init-haskell)
(require 'init-tex)
(require 'init-org)
(require 'init-elfeed)
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
