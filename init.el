;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell

;;; Code:
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))

;; set mac command
;; Both command keys are 'Super'
(when *is-a-mac*
  (setq mac-right-command-modifier 'super
        mac-command-modifier 'super)
)


;; garbage collection during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(dolist (dir '("site-lisp" "lisp"))
	(push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; use straght
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

(require 'init-elpa)
(require 'init-path)
(require 'init-const)
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
(require 'init-company)
(require 'init-yasnippet)
(require 'init-windows)
(require 'init-git)
(require 'init-projectile)
(require 'init-dashboard)
(require 'init-treemacs)
(require 'init-osx-keys)

(require 'init-prog)
(require 'init-shell)
(require 'init-lsp)
(require 'init-ess)
(require 'init-python)
(require 'init-tex)

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
