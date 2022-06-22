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
(setq initial-buffer-choice 'about-emacs)

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

;; stop use backup files
(setq make-backup-files nil)

;; set utf8 to let terminal show the corresonding symbols in the terminal
;; http://www.skybert.net/emacs/how-to-get-unicode-in-the-terminal/
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; elisp
(global-set-key (kbd "C-c s r") 'eval-region)

;; set package source
;; set up elpa
(require 'init-elpa)
(require 'init-utils)
(require 'init-tramp)
(require 'init-windows)
(if (display-graphic-p)
    (require 'init-gui-frames)
  )
(require 'init-themes)
(require 'init-dired)
(require 'init-shell)
(require 'init-prog)
(require 'init-python)
(require 'init-ess)
(require 'init-recentf)
(require 'init-org)
(require 'init-grep)


(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
