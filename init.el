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
;; https://emacs-china.org/t/emacs/21053/13
(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

;; garbage collection during startup
(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (init-gc-cons-threshold (* 32 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; allow eww to open video when emacs versin >= 29.1

(if (not (version< emacs-version "29.1"))
    (setq shr-use-xwidgets-for-media t))

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

(require 'init-elpa)
(require 'init-path)
(require 'init-const)
(require 'init-utils)
(require 'init-tramp)
(require 'init-evil)
(require 'init-themes)
(require 'init-helm)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-recentf)
(require 'init-company)
;; use yasnippet since lsp and company depend on it
;; though it occupies some time.
(require 'init-yasnippet)
(require 'init-windows)
(require 'init-git)
(require 'init-project)
(require 'init-lsp)
(require 'init-citre)
(require 'init-snakemake)
(require 'init-prog)
(require 'init-treesitter)
(require 'init-shell)
(require 'init-ess)
(require 'init-python)
(require 'init-tex)
(require 'init-org)
(require 'init-elfeed)
(require 'init-conda)
(require 'init-treemacs)

(when *is-a-mac*
  (require 'init-macos)
  )

(if (display-graphic-p)
    (progn 
      (beyondpie/setgui)
      (beyondpie/set-evil-insert-state-cursor)
      )
  )

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
