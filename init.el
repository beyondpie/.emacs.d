;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell

;;; Code:
(setq package-quickstart nil)
(defconst *is-a-mac* (eq system-type 'darwin))

;; https://www.emacswiki.org/emacs/LoadPath
(setq package-user-dir
      (expand-file-name
       (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory))
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir)
  )
(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path)
  )

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set utf8 to let terminal show the corresonding symbols in the terminal
;; http://www.skybert.net/emacs/how-to-get-unicode-in-the-terminal/
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(ispell-change-dictionary "american" t)

;; for magit, which requires 'transient' >= 0.5.0
(setq package-install-upgrade-built-in t)

;; remove naive comp function
(setq native-comp-speed -1)

(setq-default indent-tabs-mode nil
              default-tab-width 2
              tab-width 2)
(global-set-key (kbd "C-SPC") 'set-mark-command)
(fset 'yes-or-no-p 'y-or-n-p)

;; remove compling in the mode-line
(setq compilation-in-progress nil)

;; remove up/down case keys due to they usually make my codes typo
;; upcase-region
(global-unset-key (kbd "C-x C-u"))
;; upcase-word
(global-unset-key (kbd "M-u"))
;; downcase-word
(global-unset-key (kbd "M-l"))
;; downcase-region
(global-unset-key (kbd "C-x C-l"))
(define-key global-map (kbd "M-j") nil)
(define-key global-map (kbd "M-k") nil)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(require 'init-elpa)
(require 'init-const)
(require 'init-dired)
(require 'init-utils)
(require 'init-tramp)
(require 'init-better-defaults)
(require 'init-evil)
(require 'init-themes)
(require 'init-helm)
(require 'init-ibuffer)
(require 'init-recentf)
(require 'init-company)
(require 'init-windows)
(require 'init-git)
(require 'init-project)
(require 'init-treesitter)
(require 'init-prog)
(require 'init-shell)
(require 'init-ess)
(require 'init-python)
(require 'init-snakemake)
(require 'init-tex)
(require 'init-org)
(require 'init-elfeed)
(require 'init-treemacs)
(require 'init-scala)

(when *is-a-mac*
  (require 'init-macos)
  )

(if (display-graphic-p)
    (beyondpie/setgui)
  )

(when (file-exists-p custom-file)
  (load custom-file))

(load-theme 'modus-vivendi t)

(provide 'init)
;;; init.el ends here
