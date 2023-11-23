;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell

;;; Code:
(defconst *is-a-mac* (eq system-type 'darwin))
(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set utf8 to let terminal show the corresonding symbols in the terminal
;; http://www.skybert.net/emacs/how-to-get-unicode-in-the-terminal/
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require 'init-elpa)
(require 'init-const)
(require 'init-utils)
(require 'init-better-defaults)
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
(require 'init-treesitter)
(require 'init-citre)
(require 'init-snakemake)
(require 'init-prog)
(require 'init-shell)
(require 'init-ess)
(require 'init-python)
(require 'init-scala)
(require 'init-tex)
(require 'init-org)
(require 'init-elfeed)
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
