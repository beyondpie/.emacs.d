;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;; Ref:
;; - emacs config: purcell, doom emacs, spacemacs, centaur emacs
;; - better defaults:
;;   - doom emacs's doom-start.el L37-L43

;;; Global variables
(defcustom beyondpie/normal-leader-key
  "SPC"
  "Evil normal state leader key used as a global leader key."
  :type 'string
  :group 'beyondpie-key
  )

(defcustom beyondpie/non-normal-leader-key
  "M-SPC"
  "Non Evil-nomral state leader key"
  :type 'string
  :group 'beyondpie-key
  )
(defcustom beyondpie/major-mode-leader-key
  ","
  "Like spacemacs, use a different leader key for major-mode"
  :type 'string
  :group 'beyondpie-key
  )

(defcustom beyondpie/citre-readtags-program
  "/usr/local/bin/readtags"
  "citre program"
  :type 'string
  :group 'beyondpie-program
  )

(defcustom mac-ls
  "/usr/local/bin/gls"
  "insert-directory-program in macos for dired"
  :type 'string
  :group 'beyondpie-program)

(defcustom python-flymake-command
  '("ruff" "--quiet" "--stdin-filename=stdin" "-")
  "flymake command for python"
  :type 'list
  :group 'beyondpie-program)

(defcustom python-line-length
  80
  "max line length for python"
  :type 'int
  :group 'beyondpie-program)

;;; better default
;; https://idiomdrottning.org/bad-emacs-defaults
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
;; this needs to start emacs out of elpha directory.
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))

(setq backup-by-copying t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq show-trailing-whitespace t)
(setq kill-whole-line t)

;; https://emacs-china.org/t/topic/25811/5
(setq bidi-inhibit-bpa t)

;; fix error when gpg no public key on rc centos
(setq package-check-signature nil)

;; from doom emacs
;; PERF: A second, case-insensitive pass
;;  over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; make echo area showing message for 10s
(setq suggest-key-bindings 10)

;; garbage collection during startup
(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (init-gc-cons-threshold (* 32 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; close the warnings of gccemacs when compiling packages.
(setq native-comp-async-report-warnings-errors nil)

;; allow buffer undo with larger undo info
(setq undo-outer-limit 500000000)

;; https://emacs-china.org/t/emacs/21053/13
(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

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

(use-package which-key
  :hook (after-init . which-key-mode)
  :delight
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.01)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-max-description-length 30)
)

;; a better undo
(use-package vundo
  :commands (vundo)
  :bind ("C-x u" . vundo)
  :delight
  :config
  (setq vundo-compact-display t))

;; for text edit
(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 :keymaps 'override
 "ir" '(indent-region :which-key "indent region")
 "rw" '(delete-trailing-whitespace :which-key "delete trailing whitespace")
 "sr" '(eval-region :which-key "elisp eval-region")
 )

;; provide better explanations for elisp
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  )

;; better helpful mode
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
)
;; a minor-mode menu
(use-package minions
  :pin melpa
  :hook (after-init . minions-mode)
  )

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn (setq buffer-read-only t)
             (message "File is a symlink."))
    ))
(add-hook 'find-file-hooks 'read-only-if-symlink)


;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)
;; view large file
(use-package vlf
  :ensure t
  :hook (after-init . (lambda () (require 'vlf-setup)))
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "fl" '(vlf :which-key "visualize large file"))
  )

(require 'init-tramp)
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
