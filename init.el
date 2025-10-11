;;; init.el --- start emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:

;;; Codes:
;;; === Requires / dependencies ===
(require 'package)
(require 'cl-lib)
(require 'bind-key)

;; === Package Setup ===
;; fix error when gpg no public key on rc centos
(setq package-check-signature nil)
(setq package-quickstart nil)
;; for magit, which requires 'transient' >= 0.5.0
(setq package-install-upgrade-built-in t)
;; https://www.emacswiki.org/emacs/LoadPath
;; (setq package-user-dir
;;       (expand-file-name
;;        (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
;;        user-emacs-directory))
;; (unless (file-exists-p package-user-dir)
;;   (make-directory package-user-dir)
;;   )
;; (let ((default-directory package-user-dir))
;;   (normal-top-level-add-subdirs-to-load-path)
;;   )

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(setq package-archives
      '(
        ("melpa" . "~/.emacs.d/myelpa/")
        ;; ("rawmelpa" . "https://melpa.org/packages/")
        ;; ("gnu"   . "https://elpa.gnu.org/packages/")
        )
      )

;;; === package init
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
   (setq package-enable-at-startup nil)          ; To prevent initializing twice
   (package-initialize))

;; Should set before loading `use-package'
(eval-and-compile
   (setq use-package-always-ensure t)
   (setq use-package-always-defer nil)
   (setq use-package-expand-minimally t)
   (setq use-package-enable-imenu-support t))
(eval-when-compile
   (require 'use-package))

(use-package general
  :ensure t
  :demand t)

 
;;; === Global variables ===
(defconst *is-a-mac* (eq system-type 'darwin))

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

;;; === Functions ===
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see).

 Only owrks with emacs >= 29.
 From: https://tony-zorman.com/posts/package-vc-install.html"
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn (setq buffer-read-only t)
             (message "File is a symlink."))
    ))

;;; === Key bindings ===
(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)

(global-set-key (kbd "C-SPC") 'set-mark-command)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
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

;;; === Hooks ===
(add-hook 'find-file-hooks 'read-only-if-symlink)
;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)
;; navigate the windows with "C-c <left>" or "C-c <right>"
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'find-file-hooks 'read-only-if-symlink)
;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;;; === Org ===
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-old-levels-only t)

(setq org-todo-keywords
      '(
        (sequence "TODO" "DELAY" "|" "DONE" "CANCEL" "DOING")))

;;; === Dired ===
(setq dired-listing-switches "-aBhl --group-directories-first")
(with-eval-after-load 'dired
  (setq-default dired-dwim-target t)
  ;; https://emacs-china.org/t/emacs/23850/8
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (require 'dired-x)
  ;; Hook up dired-x global bindings without loading it up-front
  (define-key ctl-x-map "\C-j" 'dired-jump)
  (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'dired-mode-map
   "RET" 'dired-find-file
   "m" 'dired-mark
   "D" 'dired-do-delete
   "d" nil
   "g" 'revert-buffer
   "u" 'dired-unmark
   "+" 'dired-create-directory
   "C" 'dired-do-copy
   "R" 'dired-do-rename
   "(" 'dired-hide-details-mode
   "!" 'dired-do-shell-command
   ))

;;; === better default ===
;; disable creation of .# files
(setq create-lockfiles nil)
;; disable creation of ~ files
(setq make-backup-files nil)
;; disable creation of # files
(setq auto-save-default nil)
(setq scroll-conservatively 10000)
(load-theme 'modus-vivendi t)
(setq-default grep-highlight-matches t
              grep-scroll-output t)
;; Do not show menu and too bar.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
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

;; allow buffer undo with larger undo info
(setq undo-outer-limit 500000000)

;; https://emacs-china.org/t/emacs/21053/13
(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

;; set utf8 to let terminal show the corresonding symbols in the terminal
;; http://www.skybert.net/emacs/how-to-get-unicode-in-the-terminal/
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(ispell-change-dictionary "american" t)

;; === Native Compile ===
(setq native-comp-speed -1)
(setq native-comp-deferred-compilation nil)
(setq native-comp-async-report-warnings-errors nil)

;; remove compling in the mode-line
(setq compilation-in-progress nil)

;;; === Local Settings ===
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
(require 'init-prog)
(require 'init-shell)
(require 'init-ess)
(require 'init-python)
(require 'init-snakemake)
(require 'init-tex)
(require 'init-elfeed)
(require 'init-treemacs)
(require 'init-scala)
(when *is-a-mac*
  (require 'init-macos)
  )
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
