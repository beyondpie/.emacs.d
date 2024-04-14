;;; init-better-defaults.el --- Initialize better defaults.	-*- lexical-binding: t -*-

;;; Commentary:
;; - doom emacs
;;   https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-start.el#L37-L43
;; - https://emacs-china.org/t/topic/25811/5
;; - https://idiomdrottning.org/bad-emacs-defaults

;;; Code:

;; https://idiomdrottning.org/bad-emacs-defaults
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
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
;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq bidi-inhibit-bpa t)
;; from doom emacs as a backup
;; (setq-default bidi-display-reordering 'left-to-right
;;               bidi-paragraph-direction 'left-to-right)
;; this fix error when gpg no public key on rc centos
(setq package-check-signature nil)
;; from doom emacs
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)
;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)
;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; mine
(defconst *spell-check-support-enabled* t)
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
;; allow eww to open video when emacs versin >= 29.1
(if (not (version< emacs-version "29.1"))
    (setq shr-use-xwidgets-for-media t))

;; close the warnings of gccemacs when compiling packages.
(setq native-comp-async-report-warnings-errors nil)

;; stop use backup files
;; (setq make-backup-files nil)

;; allow buffer undo with larger undo info
(setq undo-outer-limit 500000000)

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
