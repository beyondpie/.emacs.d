;;; Early-init.el --- Set configs before init.el -*- lexical-binding: t -*-



;;; Commentary:

;; Ref: Purcell and Seagle

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.


;;; Code:
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; for lsp loading json
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq package-quickstart t)

(provide 'early-init)
;;; early-init.el ends here
