;;; -*- lexical-binding: t -*-


;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.


;;; Code:

(setq package-enable-at-startup nil)

(provide 'early-init)
