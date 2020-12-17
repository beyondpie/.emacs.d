;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; use-package cannot install spacemacs-them, wired.
(require-package 'spacemacs-theme)
(load-theme 'spacemacs-dark t)
(set-frame-font "Monaco-16" nil t)

(provide 'init-themes)
;;; init-themes.el ends here
