;;; init-themes.el --- set themes -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell

;;; Code:

(require-package 'spacemacs-theme)
(require-package 'modus-operandi-theme)
(require-package 'modus-vivendi-theme)

(setq-default custom-enabled-themes '(spacemacs-dark))
(set-frame-font "Monaco-16" nil t)

(provide 'init-themes)
;;; init-themes.el ends here
