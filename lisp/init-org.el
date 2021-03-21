;;; init-org.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

;; grid-mode for GitHub Readme Instant Preview
;; https://github.com/seagle0128/grip-mode

(use-package grip-mode
  :ensure t
  :custom
  (setq grip-preview-use-webkit nil)
  :hook ((markdown-mode org-mode) . grip-mode)
  )

(provide 'init-org)
;;; init-tex.el ends here
