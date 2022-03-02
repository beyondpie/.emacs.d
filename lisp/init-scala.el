;;; init-scala.el --- Initialize python configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: ensime

;;; Code:

(use-package scala-mode
  :ensure nil
  )

(use-package ensime-mode
  :ensure nil
  :load-path "~/Downloads/ensime-tng-3.0.0/lisp"
  :commands ensime-mode
  :bind
  (:map ensime-mode-map
        ("M-." . ensime-jump-to-definition)
        ("C-c C-i t" . ensime-type-at-point)
        ("C-c C-i s" . ensime-symbol-at-point)
        ("C-c C-r i" . ensime-import-symbol-at-point))
  :hook scala-mode-hook
  )
(provide 'init-scala)
;;; init-scala.el ends here

