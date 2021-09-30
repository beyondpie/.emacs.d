;;; init-path.el --- set PATH -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: https://xenodium.com/trying-out-gccemacs-on-macos/
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  ;; for conda env in python-mode
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (add-to-list 'exec-path-from-shell-variables "WORKON_HOME" t)
  (add-to-list 'exec-path-from-shell-variables "LDFLAGS" t)
  (exec-path-from-shell-initialize)
  )

(provide 'init-path)
;;; init-path.el ends here
