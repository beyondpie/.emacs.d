;;; init-path.el --- set PATH -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  ;; for conda env in python-mode
  (add-to-list 'exec-path-from-shell-variables "WORKON_HOME" t)
  )

(when (or (memq window-system '(mac ns x)) (daemonp))
  (exec-path-from-shell-initialize))

(provide 'init-path)
;;; init-path.el ends here
