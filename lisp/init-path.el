;;; init-path.el --- set PATH -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :pin melpa
  :commands  exec-path-from-shell-initialize
  )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(provide 'init-path)
;;; init-path.el ends here
