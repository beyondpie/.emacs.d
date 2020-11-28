;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary: from purcell
;;; Code:

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list))

;; not use flycheck-color-mode-line
;; (when (maybe-require-package 'flycheck-color-mode-line)
;; (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; (remove-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
