;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:

(use-package flycheck
  :ensure t
  :pin melpa
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  )
(provide 'init-flycheck)
;;; init-flycheck.el ends here
