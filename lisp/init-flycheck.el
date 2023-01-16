;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:

(use-package flycheck
  :pin melpa
  :hook (after-init . global-flycheck-mode)
  :delight
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "eb" '(flycheck-buffer :which-key "flycheck-buffer")
           "el" '(flycheck-list-errors :which-key "flycheck-list-errors")
           "en" '(flycheck-next-error :which-key "flycheck-next-error")
           "ep" '(flycheck-previous-error :which-key "flycheck-previous-error")
           )
  )
(provide 'init-flycheck)
;;; init-flycheck.el ends here
