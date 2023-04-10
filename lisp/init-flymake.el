;;; init-make.el --- Configure flymake global behaviour -*- lexical-binding: t -*-
;;; Code:
(use-package flymake
  :delight
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error))
  :general
  (:states '(normal visual)
           :prefix beyondpie/normal-leader-key
           "eb" '(flymake-show-buffer-diagnostics :whick-key "flymake buffer")
           "ep" '(flymake-show-project-diagnostics :which-key "flymake project"))
  )
(provide 'init-flymake)
;;; init-flymake.el ends here
