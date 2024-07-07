;;; init-flymake.el --- Configure flymake global behaviour -*- lexical-binding: t -*-
;;; Code:
(use-package flymake
  :delight
  :init
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  ;; added in 1.3.6
  (setq flymake-show-diagnostics-at-end-of-line t)
  )
(provide 'init-flymake)
;;; init-flymake.el ends here
