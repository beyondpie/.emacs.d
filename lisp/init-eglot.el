;;; init-eglot.el --- Initialize eglot configurations. -*- lexical-binding: t -*-

;;; Code:
(use-package eglot
  :delight
  :init
  (setq eglot-autoshutdown t)
  ;; no block during the waiting of eglot
  (setq eglot-sync-connect nil)
  (setq eglot-confirm-server-initiated-edits t)
  (setq eglot-extend-to-xref t)
  )

(provide 'init-eglot)
;;; init-eglot.el ends here
