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
  :general
  (:states '(normal visual)
           :keymaps 'override
           :prefix beyondpie/major-mode-leader-key
           "rn" '(eglot-rename :which-key "eglot rename")
           "rf" '(eglot-format :which-key "eglot format")
           "ri" '(eglot-code-action-organize-imports :which-key "eglot import")
           "ra" '(eglot-code-actions :which-key "eglot code actions")
           "gh" '(eldoc :which-key "eldoc")
           "gi" '(eglot-inlay-hints-mode :which-key "toggle eglot inlay hints")
           "gg" '(xref-find-definitions :which-key "xref find define")
           "gm" '(imenu :which-key "imenu")
           )
  )

(provide 'init-eglot)
;;; init-eglot.el ends here
