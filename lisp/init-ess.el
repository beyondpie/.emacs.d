;;; init-ess.el --- set R env. -*- lexical-binding: t -*-

;;; Commentary:

;; If we want to close the [R/none] showed in minibuffer

;; You can simply set mode-line-process to nil in ess-mode-hook and/or inferior-ess-mode-hook:
;; (setq-local mode-line-process nil)

;;; Code:

(use-package ess
  :delight
  :ensure t
  :pin melpa
  :config
  (setq
   ess-style 'RStudio-
   ess-indent-offset 2
   ess-indent-level 2
   ess-fancy-comments nil
   ess-offset-arguments-newline '(prev-line 2)
   ess-offset-block '(prev-line 2)
   ess-offset-arguments '(prev-line 2)
   ess-indent-from-lhs '(argument fun-decl-opening)
   ess-indent-from-chain-start t
   ess-use-flymake nil
   ess-startup-directory 'default-directory
   )
  ;; :hook (ess-r-mode . lsp)
  :general
  (:states '(normal visual)
   :keymaps 'ess-r-mode-map
   :prefix beyondpie/major-mode-leader-key
   "sl" '(ess-eval-line-and-step :which-key "eval send line")
   "sf" '(ess-eval-function :which-key "eval send function")
   "sr" '(ess-eval-region :which-key "eval send region")
   "gg" '(lsp-find-definition :which-key "lsp find definition")
   "gf" '(helm-semantic-or-imenu :which-key "helm search semantic")
   "go" '(helm-occur :which-key "helm occur")
   "gm" '(helm-all-mark-rings :which-key "helm all mark rings")
   "rn" '(lsp-rename :which-key "lsp rename")
   "rb" '(lsp-format-buffer :which-key "lsp buffer")
   "rr" '(lsp-format-region :which-key "lsp region")
   "'" '(R :which-key "start repl"))
  (:states '(insert emacs)
   :keymaps 'ess-r-mode-map
   "-" '(ess-insert-assign :which-key "ess-assign")
   )
  (:keymaps 'inferior-ess-r-mode-map
            "C-l" '(comint-clear-buffer :which-key "clear console")
             "-" '(ess-insert-assign :which-key "ess-assign")
             )
  (when *is-a-mac*
    (setq inferior-R-program "/usr/local/bin/R"))
  )

(provide 'init-ess)
;;; init-ess.el ends here
