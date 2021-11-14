;;; init-grep.el --setting for grep -*- lexical-binding: t -*-
;;; Commentary:

;; Ref: Purcell

;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)
(use-package ag
  :ensure t
  :pin melpa
  :config
  (setq-default ag-highlight-search t)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "sa" '(ag-project :which-key "ag search in project")
           "sd" '(grep-find :which-key "grep search in current dir")
           )
  )

(provide 'init-grep)
;;; init-grep.el ends here
