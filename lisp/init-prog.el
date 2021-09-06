;;; init-prog.el --- Setting general configs for init-prog -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package highlight-indent-guides
  ;; not load highlight indent guides by default since it may slow emacs.
  ;; https://emacs-china.org/t/highlight-indent-guides/16532/3
  ;; :hook
  ;; (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-character ?\|
        highlight-indent-guides-method 'character)
  )

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)


;; general could let me use "," as leader key
;; in prog mode under normal state of evil
(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 :prefix ","
 "gg" '(xref-find-definitions :which-key "xref find def")
 "el" '(flycheck-list-errors :which-key "flycheck list of errors")
 )

;; eldoc
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(use-package hl-todo
  :delight
  :custom
  ;; Better hl-todo colors, taken from spacemacs
  (hl-todo-keyword-faces '(("TODO" . "#dc752f")
                           ("NEXT" . "#dc752f")
                           ("THEM" . "#2d9574")
                           ("PROG" . "#4f97d7")
                           ("OKAY" . "#4f97d7")
                           ("DONT" . "#f2241f")
                           ("FAIL" . "#f2241f")
                           ("DONE" . "#86dc2f")
                           ("NOTE" . "#b1951d")
                           ("KLUDGE" . "#b1951d")
                           ("HACK" . "#b1951d")
                           ("TEMP" . "#b1951d")
                           ("QUESTION" . "#b1951d")
                           ("HOLD" . "#dc752f")
                           ("FIXME" . "#dc752f")
                           ("XXX+" . "#dc752f")))
  :hook
  ((dashboard-after-initialize . global-hl-todo-mode)
  (prog-mode . hl-todo-mode)))

;; realgud for debugging
(use-package realgud
  :defer t
  :init
  (setq realgud-window-split-orientation 'horizontal)
  )

;; aggressive indent mode
(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

(provide 'init-prog)
;;; init-prog.el ends here
