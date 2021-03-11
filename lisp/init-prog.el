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

;; highlight comment annotations
(use-package fic-mode
  :hook (prog-mode . fic-mode))

(provide 'init-prog)
;;; init-prog.el ends here
