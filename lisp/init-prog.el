;; init-prog.el --setting general configs for init-prog -*- lexical-binding: t -*-

(require-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-method 'character)


(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)

;; general could let me use "," as leader key
;; in prog mode under normal state of evil
(require 'general)
(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 :prefix ","
 "gg" '(xref-find-definitions :which-key "lsp find def")
 )

(provide 'init-prog)
