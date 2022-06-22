;;; init-prog.el --- Setting general configs for init-prog -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)


;; general could let me use "," as leader key
;; in prog mode under normal state of evil
(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "C-c g g") 'xref-find-definitions)
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

(provide 'init-prog)
;;; init-prog.el ends here
