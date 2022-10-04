;;; init-windows.el --- Setting windows within frames -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Purcell

;;; Code:

(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)

;; scroll continuouly one line at a time
(setq scroll-conservatively 10000)

;; navigate the windows with "C-c <left>" or "C-c <right>"
(add-hook 'after-init-hook 'winner-mode)

(use-package switch-window
  :ensure t
  :pin melpa
  :bind ("C-x o" . switch-window)
  :delight
  :config
  (setq-default switch-window-shortcut-style 'alphabet
                switch-window-timeout nil)
  )
(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 :keymaps 'override
 "wm" '(toggle-frame-maximized :which-key "max window")
 "wf" '(toggle-frame-fullscreen :which-key "full window")
 "wM" '(make-frame :which-key "make frame")
 "wo" '(other-frame :which-key "other frame")
 )

(provide 'init-windows)
;;; init-windows.el ends here
