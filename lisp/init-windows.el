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
 "wr" '(window-configuration-to-register :which-key "register window")
 "wR" '(jump-to-register :which-key "jump to register")
 "wm" '(toggle-frame-maximized :which-key "max window")
 "wf" '(toggle-frame-fullscreen :which-key "full window")
 "wM" '(make-frame :which-key "make frame")
 "wo" '(other-frame :which-key "other frame")
 )

(use-package avy
  :ensure t
  :pin melpa
  :bind ("C-:" . avy-goto-char)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "jl" '(avy-goto-line :which-key "jump to line")
           "jw" '(avy-goto-word-1 :which-key "jump to word")
           "jp" '(project-switch-project :which-key "jump to projects")
           )
  )

(use-package winum
  :pin melpa
  :hook (after-init . winum-mode)
  :config
  (setq winum-format "%s")
  (setq winum-mode-line-position 0)
  (set-face-attribute 'winum-face nil
                      :foreground "DeepPink"
                      :underline "DeepPink"
                      :weight 'bold)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "j1" '(winum-select-window-1 :which-key "1w")
           "j2" '(winum-select-window-2 :which-key "2w")
           "j3" '(winum-select-window-3 :which-key "3w")
           "j4" '(winum-select-window-4 :which-key "4w")
           "j5" '(winum-select-window-5 :which-key "5w")
           "j6" '(winum-select-window-6 :which-key "6w")
           "j7" '(winum-select-window-7 :which-key "7w")
           "j8" '(winum-select-window-8 :which-key "8w")
           "j9" '(winum-select-window-9 :which-key "9w")
           )
  )

(provide 'init-windows)
;;; init-windows.el ends here
