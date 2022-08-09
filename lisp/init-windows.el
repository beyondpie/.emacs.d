;;; init-windows.el --- Setting windows within frames -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Purcell

;;; Code:

(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)

;; scroll continuouly one line at a time
(setq scroll-conservatively 10000)

;; navigate the windoews with "C-c <left>" or "C-c <right>"
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


(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
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

;; (use-package minimap
;;   :init (setq minimap-width-fraction 0.2
;;               minimap-minimum-width 15
;;               minimap-window-location 'right)
;;   :hook (after-init . minimap-mode))

(provide 'init-windows)
;;; init-windows.el ends here
