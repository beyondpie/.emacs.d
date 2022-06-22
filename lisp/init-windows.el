;;; init-windows.el --- Setting windows within frames -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Purcell

;;; Code:

;; scroll continuouly one line at a time
(setq scroll-conservatively 10000)

;; navigate the windoews with "C-c <left>" or "C-c <right>"
(add-hook 'after-init-hook 'winner-mode)


(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-c w f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c w m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c w M") 'make-frame)
(global-set-key (kbd "C-c w o") 'other-frame)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)

(provide 'init-windows)
;;; init-windows.el ends here
