;;; init-windows.el --- Setting windows within frames -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: Purcell

;;; Code:

;; navigate the windoews with "C-c <left>" or "C-c <right>"
(add-hook 'after-init-hook 'winner-mode)

;; "C-x o" for another window
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)
(require-package 'avy)


(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(require 'init-const)
(require 'general)
(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 "wh" '(evil-window-left :which-key "left window")
 "wl" '(evil-window-right :which-key "right window")
 "wj" '(evil-window-down :which-key "down window")
 "wk" '(evil-window-up :which-key "up window")
 "wm" '(toggle-frame-maximized :which-key "max window")
 "wf" '(toggle-frame-fullscreen :which-key "full window")
 "wM" '(make-frame :which-key "make frame")
 "wo" '(other-frame :which-key "other frame")
 "jl" '(avy-goto-line :which-key "jump to line")
 "jw" '(avy-goto-word-1 :which-key "jump to word")
 )

;; scroll continuouly one line at a time
(setq scroll-conservatively 10000)

(provide 'init-windows)
;;; init-windows.el ends here
