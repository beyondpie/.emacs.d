;;; init-dired.el --- Configure dired -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:

(with-eval-after-load 'dired
  (setq-default dired-dwim-target t)
  (define-key dired-mode-map (kbd "RET") 'dired-find-file)
  (define-key dired-mode-map (kbd "m") 'dired-mark)
  (define-key dired-mode-map (kbd "D") 'dired-do-delete)
  (define-key dired-mode-map (kbd "g") 'revert-buffer)
  (define-key dired-mode-map (kbd "u") 'dired-unmark)
  (define-key dired-mode-map (kbd "+") 'dired-create-directory)
  (define-key dired-mode-map (kbd "C") 'dired-do-copy)
  (define-key dired-mode-map (kbd "R") 'dired-do-rename)
  (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd "!") 'dired-do-shell-command)
  )
  

(provide 'init-dired)
;;; init-dired.el ends here
