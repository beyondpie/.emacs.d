;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell

;;; Code:

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; (setq inhibit-startup-screen t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(scroll-bar-mode -1)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; http://xahlee.info/emacs/emacs/emacs_customize_default_window_size.html
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106) ; chars
              (height . 60) ; lines
              (font . "Monaco-16")
              (ns-transparent-titlebar . t)
              ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106)
              (height . 60)
              (font . "Monnaco-16")
              (ns-transparent-titlebar . t)
              )))
  (progn
    (set-face-attribute 'default nil :font "Monaco-16")
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0))))
  )
(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
