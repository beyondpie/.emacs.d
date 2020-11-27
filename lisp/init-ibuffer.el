;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-

(require-package 'ibuffer)

(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))


(global-set-key (kbd "C-x C-b") 'ibuffer)
(provide 'init-ibuffer)
