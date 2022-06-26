;;; init-ess.el --- set R env. -*- lexical-binding: t -*-

;;; Commentary:

;; If we want to close the [R/none] showed in minibuffer

;; You can simply set mode-line-process to nil in ess-mode-hook and/or inferior-ess-mode-hook:
;; (setq-local mode-line-process nil)

;;; Code:

(unless (package-installed-p 'ess)
  (package-refresh-contents)
  (package-install 'ess)
  )

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

(with-eval-after-load 'ess
  (setq ess-indent-offset 2
        ess-style 'RStudio
        ess-fancy-comments nil
        ess-offset-arguments-newline "prev-line"
        ess-use-flymake nil
        ess-startup-directory 'default-directory
        )  
  )

(with-eval-after-load 'ess-r-mode
  (define-key ess-r-mode-map (kbd "C-c s l") 'ess-eval-line-and-step)
  (define-key ess-r-mode-map (kbd "C-c s f") 'ess-eval-function)
  (define-key ess-r-mode-map (kbd "C-c s r") 'ess-eval-region)
  (define-key ess-r-mode-map (kbd "C-c '") 'R)
  (define-key inferior-ess-r-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-ess-r-mode-map (kbd "-") 'ess-insert-assign)
  (define-key ess-r-mode-map (kbd "-") 'ess-insert-assign)
  )
 
(provide 'init-ess)
;;; init-ess.el ends here
