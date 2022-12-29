;;; init-path.el --- set PATH -*- lexical-binding: t -*-

;;; Commentary:
;; Ref:
;; - https://xenodium.com/trying-out-gccemacs-on-macos/
;; - https://emacs-china.org/t/init-exec-path/23202/3
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :custom (exec-path-from-shell-arguments '("-l"))
  :config
  (dolist (var '("PATH" "MANPATH" "WORKON_HOME"
                 "LDFLAGS" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)
    )
  (exec-path-from-shell-initialize)
  )

(provide 'init-path)
;;; init-path.el ends here
