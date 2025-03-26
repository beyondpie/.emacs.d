;;; init-tramp.el --- Setting tramps -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://www.emacswiki.org/emacs/TrampMode
;; Tramp Info

(require 'tramp)
(setq tramp-default-method "ssh")
(setq password-cache-expiry 72000)
;; use local .ssh/config host setup instead of tramp's
(customize-set-variable 'tramp-use-ssh-controlmaster-options t)
;; direct copy between remote
(customize-set-variable 'tramp-use-scp-direct-remote-copying t)
;; This inhibits tramp continue to open with git when refresh the buffer
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; suppress: Remote file error: Forbidden reentrant call of Tramp
(setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))
;; Important: remote PATH in .profile is the PATH in login
;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(provide 'init-tramp)
;;; init-tramp.el ends here
