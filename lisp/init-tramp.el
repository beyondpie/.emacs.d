;;; init-tramp.el --- Setting tramps -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://www.emacswiki.org/emacs/TrampMode
;; Tramp Info

;;; Code:
(require 'tramp)
(customize-set-variable 'tramp-syntax 'simplified)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-terminal-type "xterm-256color")
(setq password-cache-expiry 72000)
;; use local .ssh/config host setup instead of tramp's
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
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

(provide 'init-tramp)
;;; init-tramp.el ends here
