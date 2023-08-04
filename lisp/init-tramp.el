;;; init-tramp.el --- Setting tramps -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://www.emacswiki.org/emacs/TrampMode
;; Tramp Info

;;; Code:
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
;; Important: remote PATH is the PATH in login
;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; how to run python-repl remotely
;; https://emacs.stackexchange.com/questions/13385/running-ipython-remotely
;; One way is to use *eshell*.
;; M-x eshell
;; cd /ssh:<server_name>:~
;; run-python /usr/bin/ipython 
;; Switch to *Python* buffer.
;; NOTE: I add one hacked function in init-python.el


(provide 'init-tramp)
;;; init-tramp.el ends here
