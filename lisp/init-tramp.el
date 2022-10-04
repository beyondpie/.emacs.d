;;; init-tramp.el --- Setting tramps -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://www.emacswiki.org/emacs/TrampMode

;;; Code:
(customize-set-variable 'tramp-syntax 'simplified)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")
;; This inhibits tramp continue to open with git when refresh the buffer
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(provide 'init-tramp)
;;; init-tramp.el ends here
