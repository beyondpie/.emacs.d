;;; init-tramp.el ---Setting tramps -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://www.emacswiki.org/emacs/TrampMode

;;; Code:
(setq tramp-default-method "ssh")
(setq tramp-terminal-type "tramp")
(customize-set-variable 'tramp-encoding-shell "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")
(provide 'init-tramp)
;;; init-tramp.el ends here
