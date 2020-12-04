;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Ref: seagle
;;

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-global-mode)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
