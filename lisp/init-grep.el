;;; init-grep.el --setting for grep -*- lexical-binding: t -*-
;;; Commentary:

;; Ref: Purcell

;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)
(global-set-key (kbd "C-c s d") 'grep-find)


(provide 'init-grep)
;;; init-grep.el ends here
