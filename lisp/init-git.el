;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-

(require-package 'magit)
(require-package 'magit-todos)
(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

;; TODO: check the usages of following packages
(maybe-require-package 'yagist)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(maybe-require-package 'github-clone)
(maybe-require-package 'forge)
(maybe-require-package 'github-review)


(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

(global-set-key (kbd "C-x g") 'magit-status)
(require 'general)
(general-define-key
 :states '(normal)
 :prefix "SPC"
 "gs" '(magit-status :which-key "magit status")
 )


(require-package 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'init-git)
