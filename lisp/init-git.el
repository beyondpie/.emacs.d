;;; init-git.el --- Git support -*- lexical-binding: t -*-

;;; Commentary:
;; Ref:
;; - reduce magit time
;;   https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/

;;; Code:
(use-package magit
  :ensure t
  :init
  (use-package with-editor :ensure t)
  :custom
  (magit-git-executable "/usr/local/bin/git")
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "gf" '(magit-file-dispatch :which-key "magit-file-dispatch")
           "gs" '(magit-status :which-key "magit-status")
           "gd" '(magit-dispatch :which-key "magit-dispatch")
           )
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  ;; :config
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  )

(provide 'init-git)
;;; init-git.el ends here
