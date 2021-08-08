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
  (when *is-a-mac*
    (magit-git-executable "/usr/local/bin/git")
    )
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
  :config
  ;; ref: https://magit.vc/manual/magit/Performance.html
  (setq magit-refresh-status-buffer nil)
  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-body nil
        magit-diff-refine-hunk nil)
  (setq magit-revision-insert-related-refs nil)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  )

(provide 'init-git)
;;; init-git.el ends here
