;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :pin melpa
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories '(".git" "target" "build"))
  (setq projectile-globally-ignored-files '(".DS_Store"))
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  :hook (after-init . projectile-mode)
  :config
  (when (executable-find "ag")
    (setq-default projectile-generic-command "ag --files --hidden"))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("s-p" . projectile-command-map))
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "p" '(projectile-command-map :which-key "projectile"))
  ;; disable projectile on remote buffers
  ;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory 'no-identification) ad-do-it))
  )

;;https://github.com/purcell/ibuffer-projectile
(use-package ibuffer-projectile
  :ensure t
  :pin melpa
  :commands ibuffer-projectile-set-filter-groups
  :config
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file)))
  :hook (ibuffer . (lambda () (ibuffer-projectile-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  )

(provide 'init-projectile)
;;; init-projectile.el ends here
