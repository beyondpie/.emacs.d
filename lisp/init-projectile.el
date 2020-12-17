;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :pin melpa
  :init
  (setq projectile-enable-caching t)
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
