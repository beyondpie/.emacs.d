;;; init-org.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(setq org-agenda-files '("~/Dropbox/plan/research.org"))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-old-levels-only t)

(setq org-todo-keywords
      '(
        (sequence "TODO" "DELAY" "|" "DONE" "CANCEL" "DOING")))

(provide 'init-org)
;;; init-org.el ends here
