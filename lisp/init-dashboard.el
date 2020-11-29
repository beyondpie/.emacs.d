;;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Ref: seagle

;;; Code:
(require 'init-const)
;; optional for dashboard
(require-package 'all-the-icons)
;; https://github.com/emacs-dashboard/emacs-dashboard
(require-package 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-banner-logo-title "Love at Boston.")
(setq dashboard-startup-banner mybanner)

;; show navigator below the banner
(setq dashboard-set-navigator t)
(setq dashboard-navigator-buttons
      `(;; line1
	((, (all-the-icons-octicon
             "mark-github" :height 1.1 :v-adjust 0.0)
	    "GithubPage" "Browse githubpage"
	    (lambda (&rest _) (browse-url beyondpie-homepage)))
	 )))

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

;; show items
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)))
;; use icons to the widget
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
;; modify the icons from the all-the-icons
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

;; show dashboard when using emacsclient -c
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; show packages loaded and init time
(setq dashboard-set-init-info t)
(setq dashboard-init-info "Emacs at your service.")

;; close randome footnote
;;(setq dashboard-set-footer nil)
(setq dashboard-footer-messages '("吾志所向一往无前，愈挫愈勇再接再厉。"))
(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
