;;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :ensure t
  :pin melpa)

(use-package page-break-lines
  :ensure t
  :pin melpa)

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :pin melpa
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Love at Harvard."
        dashboard-startup-banner mybanner
        dashboard-set-navigator t
        dashboard-center-content nil
        dashboard-show-shortcuts nil
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-set-init-info t
        dashboard-init-info "前有古人，后有来者；行你所行，前无古人。"
        dashboard-footer-messages '("吾志所向, 一往无前，愈挫愈奋，再接再厉。")
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))
        )
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
