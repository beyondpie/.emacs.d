;;; init-orgroam.el --- Initialize Org-ROAM configurations. -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename "~/git-recipes/notes"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))

(provide 'init-orgroam)
;;; init-orgroam.el ends here
