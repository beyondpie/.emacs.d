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

(use-package org-ref
  :ensure t)

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(use-package helm-bibtex
  :ensure t)

(use-package anki-connect
  :ensure t)
(use-package anki-editor
  :ensure t)

(provide 'init-orgroam)
;;; init-orgroam.el ends here
