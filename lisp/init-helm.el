;;; init-helm.el --- setting helm -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: http://tuhdo.github.io/helm-intro.html

;;; Code:
(use-package helm
  :ensure t
  :pin melpa
  :hook (after-init . helm-mode)
  :delight
  :init
  (setq helm-split-window-default-side 'below
        helm-autoresize-max-height 40
        helm-autoresize-min-height 10
        helm-echo-input-in-header-line nil
        history-delete-duplicates t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-move-to-line-cycle-in-source nil
        helm-allow-mouse nil
        helm-swoop-split-window-function 'helm-default-display-buffer
        )
  
  :config
  ;; used for helm-man-woman in shell
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-autoresize-mode 1)
  :bind (
         ("M-x" . helm-M-x)
         ;; ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         :map helm-map
         ;; ("C-c g" . helm-git-grep-from-helm)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ([tab] . helm-execute-persistent-action)
         )
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "bf" '(helm-mini :which-key "helm-mini"))
  ;; https://stackoverflow.com/questions/30142296/search-in-current-folder-with-helm-do-grep
  (defun my/helm-do-grep-current-directory-tree ()
    "Recursively search current directory.
   If a parent directory has a `dir-locals-file', use that as the
   root instead."
    (interactive)
    (let ((variables-file (dir-locals-find-file
                           (or (buffer-file-name) default-directory))))
      (helm-do-grep-1
       (list
        (cond
         ((stringp variables-file)
          (file-name-directory variables-file))
         ((consp variables-file)
          (nth 0 variables-file))
         (t default-directory)))
       t nil '("*"))))
  )

;;(use-package helm-git-grep
;;  :init
;;  (slot/vc-install :fetcher "github"
;;                   :repo "yasuyk/helm-git-grep"
;;                   )
;;  :delight
;;  :commands helm-git-grep
;;  :general
;;  (:states '(normal visual insert emacs)
;;	   :prefix beyondpie/normal-leader-key
;;	   :non-normal-prefix beyondpie/non-normal-leader-key
;;	   :keymaps 'override
;;	   "hg" '(helm-git-grep :which-key "helm git grep")
;;	   )
;;  )

(use-package helm-xref
  :delight
  :pin melpa)

;; (use-package helm-swoop
;;   :delight
;;   :init
;;   (slot/vc-install :fetcher "github"
;;                    :repo "emacsattic/helm-swoop")
;;   (setq helm-swoop-split-with-multiple-windows t
;;         helm-swoop-split-direction 'split-window-vertically)
;;   (setq helm-swoop-pre-input-function
;;         (lambda () ""))
;;   :bind
;;   ("C-s" . helm-swoop)
;;   )

(provide 'init-helm)
;;; init-helm.el ends here
