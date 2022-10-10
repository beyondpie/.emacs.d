;;; init-helm.el --- setting helm -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: http://tuhdo.github.io/helm-intro.html

;;; Code:

(use-package helm
  ;; :load-path ("~/.emacs.d/helm")
  :ensure t
  :pin melpa
  :hook (after-init . helm-mode)
  :delight
  :config
  (setq helm-split-window-default-side 'below
        helm-autoresize-max-height 40
        helm-autoresize-min-height 10
        helm-echo-input-in-header-line nil
        history-delete-duplicates t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-semantic-fuzzy-match nil
        helm-imenu-fuzzy-match nil
        helm-locate-fuzzy-match nil
        helm-apropos-fuzzy-match nil
        helm-lisp-fuzzy-completion t
        )
  ;; used for helm-man-woman in shell
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (use-package helm-git-grep
    :ensure t
    :pin melpa
    :delight
    :commands helm-git-grep)
  (use-package helm-xref
    :ensure t
    :delight
    :pin melpa)
  (use-package helm-swoop
    :delight
    :ensure t
    :pin melpa
    :init
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-pre-input-function
      (lambda () ""))
    :bind
    ("C-s" . helm-swoop)
    )
  :bind (("M-x" . helm-M-x)
         ;; ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         :map helm-map
         ("C-c g" . helm-git-grep-from-helm)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ([tab] . helm-execute-persistent-action))
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "hg" '(helm-git-grep :which-key "helm git grep")
           "bf" '(helm-mini :which-key "helm-mini"))
  )
(provide 'init-helm)
;;; init-helm ends here
