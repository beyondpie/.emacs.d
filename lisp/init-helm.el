;; init-helm.el ---setting helm -*- lexical-binding: t -*-

(require-package 'helm)
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-default-side 'below)
(setq helm-autoresize-max-height 30)

(require-package 'helm-git-grep)
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

(require-package 'helm-swoop)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)


(global-set-key (kbd "C-s") 'helm-swoop)
;; Disable pre-input
(setq helm-swoop-pre-input-function
      (lambda () ""))

;; http://tuhdo.github.io/helm-intro.html#orgheadline2
(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  )

(provide 'init-helm)
