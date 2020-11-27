;; init-helm.el ---setting helm -*- lexical-binding: t -*-


(require-package 'helm)
(require-package 'helm-swoop)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(global-set-key (kbd "C-SPC") 'set-mark-command)

;; helm
;; TODO: maybe we don't need isearch?
(global-set-key (kbd "C-s") 'helm-swoop)
;; http://tuhdo.github.io/helm-intro.html#orgheadline2
(eval-after-load "helm-mode"
	'(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)) ; rebind tab to do persistent action
(eval-after-load "helm-mode"
	'(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)) ; make TAB works in terminal
(eval-after-load "helm-mode"
	'(define-key helm-map (kbd "C-z")  'helm-select-action)) ; list actions using C-z


(provide 'init-helm)
