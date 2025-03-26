;;; init-prog.el --- Setting general configs for init-prog -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'init-const)

;; common functions
(defun spacemacs/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; keep global electric-indent-mode
;; (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;; diable auto reindent previous line
(setq-default electric-indent-inhibit t)
;; ref:
;; https://github.com/necaris/conda.el
(use-package conda
  :delight
  :hook
  ((find-file . (lambda ()
                  (when (bound-and-true-p conda-project-env-path)
                    (conda-env-activate-for-buffer))))
   (prog-mode . (lambda ()
                  (setq mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))))
   )
  :config
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)
  :commands
  (conda-env-activate
   conda-env-deactivate
   conda-env-activate-for-buffer
   )
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-global-mode)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(use-package eglot
  :pin melpa
  :init
  (advice-add 'eglot--mode-line-format :override (lambda () ""))
  )

(use-package flymake
  :delight
  :init
  (setq flymake-start-on-flymake-mode nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-no-changes-timeout 1)
  ;; added in 1.3.6
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (setq flymake-indicator-type 'margins)
  )

(use-package highlight-indent-guides
  ;; not load highlight indent guides by default since it may slow emacs.
  ;; https://emacs-china.org/t/highlight-indent-guides/16532/3
  ;; :hook
  ;; (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-character ?\|
        highlight-indent-guides-method 'character)
  )

;; eldoc
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; add auto-fill-mode
;; (add-hook 'prog-mode-hook 'auto-fill-mode)
;; add Emacs default fill indicator
;; (setq-default display-fill-column-indicator-column 80)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
;; use column number to instead of display fill column
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'ts-fold-mode)
;; general could let me use "," as leader key
;; in prog mode under normal state of evil
(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 :prefix beyondpie/major-mode-leader-key
 "rn" '(eglot-rename :which-key "eglot rename")
 "rb" '(eglot-format-buffer :which-key "eglot format buffer")
 "rf" '(eglot-format :which-key "eglot format")
 "rc" '(spacemacs/comint-clear-buffer :which-key "manual clear buffer")
 "gh" '(eldoc :which-key "eldoc")
 "gg" '(eglot-find-implementation :which-key "eglot find imp")
 "M-." '(xref-find-definitions :which-key "xref find def")
 "M-," '(xref-pop-to-location :which-key "xref back")
 "gm" '(imenu :which-key "imenu")
 "eb" '(flymake-show-buffer-diagnostics :whick-key "flymake buffer")
 "ep" '(flymake-show-project-diagnostics :which-key "flymake project")
 "en" '(flymake-goto-next-error :which-key "flymake next err")
 "M-n" '(flymake-goto-next-error :which-key "flymake next err")
 "ep" '(flymake-goto-prev-error :which-key "flymake prev err")
 "M-p" '(flymake-goto-prev-error :which-key "flymake prev err")
 "M-/" '(hippe-expand :which-key "hippie-expand")
 "C-M-/" '(comint-dynamic-complete-filename :which-key "complete filenm")
 "tf" '(ts-fold-toggle :which-key "treesitter fold")
 "va" '(conda-env-activate :which-key "activate conda env")
 "vd" '(conda-env-deactivate :which-key "deactivate conda env")
)

(use-package hl-todo
  :delight
  :custom
  ;; Better hl-todo colors, taken from spacemacs
  (hl-todo-keyword-faces '(("TODO" . "#dc752f")
                           ("NEXT" . "#dc752f")
                           ("THEN" . "#2d9574")
                           ("PROG" . "#4f97d7")
                           ("OKAY" . "#4f97d7")
                           ("DONT" . "#f2241f")
                           ("FAIL" . "#f2241f")
                           ("DONE" . "#86dc2f")
                           ("NOTE" . "#86dc2f")
                           ("KLUDGE" . "#b1951d")
                           ("HACK" . "#b1951d")
                           ("TEMP" . "#b1951d")
                           ("QUESTION" . "#b1951d")
                           ("HOLD" . "#dc752f")
                           ("FIXME" . "#dc752f")
                           ("XXX+" . "#dc752f")))
  :hook
  ((dashboard-after-initialize . global-hl-todo-mode)
  (prog-mode . hl-todo-mode)))

;; realgud for debugging
(use-package realgud
  :defer t
  :init
  (setq realgud-window-split-orientation 'horizontal)
  )

(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode))
  )

(use-package rainbow-delimiters
  :ensure t
  :pin melpa
  :hook ( prog-mode . rainbow-delimiters-mode ))

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (remove-hook 'before-save-hook 'format-all--buffer-from-hook t)
  (defun remove-format-all-from-save-hook ()
    (interactive)
    (remove-hook 'before-save-hook 'format-all--buffer-from-hook t))
  (advice-add 'format-all-buffer :after 'remove-format-all-from-save-hook)
  )

(use-package anzu
  :ensure t
  :pin melpa
  :hook (after-init . global-anzu-mode)
  :delight
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "rq" '(anzu-query-replace :which-key "anzu-query-replace")
           "rg" '(anzu-query-replace-regexp :which-key "anzu-query-replace-regexp")
           "rc" '(anzu-replace-at-cursor-thing :which-key "anzu-replace-at-cursor")
           ))

;; mamba install --channel conda-forge the_silver_searcher
(use-package ag
  :ensure t
  :pin melpa
  :config
  (setq-default ag-highlight-search t)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'override
           "sa" '(ag-project :which-key "ag search in project")
           "sd" '(grep-find :which-key "grep search in current dir")
           )
  )

(provide 'init-prog)
;;; init-prog.el ends here
