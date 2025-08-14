;;; init-prog.el --- Setting general configs for init-prog -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; === Functions ===
;; common functions
(defun spacemacs/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;; === variable sets ===

;; keep global electric-indent-mode
;; (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;; diable auto reindent previous line
(setq-default electric-indent-inhibit t)

(setq-default indent-tabs-mode nil
              default-tab-width 2
              tab-width 2)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(setq-default display-fill-column-indicator-column 78)

(custom-set-variables
 '(eglot-ignored-server-capabilities '(:hoverProvider))
)

;; === hooks ===
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))
;; (add-hook 'prog-mode-hook 'auto-fill-mode)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'column-number-mode)


;;; === key bindings ===
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
)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 :keymaps 'override
 "ir" '(indent-region :which-key "indent region")
 "rw" '(delete-trailing-whitespace :which-key "delete trailing whitespace")
 "sr" '(eval-region :which-key "elisp eval-region")
 )

;; === Pakcages ===
;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(use-package diminish
  :ensure t
  :demand t)
(use-package delight
  :ensure t
  :demand t)

(use-package async
  :ensure t
  :pin melpa)

(use-package dired-quick-sort
 :pin melpa
 :ensure t
 ;; depend on async
 :after dired-async
 :init
 (setq dired-quick-sort-suppress-setup-warning t)
 :config
 (with-eval-after-load 'dired
   (dired-quick-sort-setup))
 :general
 (:states '(normal visual)
          :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "dS" '(hydra-dired-quick-sort/body "dired-sort")  
          )
 )

(use-package diff-hl
  :pin melpa
  :hook (dired-mode . diff-hl-dired-mode)
  )
(use-package all-the-icons
  :pin melpa
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :diminish
  :pin melpa
  :init
  (setq all-the-icons-dired-monochrome nil)
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package tree-sitter
  :ensure t
  :pin melpa
  ;; remove global tree-sitter-mode
  ;; :hook ( (after-init . global-tree-sitter-mode))
  ;; :hook ( (python-mode . tree-sitter-mode) )
  :config
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20#issuecomment-1352675350
  (defun tree-sitter-mark-bigger-node ()
    (interactive)
    (let* ((root (tsc-root-node tree-sitter-tree))
           (node (tsc-get-descendant-for-position-range root (region-beginning) (region-end)))
           (node-start (tsc-node-start-position node))
           (node-end (tsc-node-end-position node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (tsc-get-parent node)))
          (setq node-start (tsc-node-start-position node)
                node-end (tsc-node-end-position node))))
      (set-mark node-end)
      (goto-char node-start)))
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "tm" '(tree-sitter-mark-bigger-node :which-key "tree-sitter mark")
           )
  )


;; some links from tree-sitter-langs is old
;; have to install them manually by following the README.
;; then copy the binary to ~/.emacs.d/[local dir]
(use-package tree-sitter-langs
  :ensure t
  :pin melpa)

;; program fold
;; (use-package ts-fold
;;   :init (slot/vc-install :fetcher "github" :repo "emacs-tree-sitter/ts-fold")
;;   )

(use-package conda
  :delight
  :hook
  ((find-file . (lambda ()
                  (when (bound-and-true-p conda-project-env-path)
                    (conda-env-activate-for-buffer))))
   (prog-mode . (lambda ()
                  (setq mode-line-format
                        (cons '(:exec conda-env-current-name) mode-line-format))))
   )
  :config
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)
  :commands
  (conda-env-activate
   conda-env-deactivate
   conda-env-activate-for-buffer
   )
  :general
  (:states '(normal)
           :keymaps 'prog-mode-map
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "va" '(conda-env-activate :which-key "activate conda env")
           "vd" '(conda-env-deactivate :which-key "deactivate conda env")
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
;; this will put realgud at first position of load path.
;; TODO: should we remove it?
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

(use-package vlf
  :ensure t
  :hook (after-init . (lambda () (require 'vlf-setup)))
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "fl" '(vlf :which-key "visualize large file"))
  )

(use-package minions
  :pin melpa
  :hook (after-init . minions-mode)
  )

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
)

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1
              :after #'elisp-demos-advice-describe-function-1)
  )

(use-package which-key
  :hook (after-init . which-key-mode)
  :delight
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.01)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-max-description-length 30)
)

(use-package vundo
  :commands (vundo)
  :bind ("C-x u" . vundo)
  :delight
  :config
  (setq vundo-compact-display t))

(use-package symbol-overlay
  :hook (after-init . symbol-overlay-mode)
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all) 
  )

(provide 'init-prog)
;;; init-prog.el ends here
