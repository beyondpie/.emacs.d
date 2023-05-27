;;; init-treesitter.el --- Initialize treesitter configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Use tree-sitter as a minor mode, not the one in default Emacs 29
;;; Code:
(use-package tree-sitter
  :ensure t
  :pin melpa
  :hook ( (after-init . global-tree-sitter-mode))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
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

(use-package tree-sitter-langs
  :ensure t
  :pin melpa)

;; https://github.com/ShuguangSun/tree-sitter-ess-r/issues/4
;; tree-sitter-ess-r helps to set tree-sitter-hl-default-patterns
;; from nil to right ones.
(use-package tree-sitter-ess-r
  :delight
  :pin melpa
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate)
  )

;; program fold
(use-package ts-fold
  :init (slot/vc-install :fetcher "github" :repo "emacs-tree-sitter/ts-fold")
  )

(provide 'init-treesitter)
;;; init-treesitter.el ends here
