;;; init-scala.el --- Initialize scala configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref:
;; https://scalameta.org/metals/docs/editors/emacs/
;;; Code:


;; Use monospaced font faces in current buffer
;; https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
 (defun my-buffer-face-mode-fixed ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "SourceCodePro" :height 100))
   (buffer-face-mode))

(defun my-pretty-mode ()
  (interactive)
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point t)
  (prettify-symbols-mode)
  )

(use-package scala-mode
  :init
  (slot/vc-install :fetcher "github"
                   :repo "hvesalai/emacs-scala-mode")
  ;; (setq scala-indent:indent-value-expression t
  ;;       scala-indent:align-parameters t
  ;;      scala-indent:align-forms t)
  :interpreter ("scala" . scala-mode)
  :hook (
         (scala-mode . tree-sitter-hl-mode)
         (scala-mode . company-mode)
         (scala-mode . my-buffer-face-mode-fixed)
         (scala-mode . my-pretty-mode)
         )
  :general
  (:states '(normal visual)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sr" '(sbt-send-region :which-key "sbt-send-region")
           "sl" '(sbt-send-line :which-key "sbt-send-line")
           "gg" '(lsp-find-implementation :which-key "lsp find imp")
           ;; "rn" '(lsp-rename :which-key "lsp rename")
           ;; "rf" '(lsp-format-region :which-key "lsp format")
           ;; "rb" '(lsp-format-buffer :which-key "lsp format buffer")
           )
  )


(use-package sbt-mode
  :init
  (slot/vc-install :fetcher "github"
                   :repo "hvesalai/emacs-sbt-mode")
  :commands sbt-start sbt-command sbt-send-region sbt-send-line
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (defun sbt-send-line ()
    (interactive)
    (sbt:send-region (line-beginning-position)
                     (line-end-position)))
  )

(provide 'init-scala)
;;; init-scala.el ends here

