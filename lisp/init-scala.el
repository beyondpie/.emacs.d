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

(defun scala-eglot-capabilities ()
  (interactive)
  (setq-local eglot-ignored-server-capabilities
              '())
  )

(defun scala-left-margin-width ()
  (interactive)
  (lambda ()
    (setq-local left-margin-width nil)))

(use-package lsp-mode
  :delight
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
	      lsp-keep-workspace-alive nil
        lsp-enable-snippet t
        lsp-enable-xref t
	      lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
	      lsp-modeline-code-actions-enable t
        lsp-diagnostics-provider :flymake
	      lsp-modeline-diagnostics-enable t
	      lsp-modeline-workspace-status-enable t
        ;; https://emacs-china.org/t/spacemacs-c-c/15695
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
	      lsp-enable-file-watchers nil
	      lsp-enable-folding nil
        lsp-semantic-tokens-enable t
	      lsp-enable-text-document-color t
        lsp-enable-imenu t
        lsp-eldoc-enable-hover t
	      lsp-enable-indentation nil
	      lsp-enable-on-type-formatting nil
        lsp-idle-delay 0.500
        lsp-log-io t
        lsp-modeline-code-actions-enable t
        lsp-enable-dap-auto-configure nil
        )
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  lsp
  )
(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            ;; "-J-Dmetals.icons=unicode"
                            ))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-semantic-tokens--enable nil)
  (lsp-metals-install-version "1.6.2")
  ;; :hook (scala-mode . lsp)
  )

(use-package scala-mode
  ;; :init
  ;; (slot/vc-install :fetcher "github"
  ;;                  :repo "hvesalai/emacs-scala-mode")
  ;; (setq scala-indent:indent-value-expression t
  ;;       scala-indent:align-parameters t
  ;;      scala-indent:align-forms t)
  :interpreter ("scala" . scala-mode)
  :hook (
         ;; (scala-mode . eglot-ensure)
         ;; (scala-mode . scala-eglot-capabilities)
         ;; (scala-mode . tree-sitter-hl-mode)
         ;; (scala-mode . my-buffer-face-mode-fixed)
         (scala-mode . my-pretty-mode)
         (scala-mode . scala-left-margin-width)
         )
  :general
  (:states '(normal visual)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sR" '(sbt-send-region :which-key "sbt-send-region")
           "sL" '(sbt-send-line :which-key "sbt-send-line")
           "sr" '(my-sbt-send-region :which-key "my-sbt-send-region")
           "sl" '(my-sbt-send-line :which-key "my-sbt-send-line")
           "rn" '(lsp-rename :which-key "lsp-rename")
           "rb" '(lsp-format-buffer :which-key "lsp-format-buffer")
           "rr" '(lsp-format-region :which-key "lsp-format-region")
           "gg" '(lsp-goto-type-definition :which-key "lsp-go-type-defi")
           )
  :config
  (defun my-sbt:send-region (start end)
    (unless (comint-check-proc (sbt:buffer-name))
      (error "sbt is not running in buffer %s" (sbt:buffer-name)))
    (save-excursion
      (goto-char end)
      (skip-syntax-forward ">")
      (forward-comment (- (point-max)))
      (setq end (point)))
    (save-excursion
      (goto-char start)
      (forward-comment (point-max))
      (setq start (point)))
    (unless (> end start) (error "mark a region of code first"))
    (display-buffer (sbt:buffer-name))
    (let ((submode (buffer-local-value 'sbt:submode
                                       (get-buffer (sbt:buffer-name)))))
      (comint-send-region (sbt:buffer-name) start end)
      (comint-send-string (sbt:buffer-name) "\n")))

  (defun my-sbt-send-region (start end)
        "Send the selected region (between the mark and the current
    point) to the sbt process of the current buffer's sbt
    project. Whitespace and comments at the beginning or end of the
    region are not sent."
    (interactive "r")
    (my-sbt:send-region start end))
  (defun my-sbt-send-line ()
    (interactive)
    (my-sbt:send-region (line-beginning-position)
                     (line-end-position)))
  )


(use-package sbt-mode
  ;; :init
  ;; (slot/vc-install :fetcher "github"
  ;;                  :repo "hvesalai/emacs-sbt-mode")
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

