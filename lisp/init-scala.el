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
         (scala-mode . eglot-ensure)
         ;;(scala-mode . tree-sitter-hl-mode)
         (scala-mode . company-mode)
         (scala-mode . my-buffer-face-mode-fixed)
         (scala-mode . my-pretty-mode)
         )
  :general
  (:states '(normal visual)
           :keymaps 'scala-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sR" '(sbt-send-region :which-key "sbt-send-region")
           "sL" '(sbt-send-line :which-key "sbt-send-line")
           "sr" '(my-sbt-send-region :which-key "my-sbt-send-region")
           "sl" '(my-sbt-send-line :which-key "my-sbt-send-line")
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

