;;; init-ess.el --- set R env. -*- lexical-binding: t -*-

;;; Commentary:

;; If we want to close the [R/none] showed in minibuffer

;; You can simply set mode-line-process to nil in ess-mode-hook and/or inferior-ess-mode-hook:
;; (setq-local mode-line-process nil)

;;; Code:
(use-package ess
  :delight
  :ensure t
  :pin melpa
  :hook (ess-r-mode . (lambda ()
                        (setq-local outline-regexp "^#+ +\\*+")))
  :hook (ess-r-mode . outline-minor-mode)
  :hook (ess-r-help-mode . evil-mode)
  ;; :hook (ess-r-mode . (lambda ()
  ;;                       (when (not (file-remote-p default-directory))
  ;;                         (setq-local company-backends
  ;;                                     '(campany-capf
  ;;                                       company-files
  ;;                                       company-capf-with-R-objects
  ;;                                       company-R-library
  ;;                                       company-R-args
  ;;                                       (company-R-objects :separate)
  ;;                                       company-dabbrev-code
  ;;                                       company-keywords
  ;;                                       company-dabbrev))
  ;;                           )
  ;;                       )
  ;;                  )
  :init
  ;; (defvar ess-R-fl-keyword:assign-vars
  ;;       (cons "\\(\\(?2:\\s\"\\).+\\2\\|\\sw+\\)\\s-*\\(<-\\)"
  ;; '(1 font-lock-function-name-face nil)))
  (add-to-list 'process-environment "_R_USE_PIPEBIND_=true")
  (defvar ess-R-fl-keyword:assign-vars
    (cons (concat "\\(" "\\sw+" "\\)"
                  "[ \t]*" "\\(<-\\)")
          '(1 font-lock-function-name-face nil)))
  ;; ESS highlighting
  ;; https://emacs.stackexchange.com/questions/60924/how-to-add-function-call-highlighting-in-ess
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op%       . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:messages . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-R-fl-keyword:assign-vars . t)
          ))
  :config
  (setq ess-fl-keyword:numbers
        (cons "\\b\\.?[0-9]+[.eEL]?[0-9]*\\b\\|\\(\\sw+\\)[:@\$]+"
              'ess-numbers-face))
  (setq
   ess-style 'RStudio-
   ess-indent-offset 2
   ess-indent-level 2
   ess-fancy-comments nil
   ess-offset-arguments-newline '(prev-line 2)
   ess-offset-block '(prev-line 2)
   ess-offset-arguments '(prev-line 2)
   ess-indent-from-lhs '(argument fun-decl-opening)
   ess-indent-from-chain-start t
   ess-use-flymake nil
   ess-startup-directory 'default-directory
   )
  :general
  (:states '(normal visual)
           :keymaps 'ess-r-mode-map
           :prefix beyondpie/major-mode-leader-key
           "sl" '(ess-eval-line-and-step :which-key "eval send line")
           "sf" '(ess-eval-function :which-key "eval send function")
           "sr" '(ess-eval-region :which-key "eval send region")
           "gf" '(helm-semantic-or-imenu :which-key "helm search semantic")
           "go" '(helm-occur :which-key "helm occur")
           "'" '(R :which-key "start repl")
           "rb" '(format-all-buffer :which-key "format-all buffer")
           )
  (:states '(insert emacs)
           :keymaps 'ess-r-mode-map
           "-" '(ess-insert-assign :which-key "ess-assign")
           )
  (:keymaps 'inferior-ess-r-mode-map
            "C-l" '(comint-clear-buffer :which-key "clear console")
            "-" '(ess-insert-assign :which-key "ess-assign")
            )
  (:keymaps 'ess-r-help-mode-map
            "w" nil)
  :general
  (:states 'normal
           :keymaps 'outline-minor-mode-map
           :prefix beyondpie/major-mode-leader-key
           "ha" '(outline-hide-body :which-key "hide body")
           "he" '(outline-hide-entry :which-key "hide entry")
           "ho" '(outline-hide-other :which-key "hide other")
           "hl" '(outline-hide-leaves :which-key "hide-leaves")
           "hs" '(outline-hide-subtree :which-key "hide-subtree")
           "sa" '(show-all :which-key "show all")
           "se" '(outline-show-entry :which-key "show entry")
           "si" '(outline-show-children :which-key "show children")
           "sk" '(outline-show-branches :which-key "show branches")
           "ss" '(outline-show-subtree :which-key "show subtree")
           )
  )

(provide 'init-ess)
;;; init-ess.el ends here
