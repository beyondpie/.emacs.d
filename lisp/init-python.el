;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: pacemacs

;;; Code:

(defun spacemacs//python-setup-shell (&rest args)
  "Set up python shell"
  (if (executable-find "ipython")
      (progn
        (setq python-shell-interpreter "ipython")
        ;; ipython version >= 5
        (setq python-shell-interpreter-args "--simple-prompt -i")
        )
    (progn
      (setq python-shell-interpreter "python")
      ;; (setq python-shell-interpreter-args "-i")
      )
    )
  )

(defun spacemacs//python-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (executable-find "pylint"))
          (flake8 (executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))


(defun spacemacs/python-setup-everything (&rest args)
  "Set up python env"
  (spacemacs//python-setup-shell)
  (spacemacs//python-setup-checkers)
  )

(defun beyondpie/start-lsp-pyright()
  "Setup python backend"
  (interactive)
  (require 'lsp-pyright)
  (lsp)
  )

(defun spacemacs//python-default ()
  "Default settings for python buffers"
  (setq mode-name "Python"
        tab-width 4
        fill-column 88)
  )

(defun spacemacs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun spacemacs/python-shell-send-defun ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)))

(defun spacemacs/python-shell-send-region (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)))

(defun spacemacs/python-shell-send-line ()
	"Send the current line to shell"
	(interactive)
	(let ((python-mode-hook nil)
	       (start (point-at-bol))
	       (end (point-at-eol)))
	  (python-shell-send-region start end)))

(defun spacemacs/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package blacken
  :ensure t
  :pin melpa
  :commands (blacken-buffer))

(use-package lsp-pyright
  :ensure t
  :pin melpa
  )

(use-package pyvenv
  :ensure t
  :pin melpa
  :hook (python-mode . pyvenv-tracking-mode)
  :general
  (:states '(normal visual)
           :keymaps '(python-mode-map ess-r-mode-map)
           :prefix beyondpie/major-mode-leader-key
           "va"  #'pyvenv-activate
           "vd" #'pyvenv-deactivate
           "vw" #'pyvenv-workon)
  :config
  (dolist (func '(pyvenv-actiate pyvenv-deactivate pyvenv-workon))
    (advice-add func :after 'spacemacs/python-setup-everything)
    )
  )

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package python
  :ensure t
  :pin melpa
  :mode (("\\.py\\'" . python-mode)
         ("\\.snakefile\\'" . python-mode)
         ("Snakefile\\'" . python-mode)
         ("snakefile\\'" . python-mode))
  :hook (;; (python-mode . spacemacs//python-setup-backend)
         (python-mode . spacemacs//python-default))
  :init
  (progn
    (spacemacs//python-setup-shell)
    (setq python-indent-offset 4)
    (setq python-shell-completion-native-enable nil)
    )
  (defun spacemacs/python-remove-unused-imports()
    "Use Autoflake to remove unused function"
    "autoflake --remove-all-unused-imports -i unused_imports.py"
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (message "Error: Cannot find autoflake executable.")))
 :config
  (progn
    ;; Env vars
    (with-eval-after-load 'exec-path-from-shell
      (exec-path-from-shell-copy-env "PYTHONPATH"))
    (setq-default python-indent-guess-indent-offset nil)
    )
  :general
  (:states '(normal visual)
           :keymaps 'python-mode-map
           :prefix beyondpie/major-mode-leader-key
           "gg" '(lsp-find-definition :which-key "lsp find definition")
           "gf" '(helm-semantic-or-imenu :which-key "helm search semantic")
           "go" '(helm-occur :which-key "helm occur")
           "gm" '(helm-all-mark-rings :which-key "helm all mark rings")
           "rn" '(lsp-rename :which-key "lsp rename")
           "ri" '(spacemacs/python-remove-unused-imports :which-key "remove unused imports")
           "==" '(blacken-buffer :which-key "black buffer")
           "'" '(spacemacs/python-start-or-switch-repl :which-key "python repl")
           "sl" '(spacemacs/python-shell-send-line :which-key "send line")
           "sf" '(spacemacs/python-shell-send-defun :which-key "send defun")
           "sc" '(spacemacs/python-shell-send-defun :which-key "send class")
           "sr" '(spacemacs/python-shell-send-region :which-key "send region")
           )
  (:states '(insert emacs)
           :keymaps 'inferior-python-mode-map
           "C-l" '(spacemacs/comint-clear-buffer :which-key "clear buffer"))
  )

(provide 'init-python)
;;; init-python.el ends here
