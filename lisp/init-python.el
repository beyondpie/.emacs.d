;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: spacemacs

;;; Code:
(require 'init-const)
(require 'init-prog)

(defun spacemacs//python-setup-shell ()
  "Set up python shell
   Note: need to re-run after conda activation."
  (interactive)
  (if (executable-find "ipython")
      (progn
        (setq python-shell-interpreter "ipython")
        ;; ipython version >= 5
        (setq python-shell-interpreter-args "--simple-prompt -i")
        )
    (progn
      (setq python-shell-interpreter "python")
      )
    )
  (message "python interpreter is %s." python-shell-interpreter)
  )

(defun spacemacs//python-default ()
  "Default settings for python buffers"
  (setq-local mode-name "py"
        tab-width 4
        fill-column 80)
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

(defun spacemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(use-package python-black
  :after python
  :commands (python-black-buffer python-black-region))

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package python-mode
  :hook ((python-mode python-ts-mode) . (lambda () (progn
                                                     (spacemacs//python-default)
                                                     (spacemacs//python-setup-shell))))
  :init
  (setq python-ts-mode-hook python-mode-hook)
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil)
  (if (executable-find (nth 0 python-flymake-command))
      (setq python-flymake-command python-flymake-command)
    )
  (setq python-indent-def-block-scale 1)
  :config
  (setq-default python-indent-guess-indent-offset nil)
  :general
  (:states '(normal visual)
           :keymaps 'python-mode-map
           :prefix beyondpie/major-mode-leader-key
           "go" '(helm-occur :which-key "helm occur")
           "'" '(spacemacs/python-start-or-switch-repl :which-key "repl")
           "sl" '(spacemacs/python-shell-send-line :which-key "send line")
           "sf" '(spacemacs/python-shell-send-defun :which-key "send defun")
           "sc" '(spacemacs/python-shell-send-defun :which-key "send class")
           "sr" '(spacemacs/python-shell-send-region :which-key "send region")
           "ri" '(spacemacs/python-remove-unused-imports :which-key "clean import")
           "rB" '(python-black-buffer :which-key "black buffer")
           "rR" '(python-black-region :which-key "black region")
           "rp" '(spacemacs//python-setup-shell :which-key "setup python shell")
           )
  (:states '(insert emacs)
           :keymaps 'inferior-python-mode-map
           "C-l" '(spacemacs/comint-clear-buffer :which-key "clear buffer"))
  :general
  (:states '(normal visual)
           :keymaps 'python-ts-mode-map
           :prefix beyondpie/major-mode-leader-key
           "go" '(helm-occur :which-key "helm occur")
           "'" '(spacemacs/python-start-or-switch-repl :which-key "python repl")
           "sl" '(spacemacs/python-shell-send-line :which-key "send line")
           "sf" '(spacemacs/python-shell-send-defun :which-key "send defun")
           "sc" '(spacemacs/python-shell-send-defun :which-key "send class")
           "sr" '(spacemacs/python-shell-send-region :which-key "send region")
           "ri" '(spacemacs/python-remove-unused-imports :which-key "clean import")
           "rB" '(python-black-buffer :which-key "black buffer")
           "rR" '(python-black-region :which-key "black region")
           "rp" '(spacemacs//python-setup-shell :which-key "setup python shell")
           )
  )
(provide 'init-python)
;;; init-python.el ends here
