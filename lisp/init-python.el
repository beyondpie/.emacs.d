;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: spacemacs

;;; Code:
(require 'init-const)

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
      )
    )
  )

(defun spacemacs//python-default ()
  "Default settings for python buffers"
  (setq-local mode-name "py"
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

(defun spacemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))


(use-package pyvenv
  :ensure t
  :pin melpa
  :hook ((python-mode python-ts-mode) . pyvenv-tracking-mode)
  :general
  (:states '(normal visual)
           :keymaps 'python-mode-map
           :prefix beyondpie/major-mode-leader-key
           "va"  '(pyvenv-activate :which-key "pyvenv activate")
           "vd" '(pyvenv-deactivate :which-key "pyvenv deactivate")
           "vw" '(pyvenv-workon :which-key "pyvenv workon"))
  (:states '(normal visual)
           :keymaps 'python-ts-mode-map
           :prefix beyondpie/major-mode-leader-key
           "va"  '(pyvenv-activate :which-key "pyvenv activate")
           "vd" '(pyvenv-deactivate :which-key "pyvenv deactivate")
           "vw" '(pyvenv-workon :which-key "pyvenv workon"))
  
  :config
  (dolist (func '(pyvenv-actiate pyvenv-deactivate pyvenv-workon))
    (advice-add func :after 'spacemacs//python-setup-shell)
    )
  )

(defun encoder-ipython ()
  "Start and/or switch to the REPL remotely."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             (run-python encoder-ipython)
             (python-shell-get-process)
             )))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun mediator-python ()
  "Start and/or switch to the REPL remotely."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             (run-python mediator-ipython)
             (python-shell-get-process)
             )))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun tscc2-python ()
  "Start and/or switch to the REPL remotely."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             (run-python tscc2-ipython)
             (python-shell-get-process)
             )))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

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
           )
  )
(provide 'init-python)
;;; init-python.el ends here
