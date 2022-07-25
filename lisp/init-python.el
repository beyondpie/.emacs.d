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
    (pop-to-buffer (process-buffer shell-process))))

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

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode #'spacemacs//python-default)
(spacemacs//python-setup-shell)
(setq python-indent-offset 4)
(setq python-shell-completion-native-enable nil)
(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (setq-default python-indent-guess-indent-offset nil))

(with-eval-after-load 'python-mode
  (define-key inferior-python-mode-map (kbd "C-l") 'spacemacs/comint-clear-buffer)
  (define-key python-mode-map (kbd "C-c '") 'spacemacs/python-start-or-switch-repl)
  (define-key python-mode-map (kbd "C-c s l") 'spacemacs/python-shell-send-line)
  (define-key python-mode-map (kbd "C-c s f") 'spacemacs/python-shell-send-defun)
  (define-key python-mode-map (kbd "C-c s r") 'spacemacs/python-shell-send-region)
  )

(provide 'init-python)
;;; init-python.el ends here
