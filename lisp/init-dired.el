;;; init-dired.el --- Configure dired -*- lexical-binding: t -*-
;;; Commentary:
;; Ref: Purcell
;;; Code:
(setq dired-listing-switches "-aBhl --group-directories-first")
;; https://www.emacs.dyerdwelling.family/emacs/20240120084016-emacs--dired-async-mode/
(defun my/rsync (dest)
  "Rsync copy."
  (interactive
    (list
      (expand-file-name (read-file-name "rsync to:"
                          (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
         (command "rsync -arvz --progress --no-g "))
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    (setq command (concat command (shell-quote-argument dest)))
    (async-shell-command command "*rsync*")
    (dired-unmark-all-marks)
    (other-window 1)
    (sleep-for 1)
    (dired-revert)
    (revert-buffer nil t nil)))


(with-eval-after-load 'dired
  (dired-async-mode 1)
  (setq-default dired-dwim-target t)
  ;; https://emacs-china.org/t/emacs/23850/8
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (require 'dired-x)
  ;; Hook up dired-x global bindings without loading it up-front
  (define-key ctl-x-map "\C-j" 'dired-jump)
  (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'dired-mode-map
   "RET" 'dired-find-file
   "m" 'dired-mark
   "D" 'dired-do-delete
   "d" nil
   "g" 'revert-buffer
   "u" 'dired-unmark
   "+" 'dired-create-directory
   "C" 'dired-do-copy
   "R" 'dired-do-rename
   "(" 'dired-hide-details-mode
   "!" 'dired-do-shell-command
   "S" 'hydra-dired-quick-sort/body
   ))

(use-package diff-hl
  :pin melpa
  :hook (dired-mode . diff-hl-dired-mode)
  )

(use-package dired-quick-sort
  :pin melpa
  :init
  (setq dired-quick-sort-suppress-setup-warning t)
  :config
  (with-eval-after-load 'dired
    (dired-quick-sort-setup))
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

(provide 'init-dired)
;;; init-dired.el ends here
