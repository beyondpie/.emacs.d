;;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(global-set-key (kbd "C-SPC") 'set-mark-command)
 
;; for tabs
(setq-default indent-tabs-mode nil
              default-tab-width 2
              tab-width 2)
;; ** language
(ispell-change-dictionary "american" t)
(define-coding-system-alias 'UTF-8 'utf-8)

;; files
;; from "writing GNU Emacs Extensions"
(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn (setq buffer-read-only t)
             (message "File is a symlink."))
    ))

(add-hook 'find-file-hooks 'read-only-if-symlink)

;; https://www.murilopereira.com/a-rabbit-hole-full-of-lisp/
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; remove compling in the mode-line
;; ref: https://emacs.stackexchange.com/questions/61957/mode-line-always-shows-compiling-after-compile-a-tex-file-with-typos?newreg=6aa1e0e4e19b423a9bce34c66bacc1e4
(setq compilation-in-progress nil)


(defun beyondpie/simplify-minibatch-emacs ()
  (interactive)
  (use-package emacs
    :delight
    (auto-fill-function " AF")
    (visual-line-mode)
    (eldoc-mode)
    (auto-revert-mode)
    (dired-mode)
    (winner-mode)
    (ess-r-mode "R")
    (windmove-mode)
    (flymake-mode)
    (evil-collection-unimpaired-mode)
    (global-evil-collection-unimpaired-mode)
    (helm-mode)
    )
  )

;; remove up/down case keys due to they usually make my codes typo
;; upcase-region
(global-unset-key (kbd "C-x C-u"))
;; upcase-word
(global-unset-key (kbd "M-u"))
;; downcase-word
(global-unset-key (kbd "M-l"))
;; downcase-region
(global-unset-key (kbd "C-x C-l"))

(provide 'init-utils)
;;; init-utils.el ends here
