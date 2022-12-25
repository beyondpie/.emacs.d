;;; init-elpa.el --- Package settings -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell and seagle
;; also setup use-package here

;;; Code:

(require 'package)
(require 'cl-lib)


;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
;; from seagle
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages
            :override #'my-save-selected-packages)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
	      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ))

;; FIX cannot find package error after updating pacakges
;; https://www.emacswiki.org/emacs/LoadPath
(setq package-user-dir
      (expand-file-name
       (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory))
(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path)
  )


;; Initialize packages
;; NOTE: if delete this, use-package will not be found
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
;; Keep the codes for emacs without use-package by default
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer nil)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :demand)
(use-package delight
  :ensure t
  :demand)
;; NOTE: add demand in above, so
;; require may not be needed
;; (require 'diminish)

;; TODO: I cannot put this on the top, why?
(require 'bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; auto-package-update
(use-package auto-package-update
  :commands (auto-package-update-now)
  :init
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 14)
  (setq auto-package-update-prompt-before-update t)
  :ensure t
  :pin melpa
  :config
  (defalias 'upgrade-packages #'auto-package-update-now)
)

(provide 'init-elpa)
;;; init-elpa.el ends here
