;;; init-elpa.el --- Package settings -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: purcell and seagle
;; - Use use-package in Emacs29
;; - Use package-update in Emacs29

;;; Code:

(require 'package)
(require 'cl-lib)

;; https://emacs-china.org/t/native-compilation/23316
;; Stop native comp for 3rd packages, and only use it for native packages.
(setq native-comp-deferred-compilation nil)

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
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
;; Keep the codes for emacs without use-package by default
;; use-package is in Emacs-code in Emacs29.

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

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

;; Only works with emacs >= 29
;; https://tony-zorman.com/posts/package-vc-install.html
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(provide 'init-elpa)
;;; init-elpa.el ends here
