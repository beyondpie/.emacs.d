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

;; TMP: upgrade built-in package as well if they are also in elpa
;; such as eldoc, eglot, use-package and others.
;; Error: package-install-upgrade-built-in <= no such function
;; From emacs-devel discussion

;; (defun package-menu--find-upgrades-tmp ()
;;   "In Package Menu, return an alist of packages that can be upgraded.
;; The alist has the same form as `package-alist', namely a list
;; of (PKG . DESCS), but where DESCS is the `package-desc' object
;; corresponding to the newer version."
;;   (let (installed available upgrades)
;;     ;; Build list of installed/available packages in this buffer.
;;     (dolist (entry tabulated-list-entries)
;;       ;; ENTRY is (PKG-DESC [NAME VERSION STATUS DOC])
;;       (let ((pkg-desc (car entry))
;;             (status (aref (cadr entry) 2)))
;;         (cond (
;;                ;; allow build in
;;                (member status (append '("installed" "dependency" "unsigned" "external")
;;                                       (and package-install-upgrade-built-in '("built-in"))))
;;                (push pkg-desc installed))
;;               ((member status '("available" "new"))
;;                (setq available (package--append-to-alist pkg-desc available))))))
;;     ;; Loop through list of installed packages, finding upgrades.
;;     (dolist (pkg-desc installed)
;;       (let* ((name (package-desc-name pkg-desc))
;;              (avail-pkg (cadr (assq name available))))
;;         (and avail-pkg
;;              ;; update build-in as well
;;              (or (version-list-< (package-desc-priority-version pkg-desc)
;;                                  (package-desc-priority-version avail-pkg))
;;                  (and package-install-upgrade-built-in
;;                       (package--active-built-in-p pkg-desc)))
             
;;              (push (cons name avail-pkg) upgrades))))
;;     upgrades))

;; (advice-add 'package-menu--find-upgrades :override #'package-menu--find-upgrades-tmp)
;; (advice-remove 'package-menu--find-upgrades #'package-menu--find-upgrades-tmp)

(provide 'init-elpa)
;;; init-elpa.el ends here
