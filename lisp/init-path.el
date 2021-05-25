;;; init-path.el --- set PATH -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: https://xenodium.com/trying-out-gccemacs-on-macos/
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  ;; for conda env in python-mode
  (add-to-list 'exec-path-from-shell-variables "WORKON_HOME" t)
  (exec-path-from-shell-initialize)
  ;; (if (and (fboundp 'native-comp-available-p)
  ;;          (native-comp-available-p))
  ;;     (progn
  ;;       (message "Native comp is available")
  ;;       ;; Add Emacs.app/Contents/MacOS/bin
  ;;       (add-to-list 'exec-path (concat invocation-directory "bin") t)
  ;;       (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
  ;;                                      (when (getenv "LIBRARY_PATH")
  ;;                                        ":")
  ;;                                      ;; where brew puts gcc libraries
  ;;                                      (car (file-expand-wildcards (expand-file-name "/usr/local/opt/gcc/lib/gcc/*")))
  ;;                                      (when (getenv "LIBRARY_PATH")
  ;;                                        ":")
  ;;                                      (car (file-expand-wildcards (expand-file-name "/usr/local/opt/libgccjit/lib/gcc/*")))
  ;;                                      ))
  ;;       ;; Only set after LIBRARY_PATH can find gcc libraries.
  ;;       (setq comp-deferred-compilation t)
  ;;       )
  ;;   (message "Native comp is *NOT* available.")
  ;;  )
  )

;; (when (or (memq window-system '(mac ns x)) (daemonp))
;;   (exec-path-from-shell-initialize))

(provide 'init-path)
;;; init-path.el ends here
