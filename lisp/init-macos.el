;;; init-macos.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-word-at-point)
  )

(setq mac-option-modifier 'meta
      mac-right-command-modifier 'super
      mac-command-modifier 'super
      )

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-˙") 'ns-do-hide-others)

(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "M-h") nil))

;; what describe-key reports for cmd-option-h
(global-set-key (kbd "M-ˍ") 'ns-do-hide-others)

;; for naive Emacs lisp, not sure if needed.
;; ref: https://emacs-china.org/t/native-emacs-lisp/11165/396
;; apple version: when I use macOS Catalina 10.15.7, it syas darwin19
(setenv "LIBRARY_PATH"
        "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin19/10.2.0")

;; dired setup
(with-eval-after-load 'dired
  ;; need gnu ls
  (when *is-a-mac*
    (setq dired-use-ls-dired t
	        insert-directory-program "/usr/local/bin/gls"
	        dired-listing-switches "-aBhl --group-directories-first"
	        )
    ))

;; Ref: purcell
;; Stop C-z from minimizing windows under OS X
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))
(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)


(provide 'init-macos)
;;; init-macos.el ends here
