;;; init-macos.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      )

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro
                     (concat "<" multiple "wheel-" direction ">")) 'ignore)))
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-˙") 'ns-do-hide-others)

(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "M-h") nil))

;; what describe-key reports for cmd-option-h
(global-set-key (kbd "M-ˍ") 'ns-do-hide-others)

;; dired setup
(with-eval-after-load 'dired
  (setq dired-use-ls-dired t
	      insert-directory-program "/usr/local/bin/gls"
	      dired-listing-switches "-aBhl --group-directories-first"
	      )
  )

;; Ref: purcell
(defun sanityinc/maybe-suspend-frame ()
  "Stop Ctrl z from minimizing windows under OS X."
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))
(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)

(defvar dictionary-server "dict.org")

(use-package osx-dictionary
  :ensure t)

(general-define-key
 :states '(normal visual motion)
 :prefix beyondpie/normal-leader-key
 :keymaps 'override
 "ds" '(dictionary-search :which-key "dictionary-search")
 "dd" '(osx-dictionary-search-word-at-point
        :which-key "osx-dictionary word at paint")
 )

;; use minions instead
;; (beyondpie/simplify-minibatch-emacs)

;; (use-package nyan-mode
;;   :pin melpa
;;   :hook (after-init . nyan-mode))

(provide 'init-macos)
;;; init-macos.el ends here
