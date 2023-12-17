;;; init-macos.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(require 'init-const)
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
	      insert-directory-program mac-ls
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

;; set safety delete file
(setq trash-directory  "~/.Trash")
 ;;  See ` trash-directory ' as it requires defining ` trash '.
(defun trash (file)
   "Use \"trash\" to move FILE to the system trash."
   (cl-assert (executable-find  "trash") nil
              "' trash ' must be installed. Needs \"brew install trash\"")
   (call-process  "trash" nil 0 nil  "-F"  file))

;; https://www.reddit.com/r/emacs/comments/f8xwau/hack_replace_execpathfromshell/
;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
(defconst my-local-dir (concat user-emacs-directory ".local/"))
(defconst my-env-file (concat my-local-dir "env"))
(defun my-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my-env-file))
  (my-load-envvars-file my-env-file))

(provide 'init-macos)
;;; init-macos.el ends here
