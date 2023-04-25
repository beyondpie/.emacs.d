;;; init-shell.el --- Set shells

;;; Commentary:
;; Official website: https://github.com/akermu/emacs-libvterm
;; Ref: lazycat
;; Ref: http://www.howardism.org/Technical/Emacs/eshell-fun.html
;; Ref: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; Code:

(setq comint-prompt-read-only t)
(setq shell-command-completion-mode t)

;; (add-hook 'eshell-mode-hook (lambda ()
;;                               (setenv "TERM" "xterm-256color")))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun myeshell ()
  "eshell with window size "
  (interactive)
  (let* (
         (height (/ (window-total-height) 3))
         )
    (split-window-vertically (- height))
    (other-window 1)
    (eshell)
    (insert (concat "ls"))
    (eshell-send-input))
  )


(defun eshell/eexit ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 :keymaps 'override
 "'" '(myeshell :which-key "eshell")
 ";" '(eshell-here :which-key "eshell-here")
 )

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output))
           (git-icon  "\xe0a0")
           (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
      (concat git-repo " " git-icon2 " " git-branch))))

;; set safety delete file
(setq trash-directory  "~/.Trash")
 ;;  See ` trash-directory ' as it requires defining ` trash '.
(defun trash (file)
   "Use \"trash\" to move FILE to the system trash."
   (cl-assert (executable-find  "trash") nil
              "' trash ' must be installed. Needs \"brew install trash\"")
  (call-process  "trash" nil 0 nil  "-F"  file))


(provide 'init-shell)
;;; init-shell.el ends here
