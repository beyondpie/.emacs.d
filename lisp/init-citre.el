;;; init-citre.el --- Initialize Citre configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: seagle0128/.emacs.d: init-lsp.el


;;; Code:

(use-package citre
  :defer t
  :diminish
  :init
  (require 'citre-config)
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :functions projectile-project-root
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   citre-readtags-program "/usr/local/bin/readtags"
   citre-ctags-program "/usr/local/bin/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t)
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           :keymaps 'prog-mode-map
           "jj" '(citre-jump :which-key "citre jump")
           "jJ" '(citre-peek-next-line :which-key "citre peek next line")
           "jK" '(citre-peek-prev-line :which-key "citre peek prev line")
           "j[" '(citre-peek-prev-definition :which-key "citre peek prev def")
           "j]" '(citre-peek-next-definition :which-key "citre peek next def")
           "j{" '(citre-peek-chain-backward :which-key "citre peek chain backward")
           "j}" '(citre-peek-chain-forward :which-key "citre peek chain forward")
           "jp" '(citre-peek :which-key "citre peek")
           "ja" '(citre-ace-peek :which-key "citre ace peek")
           "jb" '(citre-jump-back :which-key "citre jump back")
           "jP" '(citre-peek-through :which-key "citre peek through")
           "jS" '(citre-peek-save-session :which-key "citre peek restore")
           )
  )

(provide 'init-citre)
;;; init-citre.el ends here
