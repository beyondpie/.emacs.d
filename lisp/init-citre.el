;;; init-citre.el --- Initialize Citre configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: seagle0128/.emacs.d: init-lsp.el


;;; Code:

(use-package citre
  :diminish
  :functions projectile-project-root
  :bind (("C-x c j" . citre-jump)
         ("C-x c k" . citre-jump-back)
         ("C-x c p" . citre-peek)
         ("C-x c P" . citre-ace-peek))
  ;; :hook (prog-mode . citre-auto-enable-citre-mode)
  :config
  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
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
  (setq citre-ctags-program "/usr/local/bin/ctags")
  )

(provide 'init-citre)
;;; init-citre.el ends here
