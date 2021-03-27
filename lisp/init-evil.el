;;; init-evil.el --- Initialize evil. -*- lexical-binding: t -*-

;;; Comentary:

;; evil-collection suggest to use the folllowing to
;; let general override evil-collection binds over SPC.
;; In my case, I only need to use keymaps 'override in eivl or other module
;; when using general to define the keys. 

;; https://github.com/emacs-evil/evil-collection
;; (use-package general
;;   :ensure t
;;   :init
;;   (setq general-override-states '(insert
;;                                   emacs
;;                                   hybrid
;;                                   normal
;;                                   visual
;;                                   motion
;;                                   operator
;;                                   replace))
;;   :config
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'override
;;    "SPC" 'hydra-space/body))
;;    ;; Replace 'hydra-space/body with your leader function.


;;; Code:
(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t
        evil-mode-line-format t
        evil-want-integration t
        evil-want-keybinding nil
        evil-visual-state-cursor '(box "#F86155")
        evil-normal-state-cursor '(box "Orange")
        evil-shift-width 2
        evil-collection-company-use-tng nil
        )
  (defun set-evil-insert-state-cursor ()
    "change evil insert state cusor color based on theme"
    (interactive)
    (if (string= (frame-parameter nil 'background-mode) "light")
        (setq evil-insert-state-cursor '(bar "Black"))
      (setq evil-insert-state-cursor '(bar "White"))))
  :hook ((after-init . evil-mode)
         (after-init . set-evil-insert-state-cursor)
         )
  :config
  (set-evil-insert-state-cursor)
  (setq evil-want-fine-undo t)
  :bind
  (:map evil-normal-state-map
        ("M-." . nil)
        ;; https://github.com/noctuid/evil-guide#use-some-emacs-keybindings
        ;; Note that at any time you can use evil-toggle-key (C-z by default;
        ;; bound to evil-emacs-state) to enter emacs state or \ (bound to
        ;; evil-execute-in-emacs-state) to execute the next command in emacs
        ;; state. In emacs state, evil-toggle-key is bound to switch to the
        ;; previous state. This may not be what you want if you’ve entered emacs
        ;; state from insert state, so you may want to also bind ESC to enter
        ;; normal state
        ;; Note that in this case, attempting to rebind (kbd "ESC") will not work
        ;; in GUI Emacs (and will prevent meta from working if used in the
        ;; terminal). Currently it is not possible to bind
        ;; escape in emacs state for terminal Emacs (see issue #14).
        ("C-e" . move-end-of-line)
        :map evil-emacs-state-map
        ([escape] . evil-normal-state)
        )
  :general
  (:states '(normal visual insert emacs)
           :keymaps 'override
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           ;; https://github.com/noctuid/general.el/issues/99
           "wh" '(evil-window-left :which-key "goto left window")
           "wl" '(evil-window-right :which-key "goto right window")
           "wj" '(evil-window-down :which-key "goto down window")
           "wk" '(evil-window-up :which-key "goto up window")
           "fj" '(dired-jump :which-key "jump dired")
           "ff" '(helm-find-files :which-key "find file")
           "fs" '(save-buffer :which-key "save file")
           "wJ" '(evil-window-move-very-bottom :whick-key "move window upward")
           "wK" '(evil-window-move-very-top :which-key "move window downward")
           "wL" '(evil-window-move-far-right :which-key "move window right")
           "wH" '(evil-window-move-far-left :which-key "move window left")
           )
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  ;; (evil-collection-init 'magit)
  ;; (evil-collection-init 'pdf-tools)
  )

(provide 'init-evil)
;;; init-evil.el ends here
