;;; init-telega.el --- Initialize telega. -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(use-package alert
  :ensure t )
(use-package telega
  :ensure t
  ;; :pin melpa-stable
  :pin melpa
  :commands (telega)
  :defer t
  :config
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (if (evil-mode)
      (progn 
        (evil-set-initial-state 'telega-root-mode 'emacs)
        (evil-set-initial-state 'telega-chat-mode 'emacs)
        )
    )
  )

(provide 'init-telega)
;;; init-telega.el ends here
