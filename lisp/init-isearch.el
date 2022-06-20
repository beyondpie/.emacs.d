;;; init-isearch.el --- isearch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package anzu
  :ensure t
  :pin melpa
  :hook (after-init . global-anzu-mode)
  :delight
  :general
  (:states '(normal visual insert emacs)
           :prefix beyondpie/normal-leader-key
           :non-normal-prefix beyondpie/non-normal-leader-key
           "rq" '(anzu-query-replace :which-key "anzu-query-replace")
           "rg" '(anzu-query-replace-regexp :which-key "anzu-query-replace-regexp")
           "rc" '(anzu-replace-at-cursor-thing :which-key "anzu-replace-at-cursor")
           ))

(provide 'init-isearch)
;;; init-isearch.el ends here
