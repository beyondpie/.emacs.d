;;; init-elfeed.el --- Initialize Elfeed configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Ref: seagle0128/.emacs.d: init-lsp.el


;;; Code:
(use-package elfeed
  :diminish
  :ensure t
  :defer t
  :functions elfeed
  :init
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"))
  :general
  (:states '(normal visual)
           :keymaps 'elfeed-search-mode-map
           :prefix beyondpie/major-mode-leader-key
            "h" #'describe-mode
            "q" #'elfeed-search-quit-window
            "g" #'elfeed-search-update--force
            "G" #'elfeed-search-fetch
            "s" #'elfeed-search-live-filter
            "S" #'elfeed-search-set-filter
            "c" #'elfeed-search-clear-filter
            "b" #'elfeed-search-browse-url
            "y" #'elfeed-search-yank
            "u" #'elfeed-search-tag-all-unread
            "r" #'elfeed-search-untag-all-unread
            "n" #'next-line
            "p" #'previous-line
            "+" #'elfeed-search-tag-all
            "-" #'elfeed-search-untag-all
            "<" #'elfeed-search-first-entry
           )
  )


(provide 'init-elfeed)
;;; init-elfeed.el ends here
