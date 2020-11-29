;;; init-tex.el --- set latex and pdf. -*- lexical-binding: t _*_

;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :pin melpa)

(use-package tex
  :ensure auctex
)


(provide 'init-tex)
;;; init-tex.el ends here
