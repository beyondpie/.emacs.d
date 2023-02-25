
;;; init-conda.el --- Completion with conda -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; https://github.com/necaris/conda.el

(use-package conda
  :delight
  :init
  (if (eq system-type 'darwin)
      (setq conda-anaconda-home (expand-file-name "~/mambaforge"))
    (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
    )
  :config
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)
  :commands
  (conda-env-activate conda-env-deactivate conda-env-activate-for-buffer
                      conda-env-initialize-eshell)
)

;;; Code:
(provide 'init-conda) 
;;; init-conda.el ends here
