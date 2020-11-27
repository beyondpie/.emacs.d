;; init-prog.el --setting general configs for init-prog -*- lexical-binding: t -*-

(require-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-method 'character)

(provide 'init-prog)
