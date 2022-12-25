;;; init-company.el --- Completion with company -*- lexical-binding: t -*-

;;; Commentary:
;; ref:
;; - Major: Seagle init-company
;;   https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el
;; - Minor: Lazycat: https://manateelazycat.github.io/emacs/2021/06/30/company-multiple-backends.html

;;; Code:

(use-package company
  :delight
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.5
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 2
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords
                                                 company-files)
                           company-dabbrev))
        ;; tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  :config
  (add-to-list 'company-transformers #'delete-dups)
  (with-no-warnings
    ;; Company anywhere
    ;; @see https://github.com/zk-phi/company-anywhere
    (defun company-anywhere-after-finish (completion)
      (when (and (stringp completion)
                 (looking-at "\\(?:\\sw\\|\\s_\\)+")
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (delete-region (match-beginning 0) (match-end 0))))
    (add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

    (defun company-anywhere-grab-word (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
    (advice-add 'company-grab-word :around 'company-anywhere-grab-word)

    (defun company-anywhere-grab-symbol (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
    (advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

    (defun company-anywhere-dabbrev-prefix (_)
      (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
    (advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

    (defun company-anywhere-capf (fn command &rest args)
      (if (eq command 'prefix)
          (let ((res (company--capf-data)))
            (when res
              (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                    (prefix (buffer-substring-no-properties (nth 1 res) (point))))
                (cond
                 (length (cons prefix length))
                 (t prefix)))))
        (apply fn command args)))
    (advice-add 'company-capf :around 'company-anywhere-capf)

    (defun company-anywhere-preview-show-at-point (pos completion)
      (when (and (save-excursion
                   (goto-char pos)
                   (looking-at "\\(?:\\sw\\|\\s_\\)+"))
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
        (let ((after-string (overlay-get company-preview-overlay 'after-string)))
          (when after-string
            (overlay-put company-preview-overlay 'display after-string)
            (overlay-put company-preview-overlay 'after-string nil)))))
    (advice-add 'company-preview-show-at-point :after
                'company-anywhere-preview-show-at-point)
    ;; `yasnippet' integration
    (with-eval-after-load 'yasnippet
      (defun my-company-yasnippet ()
        "Hide the current completeions and show snippets."
        (interactive)
        (company-cancel)
        (call-interactively 'company-yasnippet))

      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq cmd  'prefix)
            (when-let ((prefix (funcall fn 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fn cmd arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))
  )

;; Better sorting and filtering
(use-package company-prescient
  :delight
  :init
  (setq company-prescient-mode 1)
  )
  
;;   ;; quickhelp
  ;; (use-package company-quickhelp
  ;;   :delight
  ;;   :defines company-quickhelp-delay
  ;;   :bind (:map company-active-map
  ;;               ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  ;;   :hook (global-company-mode . company-quickhelp-mode)
  ;;   :init (setq company-quickhelp-delay 5))

(provide 'init-company)
;;; init-company.el ends here
