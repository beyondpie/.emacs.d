;;; init-const.el --- settings of extra const values	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst beyondpie-homepage
	"https://github.com/beyondpie"
	"My Github page.")

;; figures
(defconst mybanner
	(expand-file-name "materials/boston_autumn.png" user-emacs-directory)
	"Banner for dashboard.")

(defconst beyondpie/normal-leader-key
  "SPC"
  "Evil normal state leader key used as a global leader key."
  )

(defconst beyondpie/non-normal-leader-key
  "M-SPC"
  "Non-normal state leader key."
  )

(defconst beyondpie/major-mode-leader-key
  ","
  "Like spacemacs, use a different leader key for 'major-mode'."
  )

(provide 'init-const)
;;; init-const.el ends here
