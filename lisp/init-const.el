;; init-const.el ---settings of extra const values	-*- lexical-binding: t -*-

(defconst beyondpie-homepage
	"https://github.com/beyondpie"
	"My Github page")

;; figures
(defconst mybanner
	(expand-file-name "materials/boston_autumn.png" user-emacs-directory)
	"Image as banner")

(defconst beyondpie/normal-leader-key
  "SPC"
  "evil normal state leader key"
  )

(defconst beyondpie/non-normal-leader-key
  "M-SPC"
  "other states inclusing emacs leader key"
  )

(provide 'init-const)
