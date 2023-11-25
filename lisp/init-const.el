;;; init-const.el --- settings of extra const values	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defcustom beyondpie/github-link
  "https://github.com/beyondpie"
  "Github page"
  :type 'string
  :group 'beyondpie
  )

(defcustom beyondpie/normal-leader-key
  "SPC"
  "Evil normal state leader key used as a global leader key."
  :type 'string
  :group 'beyondpie-key
  )

(defcustom beyondpie/non-normal-leader-key
  "M-SPC"
  "Non Evil-nomral state leader key"
  :type 'string
  :group 'beyondpie-key
  )
(defcustom beyondpie/major-mode-leader-key
  ","
  "Like spacemacs, use a different leader key for major-mode"
  :type 'string
  :group 'beyondpie-key
  )

(defcustom beyondpie/citre-readtags-program
  "/usr/local/bin/readtags"
  "citre program"
  :type 'string
  :group 'beyondpie-program
  )
()

(provide 'init-const)
;;; init-const.el ends here
