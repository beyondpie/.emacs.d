;;; doggypoo.el                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Songpeng Zu

;; Author: Songpeng Zu <zusongpeng@gmail>
;; Keywords: doggypoo, lisp
;; Version: 0.0.1
;; URL: https://github.com/songpeng/doggypoo.git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description of the package
;; DOGGYPOO is a smart, lovely, and loyalty virtual dog.

;; Doggypoo: loyalty.
;;; Code:

;; code goes here

(defgroup doggypoo nil
  "A smart, lovely, loyalty dog named doggypoo."
  :group 'pets)

(defcustom doggypoo-whose-name "doggypoo"
  "The name of the dog."
  :type 'string
  :group 'doggypoo)

(defvar doggypoo-mode nil
  "Determines if doggypoo minor mode is active.")
(make-variable-buffer-local 'doggypoo-mode)

(defvar doggypoo-mode-map (make-sparse-keymap))

(defvar doggypoo-mode-menu nil)

;;;###autoload
(defun wake-up-doggypoo ()
  "Wake up doggypoo minor mode."
  (doggypoo-mode t))

(defun doggypoo-mode (&optional arg)
  "Minor mode for a smart, lovely, and loyalty dog."
  (interactive "P")
  (setq doggypoo-mode (not (or (and (null arg) doggypoo-mode)
                               (<= (prefix-numeric-value arg) 0))))
  (if doggypoo-mode
      (progn
        (easy-menu-add doggypoo-mode-menu)
        (run-hooks 'doggypoo-mode-hook)))
  )

(defun beyondpie-doggypoo/hi ()
  (interactive)
  (message "Master, Doggypoo is here!")
  )

(defun beyondpie-doggypoo/wake-up ()
  (interactive)
  (message "Zzzzzzzz---"))

(provide 'doggypoo)
