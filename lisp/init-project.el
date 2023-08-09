;;; init-project.el --- Setting project within frames -*- lexical-binding: t -*-


;;; Commentary:
;; Use project.el directly supported by Emacs 28
;; Keep SPC-j-p as project-switch-project key bindings
;; Find files with the package below
;; https://github.com/redguardtoo/find-file-in-project
;;; Code:

(general-define-key
 :states '(normal visual insert emacs)
 :prefix beyondpie/normal-leader-key
 :non-normal-prefix beyondpie/non-normal-leader-key
 :keymaps 'override
 "pp" '(project-switch-project :which-key "project switch")
 "pf" '(project-find-file :which-key "project find file")
 "pb" '(project-switch-to-buffer :which-key "project switch buffer")
 "pk" '(project-kill-buffers :which-key "project kill all buffers")
 "pd" '(project-dired :which-key "project dired")
 "ps" '(project-shell :which-key "project shell")
 "pq" '(project-find-regexp :which-key "project regrex search")
 )


(use-package find-file-in-project
  :init (slot/vc-install :fetcher "github" :repo "redguardtoo/find-file-in-project")
  
  )

(provide 'init-project)
;;; init-project.el ends here
