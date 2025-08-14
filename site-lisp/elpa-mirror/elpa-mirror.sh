mkdir -p ~/.emacs.d/myelpa && \
    /Applications/Emacs.app/Contents/MacOS/emacs-nw --batch -l ~/.emacs.d/init.el -l ~/.emacs.d/site-lisp/elpa-mirror/elpa-mirror.el \
    --eval='(setq elpamr-default-output-directory "~/.emacs.d/myelpa")' \
    --eval='(elpamr-create-mirror-for-installed)'
