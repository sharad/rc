

(defun setup-emacs-in-fresh-system ()
  (interactive)
  (progn
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'package-build)
    (load-file "~/.xemacs/elpa/pkgs/package-dev-utils-lotus/package-dev-utils-lotus.el")
    (package-install-packages-from-source-path)))
