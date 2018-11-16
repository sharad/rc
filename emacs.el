;;; XEmacs backwards compatibility file
(eval-after-load 'package
  '(progn
     (message "package-archives: %s" package-archives)
     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
     (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
     (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
     (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when nil
  (progn
    (message "package-archives: %s" package-archives)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))))

(let* ((dist-dir (getenv "EMACS_DIST_DIR"))
       (dist-dir
        (if dist-dir
            (cond
             ((file-directory-p dist-dir)
              dist-dir)
             ((file-directory-p (expand-file-name dist-dir (getenv "HOME")))
                            (expand-file-name dist-dir (getenv "HOME")))
             (t (expand-file-name ".xemacs" "~")))
          (expand-file-name ".xemacs" "~"))))

  (setq user-init-file
        (expand-file-name "init.el"
                          dist-dir
                          ;; (expand-file-name ".xemacs" "~")
                          ;; (expand-file-name "~/.emacs.d")
                          ))
  (if (string-equal dist-dir (expand-file-name ".xemacs" "~"))
      (setq custom-file
        (expand-file-name "custom.el"
                          (expand-file-name ".xemacs" "~"))))
  (message "%s %s" dist-dir user-init-file)
  (load-file user-init-file))
;; (load-file custom-file)
