;; ensure we elc files.



;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp/emacs-lisp")
;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp")

(require 'cl nil nil)
(load-file "~/.xemacs/macros.el")
(load-file "~/.xemacs/utils.el")
(load-file "~/.xemacs/basic.el")

(add-to-list 'load-path "/usr/local/share/emacs/23.3/site-lisp")

(progn
  (defun package-dir-setup (package-dir)
    (when (file-directory-p package-dir)
      (mapc #'(lambda (path)
                (add-to-list 'load-path path))
            (directory-files package-dir t "[a-zA-Z]+"))
      (mapc #'(lambda (dir)
                (byte-recompile-directory dir 0))
            (directory-files package-dir t "[a-zA-Z]+"))))


 ;; (package-dir-setup "~/.xemacs/pkgrepos/world")
  (package-dir-setup "~/.xemacs/pkgrepos/mypkgs")
  (package-dir-setup "~/.xemacs/pkgrepos/elpa")
  (package-dir-setup "~/.xemacs/pkgrepos/world"))

(mapc
 '(lambda (dir)
   (byte-recompile-directory dir 0)
   (add-to-list 'load-path dir))
 `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
   "~/.osetup/info/common/elisp"
  ,(concat "~/.osetup/info/hosts/" (system-name) "/elisp")))

(deh-require-maybe dot-emacs-helper)

(when (require 'cl nil) ; a rare necessary use of REQUIRE
  ; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
  (defvar *emacs-load-start* (current-time)))
;;

(require 'general-testing)
(irequire 'common-info)



(when (file-exists-p (setq custom-file "~/.xemacs/custom.el"))
  (load-file custom-file))

(when (file-exists-p (setq custom-override-file "~/.xemacs/hand-custom.el"))
  (load-file custom-override-file))




(when (xrequire 'server)
  (setq server-use-tcp t
        server-name (or (getenv "EMACS_SERVER_NAME") server-name))
  (setq server-host (system-name))
  (if (functionp 'server-running-p)
      (when (not (server-running-p))
	(server-start)))
  (message (concat "SERVER: " server-name))
  (when (server-running-p "general")
    (message (concat "YES SERVER: " server-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all files present in ~/\.xemacs/session-start\.d directory.
(defconst *work-dir* "~/\.\./paradise")

(require-dir-libs "~/\.xemacs/pkgrepos/mypkgs/session-start")


;;end



;; (when (boundp '*emacs-load-start*)
;;   ;; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
;;     (message "My .emacs loaded in %ds"
;;              (destructuring-bind (hi lo ms) (current-time)
;;                (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*))))))


;; (message "My .emacs loaded in %s" (emacs-init-time))


;; (sharad/enable-startup-inperrupting-feature)



