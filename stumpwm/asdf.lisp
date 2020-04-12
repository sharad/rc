
(in-package :stumpwm)

;;{{
(defun stumpwm-initialize-asdf ()
  (let* ((asdf-files '(#p"/run/current-system/profile/share/sbcl/contrib/asdf/asdf.lisp"))
         (asdf-files (member-if #'probe-file asdf-files))
         (asdf-file  (car asdf-files)))
    (when asdf-file
      (message "found asdf file ~a" asdf-file)
      (load asdf-file)
      (dolist (dir '(#p"/run/current-system/profile/lib/sbcl/"
                     #p"~/.guix-profile/lib/sbcl/"
                     #p"/run/current-system/profile/lib/sbcl/contrib/"
                     #p"~/.guix-profile/lib/sbcl/contrib/"
                     ))
        (pushnew dir asdf:*central-registry* :test #'equal)))))
;;}}
