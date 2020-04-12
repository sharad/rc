
(in-package :stumpwm)

(defun local-set-contrib-dir ())
  
;;{{
(defun stumpwm-initialize-asdf ()
  (let* ((asdf-files '("/run/current-system/profile/share/sbcl/contrib/asdf/asdf.lisp"))
         (asdf-files (member-if #'probe-file asdf-files))
         (asdf-file  (car asdf-files)))
    (when asdf-file
      (message "found asdf file ~a" asdf-file)
      (load asdf-file)
      (asdf:clear-source-registry)
      (push #p"/run/current-system/profile/profile/lib/sbcl/" asdf:*central-registry*)
      (push #p"/run/current-system/profile/profile/lib/sbcl/contrib/" asdf:*central-registry*)
      (push #p"/home/s/hell/.guix-profile/lib/sbcl/" asdf:*central-registry*)
      (push #p"/home/s/hell/.guix-profile/lib/sbcl/contrib/" asdf:*central-registry*)
      ;; (asdf:initialize-source-registry)
      )))

(stumpwm-initialize-asdf)
(asdf:initialize-source-registry)
;;}}
