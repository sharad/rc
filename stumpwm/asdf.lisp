
(in-package :stumpwm)

(defun local-set-contrib-dir ())
  
;;{{
(defun stumpwm-initialize-asdf ()
  (let* ((asdf-files '("/run/current-system/profile/share/sbcl/contrib/asdf/asdf.lisp"))
         (asdf-files (member-if #'probe-file asdf-file))
         (asdf-file  (car asdf-files)))
    (when asdf-file
      (load asdf-file)
      (asdf:clear-source-registry)
      (asdf:initialize-source-registry))))

(stumpwm-initialize-asdf)
;;}}
