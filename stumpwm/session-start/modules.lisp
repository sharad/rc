

(in-package :stumpwm)

;;  #-quicklisp
(defvar *contrib-dir* #p"/usr/local/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib/")

(defun load-external-module (module)
  #+quicklisp
  (if (ql:where-is-system module)
      (ql:quickload module)
      (message "failed to load ~a" module))
  #-quicklisp
  (when (and
         (boundp '*contrib-dir*)
         (probe-file *contrib-dir*))
    (stumpwm:load-module module)
    (stumpwm::message "failed to load ~a" module)))

;; (add-to-load-path #p"~/.stumpwm.d/modules")

;;{{{ Load module
(defun list-directory-resursively (dir &key (predicate t))
  (flatten
   (mapcar
    #'(lambda (e)
        (append
         (when (or
                (eq predicate t)
                (funcall predicate e))
           (list e))
         (list-directory-resursively e :predicate predicate)))
    (when (cl-fad:directory-pathname-p dir)
      (cl-fad:list-directory dir)))))

(defun stumpwm-contrib-modules (dir)
  (reverse
   (mapcar
    #'(lambda (asd-path) (car (last (pathname-directory asd-path))))
    (list-directory-resursively
     dir
     :predicate
     #'(lambda (path)
         (string-equal (pathname-type path) "asd"))))))

(defvar stumpwm-contrib-exclude-modules '("notify ""qubes"))

(defun stumpwm-contrib-included-modules (dir)
  (set-difference
   (stumpwm-contrib-modules dir)
   stumpwm-contrib-exclude-modules
   :test #'string-equal))

(defun load-all-modules ()
  (dolist
      (mod
       (append
        (stumpwm-contrib-included-modules *contrib-dir*)
        '(list "notify")))
    (stumpwm::message "loading ~a" mod)
    (ignore-errors
     (stumpwm::load-external-module mod))))

(load-all-modules)

;; enable
#+stumptray
(when (fboundp 'stumptray:stumptray)
  (stumptray:stumptray))

#+clipboard-history
(progn
  (define-key *root-map* (kbd "C-y") "show-clipboard-history")
  ;; start the polling timer process
  (clipboard-history:start-clipboard-manager))

(defcommand load-all-eexternal-modules () ()
  (load-all-modules))

;; (load-external-module "wmii-like-stumpwmrc")

;;}}}
