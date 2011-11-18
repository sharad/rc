;; -*-lisp-*-
;;
;; utils.lisp ------------------------------------------------------------

(in-package :stumpwm)


(defun change-dir (path)
  (let ((sdir (truename (cl:pathname path))))
    (if (cl:probe-file sdir)
        (progn
          (setf *default-pathname-defaults* sdir)
          #+(and clisp linux)
          (linux:|chdir| dir)
          #+sbcl
          (sb-posix:chdir sdir))
        (message "No Such dir exists. Good bye"))))


(defun get-current-directory ()
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+(or cmu scl) (ext:default-directory)
  #+sbcl (sb-unix:posix-getcwd/)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+mcl (ccl:mac-default-directory)
  #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename "."))

;; from: http://files.b9.com/lboot/utils.lisp
(defun pathname-drive-letter (path)
  "Enscapsulate difference in handling Win32 drive letters"
  #-lispworks (pathname-device path)
  #+lispworks (pathname-host path)
  )

(defun find-directory (pathlist)
  "Returns a pathname of a path that exists while searching a list of
candidates. Candidate is a list of a drive letter(or nil) and a directory"
  (declare (list pathlist))
  (let ((loc (find-if
	      #'(lambda (loc)
		  (let ((dir (make-pathname
			      #-lispworks :device
			      #+lispworks :host
			      (first loc)
			      :directory (second loc))))
		    (when (directory dir)
		      t)))
	      pathlist)))
    (when loc
      (make-pathname
       #-lispworks :device
       #+lispworks :host
       (first loc)
       :directory (second loc)))))


;; (defparameter *lisp-lib-path*
;;   (find-directory
;;    `((,(pathname-drive-letter *my-lisp-path*)
;;       (:absolute "opt" "lisp"))
;;      (,(pathname-drive-letter *my-lisp-path*)
;;       (:absolute "Users" "kevin" "lisp-lib"))))
;;   "Pathname for the root directory of my Lisp program storage.")

;; (defun pathname-relative-lisp-lib (dirs &optional name type)
;;   (pathname-relative *lisp-lib-path* dirs name type))

(defun cwd (&optional dir)
  "Change directory and set default pathname"
  (cond
   ((not (null dir))
    (when (and (typep dir 'logical-pathname)
	       (translate-logical-pathname dir))
      (setq dir (translate-logical-pathname dir)))
    (when (stringp dir)
      (setq dir (parse-namestring dir)))
    #+allegro (excl:chdir dir)
    #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
    #+(or cmu scl) (setf (ext:default-directory) dir)
    #+cormanlisp (ccl:set-current-directory dir)
    #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
    #+openmcl (ccl:cwd dir)
    #+gcl (si:chdir dir)
    #+lispworks (hcl:change-directory dir)
    (setq cl:*default-pathname-defaults* dir))
   (t
    (let ((dir
	   #+allegro (excl:current-directory)
	   #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
	   #+(or cmu scl) (ext:default-directory)
	   #+sbcl (sb-unix:posix-getcwd/)
	   #+cormanlisp (ccl:get-current-directory)
	   #+lispworks (hcl:get-working-directory)
	   #+mcl (ccl:mac-default-directory)
	   #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename ".")))
      (when (stringp dir)
	(setq dir (parse-namestring dir)))
      dir))))


(defun directory-up (n-levels path)
  "Return a directory list less n-levels elements from the tail"
  (etypecase path
    (pathname
     (let ((dir (pathname-directory path)))
       (butlast dir n-levels)))
    (cons
     (when (> (length path) n-levels)
       (butlast path n-levels)))))

;; (defun quit (&optional (code 0))
;;   "Function to exit the Lisp implementation."
;;     #+allegro (excl:exit code :quiet t)
;;     #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
;;     #+(or cmu scl) (ext:quit code)
;;     #+cormanlisp (win32:exitprocess code)
;;     #+gcl (lisp:bye code)
;;     #+lispworks (lw:quit :status code)
;;     #+lucid (lcl:quit code)
;;     #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
;;     #+mcl (ccl:quit code)
;;     #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl)
;;     (error 'not-implemented :proc (list 'quit code)))

(defun un-unspecific (value)
  "Convert :UNSPECIFIC to NIL."
  (if (eq value :unspecific) nil value))

;; (defun probe-directory (filename)
;;   "Check whether the file name names an existing directory."
;;   (let* ((path (pathname filename))
;;          (name (un-unspecific (pathname-name path)))
;;          (type (un-unspecific (pathname-type path)))
;;          (new-dir
;;           (cond ((and name type) (list (concatenate 'string name "." type)))
;;                 (name (list name))
;;                 (type (list type))
;;                 (t nil))))
;;     (when new-dir
;;       (setq path (make-pathname
;;                   :directory (append (un-unspecific (pathname-directory path))
;;                                      new-dir)
;;                   :name nil :type nil :version nil :defaults path)))
;;     #+allegro (excl::probe-directory path)
;;     #+clisp (values
;;              (ignore-errors
;;                (#+lisp=cl ext:probe-directory #-lisp=cl lisp:probe-directory
;;                           path)))
;;     #+(or cmu scl) (eq :directory (unix:unix-file-kind (namestring path)))
;;     #+lispworks (lw:file-directory-p path)
;;     #+sbcl (eq :directory (sb-unix:unix-file-kind (namestring path)))
;;     #-(or allegro clisp cmu lispworks sbcl scl)
;;     (probe-file path)))

;; (defun load-file-if-exists (path)
;;   (when (probe-file path)
;;     (load path)
;;     path))

;; (defun shell-parse (string &optional (seperator ))
;;   ())



;; (defun prefixcommand () ()
;; support both number and symbol (gnus) prefix.
;;        ;; read prefix
;;        ;; pass it by dynamix variable
;;        ;; call actual function.
;;        )
