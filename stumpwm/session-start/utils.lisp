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

(defcommand set-debug-level (&optional (level 1)) ((:number "Debug level: "))
  (defparameter *debug-level* level))



(defun select-group (screen query)
  "Attempt to match string QUERY against group number or partial name."
  (labels ((match-num (grp)
             (let ((gn (group-map-number grp)))
               (unless gn
                 (string-equal gn query))))
           (match-whole (grp)
             (string-equal (group-name grp) query))
           (match-partial (grp)
             (let* ((end (min (length (group-name grp)) (length query))))
               (string-equal (group-name grp) query :end1 end :end2 end))))
    (when query
      (or (find-if #'match-num (screen-groups screen))
          (find-if #'match-whole (screen-groups screen))
          (find-if #'match-partial (screen-groups screen))))))

(let (group-stack
      pushed)
  (defun group-stack-switch-to-group (group)
    (let ((old-pushed pushed))
      (setf pushed t)
      (switch-to-group group)
      (setf pushed old-pushed)))
  (defun push-group (group)
    (if (push (screen-current-group (current-screen))
              group-stack)
        (group-stack-switch-to-group group)))
  (defun pop-group ()
    (if group-stack
        (let ((group (pop group-stack)))
          (if (member group (screen-groups (current-screen)))
                (progn
                  (group-stack-switch-to-group group)
                  t)
                (pop-group)))))
  (defun get-group-stack ()
    group-stack)
  (defun reset-group-stack ()
    (setf group-stack nil))
  (defun push-group-callback (n o)
    (let ((pg (if (and (string-equal (group-name o) ".scratchpad")
                       (scratchpad-last-group (current-scratchpad)))
                  (scratchpad-last-group (current-scratchpad))
                  o)))
      (unless (and pushed (not (eq pg (car group-stack))))
        (push pg group-stack)))))


(add-hook *focus-group-hook* 'push-group-callback)

(defcommand gsclear () ()
   (unless (reset-group-stack)
     (message "cleared group stack.")))

(defcommand gspop () ()
   (unless (pop-group)
       (message "group stack is empty.")))

(defcommand gspush (group) ((:group "Select Group: "))
   (when group
     (push-group group)))

(defcommand gspushlist (&optional (fmt *group-format*)) (:rest)
  "Allow the user to select a group from a list, like windowlist but
  for groups"
  (let ((group (second (select-from-menu
		(current-screen)
		(mapcar (lambda (g)
			  (list (format-expand *group-formatters* fmt g) g))
			(screen-groups (current-screen)))))))
    (when group
      (push-group group))))

(defcommand gsshow (&optional (fmt *group-format*)) (:rest)
  (let ((gstack (get-group-stack)))
    (if gstack
        (let* ((*list-hidden-groups* t)
               (groups gstack)
               (names (mapcan (lambda (g)
                                (list*
                                 (format-expand *group-formatters* fmt g)
                                 (when nil ;; verbose
                                   (mapcar (lambda (w)
                                             (format-expand *window-formatters*
                                                            (concatenate 'string "  " wfmt)
                                                            w))
                                           (sort-windows g)))))
                              (if *list-hidden-groups* groups (non-hidden-groups groups)))))
          (echo-string-list (current-screen) names))
        (message "group stack is empty."))))




;;{{
#+climacs
(when (and t (ql:where-is-system "climacs"))
  ;; from: http://g000001.cddddr.org/?TAGS=Climacs
  (defun fun-run-or-raise (fun props &optional (all-groups *run-or-raise-all-groups*) (all-screens *run-or-raise-all-screens*))
    (labels
        ;; Raise the window win and select its frame.  For now, it
        ;; does not select the screen.
        ((goto-win (win)
           (let* ((group (window-group win))
                  (frame (window-frame win))
                  (old-frame (tile-group-current-frame group)))
             (frame-raise-window group frame win)
             (focus-all win)
             (unless (eq frame old-frame)
               (show-frame-indicator group)))))
      (let* ((matches (find-matching-windows props all-groups all-screens))
             ;; other-matches is list of matches "after" the current
             ;; win, if current win matches. getting 2nd element means
             ;; skipping over the current win, to cycle through matches
             (other-matches (member (current-window) matches))
             (win (if (> (length other-matches) 1)
                      (second other-matches)
                      (first matches))))
        (if win
            (goto-win win)
            #+sbcl (sb-thread:make-thread fun
                                          :name (format nil "~A" fun))))))

  (defcommand climacs () ()
              ""
              (fun-run-or-raise (lambda ()
                                  (climacs:climacs :new-process "climacs"))
                                '(:class "Climacs"))))



;;}}


(defun run-on-screen (screen cmd props)
  (switch-to-screen screen)
  (run-or-raise cmd props))

(defun kill-empty-group (win)
  (declare (ignore win))
  (unless (group-windows (current-group))
    (gkill)))

;;{{ move to previous window when current window destroyed
(defun jump-to-previous-window (&optional window (current-window))
  ;; BUG TODO Need lots of improvements.
  (when (eq (type-of (current-group)) 'tile-group)
    (let ((frame (window-frame window))
          (group (window-group window)))
      ;; (message "fm win == win fw ~a cw ~a w ~a" (frame-window frame) (current-window) window)
      ;; (message "if (group-current-window group) ~a   (focus-pre-w) ~a  ow ~a"
      ;;          (group-current-window group)
      ;;          (focus-prev-window group)
      ;;          (other-window group))
      (when (eq (tile-group-current-frame group) frame) ;only for current frame window
        (let ((group (current-group)))
          (unless (group-current-window group) ;unless prev window present
            ;; (focus-prev-window group)
            (other-window group)))))))

(add-hook *destroy-window-hook* #'jump-to-previous-window)
;; (nconc *destroy-window-hook* (list #'jump-to-previous-window))
;;}}


(defun get-dbus-conf ()
  (let ((dbus-parms
         (mapcar
          #'(lambda (envstr) (format nil "~a=~a" envstr (getenv envstr)))
          '("DBUS_SESSION_BUS_ADDRESS" "DBUS_SESSION_BUS_PID" "DBUS_SESSION_BUS_WINDOWID"))))
    (format nil "~{~a~%~}" dbus-parms)))

(defcommand get-dbus-config () ()
            (message "~a" (get-dbus-conf)))
