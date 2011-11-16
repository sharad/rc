
;; (defpackage :pa.driver.emacs.planner
;;   ;; I am not using stumpwm, pa any one.
;;   ;; pa is going to use me.
;;   (:use :common-lisp :cl-ppcre)
;;   ;(:export #:*net-device*)
;;   )
;; (in-package :pa.driver.emacs.planner)

(in-package :stumpwm)

;; utils funs from: http://cl-cookbook.sourceforge.net/strings.html#reverse
(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun join-string-list (&rest string-list)
  "Concatenates a list of strings
and puts spaces between the elements."
  (format nil "~{~A~^ ~}" string-list))

(in-package :stumpwm)                   ;I have to have it, else thng will not run in stumpwm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs connection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *emacsclient-command* (join-string-list "emacsclient -f" (concat (getenv "HOME") "/.emacs.d/server/general") "-e")
  "Emacs client command")

(defvar *emacs-eval-timeout* 20 "Emacs evaluation timeout in seconds.")

;; (run-prog *shell-program* :args (list "-c" cmd) :wait nil)

(defun emacs-eval (estring &optional collect-output (seconds *emacs-eval-timeout*))
  (dformat 5 "~a" estring)
  (if (zerop seconds)
      (progn
        (dformat 5 "~a" estring)
        (run-shell-command
         (join-string-list *emacsclient-command*
                           ;; "'" estring "'"
                           ;; (prin1-to-string (prin1-to-string estring))
                           (prin1-to-string estring)
                           ;; (prin1-to-string
                           ;;  (concat "(keyboard-quit)" estring))
                           "| grep -v 'connected to remote'")
         collect-output))

      (handler-case
          (sb-ext:with-timeout seconds            ;so it will not hang to wait
                                        ;for forever.
            (progn
              (dformat 5 "~a" estring)
              (run-shell-command
               (join-string-list *emacsclient-command*
                                 ;; "'" estring "'"
                                 ;; (prin1-to-string (prin1-to-string estring))
                                 (prin1-to-string estring)
                                 ;; (prin1-to-string
                                 ;;  (concat "(keyboard-quit)" estring))
                                 "| grep -v 'connected to remote'")
               collect-output)))
        (sb-ext:timeout ()
          (progn
            (message "Emacs is not responding")
            nil)))))

;; (defun emacs-eval (estring &optional collect-output)
;;   (run-shell-command
;;    (join-string-list *emacsclient-command*
;;                      (prin1-to-string estring)
;;                      "| grep -v 'connected to remote'")
;;    collect-output))

;; (defun emacs-eval (estring &optional collect-output)
;;   (handler-case
;;       (sb-ext:with-timeout 30            ;so it will not hang to wait
;;                                         ;for forever.
;;         (format nil
;;          (join-string-list *emacsclient-command*
;;                            ;; "'" estring "'"
;;                            ;; (prin1-to-string (prin1-to-string estring))
;;                            (prin1-to-string estring)
;;                            ;; (prin1-to-string
;;                            ;;  (concat "(keyboard-quit)" estring))
;;                            "| grep -v 'connected to remote'")
;;          collect-output))
;;     (sb-ext:timeout ()
;;       (progn
;;         (message "Emacs is not responding")
;;         nil))))

;; (defun emacs-eval (estring &optional collect-output)
;;   (format nil
;;    (join-string-list *emacsclient-command*
;;                      (prin1-to-string estring)
;;                      ;; "'" estring "'"
;;                      "| grep -v 'connected to remote'")
;;    collect-output))

;; (handler-case
;;     (message (sb-ext:with-timeout 2
;;                (run-shell-command "sleep 3; echo FDSSAf" t)))
;;   (sb-ext:timeout () (message "asfsdf")))

;; (string-trim '(#\Space #\Tab #\Newline) " sdfdsa \n")
;; (cl-ppcre:regex-replace-all "(\"|a)"  "ds\"afsd"  "\\A\\&")
;; (emacs-eval "(message \"Hi\")" t)
;; (emacs-eval1 "(message \"Hi\")" t)
;; (join-string-list "sfdds" "sadfds" "safds")
;; (cl-ppcre:regex-replace-all "(\"|a)"  "ds\"afsd"  "\\A\\&")

(defun emacs-eval-output-list (estring &optional (seconds *emacs-eval-timeout*))
  (let ((output (emacs-eval estring t seconds)))
    (if (string= "" output) "nil" output)))

(defun emacs-eval-nooutput (estring &optional (seconds *emacs-eval-timeout*))
  (emacs-eval estring nil seconds))


(defun read-from-emacs-eval (estring &optional (seconds *emacs-eval-timeout*))
  (let ((*read-eval* nil))
    ;; see http://groups.google.com/group/comp.lang.lisp/msg/69f60e759b9d7e06?pli=1
    ;; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html
      (read-from-string (emacs-eval-output-list estring seconds))))

(defun make-list-from-emacs-eval (estring &optional (seconds *emacs-eval-timeout*))
  (read-from-emacs-eval estring seconds))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defcommand get-emacs-plans-today () ()
            (emacs-eval "(plan)" nil 0)
            (make-list-from-emacs-eval "(planner-plans-on-today)" 0))

;; (testing
;;  (make-list-from-emacs-eval "(planner-plans-on-today)"))


(defun get-emacs-plan-tasks-element (plan group)
  (make-list-from-emacs-eval
   (join-string-list "(planner-page-get-env" (prin1-to-string plan)  (prin1-to-string group) ")")))

(defun get-emacs-tasks (plan)
  (nreverse
   (make-list-from-emacs-eval
    (join-string-list "(planner-tasks-of-plan-today" (prin1-to-string plan) "'(\"_\" \"o\"))"))))

;; (planner-tasks-of-plan-from-page (planner-today) "MyMIS" '("_" "o"))
;; eval it with eval-last-sexp

(defcommand get-emacs-tasks-element () ()
  (message "~{~%~a~}"
           (get-emacs-plan-tasks-element
            *emacs-planner-current-plan*
            (group-name (current-group)))))


;; test run
;; (make-list-from-emacs-eval "(planner-page-get-env \"GNUEmacs\" \"Myname\")")
;;  (emacs-eval (concat "(planner-page-create-env \"" *emacs-planner-current-plan* "\" \"" (group-name (current-group)) "\")"))
;;  (make-list-from-emacs-eval (concat "(planner-page-get-env \"" *emacs-planner-current-plan* "\" \"" (group-name (current-group)) "\")"))


(defun groups-create-from-list (screen glist)
  (dolist (g glist)
    (add-group screen g :background t)))

(defun groups-number-visible (screen)
  (let ((grnum 0))
    (dolist (g (screen-groups screen))
      (unless (char= (char (group-name g) 0) #\.)
        (setf (group-number g) (incf grnum))))))

(defcommand groups-create-from-tasks (&optional not-calculate) ()
  (if (and *emacs-planner-current-plan* (not not-calculate))
      (defparameter *emacs-planner-tasks* (get-emacs-tasks *emacs-planner-current-plan*)))
  (if *emacs-planner-tasks*
      (groups-create-from-list (current-screen) *emacs-planner-tasks*)))

(defcommand groups-number () ()
            (groups-number-visible (current-screen)))

(defcommand-alias groups-recreate-from-tasks groups-create-from-tasks)
(defcommand-alias groups-renumber groups-number)


(defun get-group-time (current last)
  (if (and (boundp '*emacs-planner-current-plan*)
           *emacs-planner-current-plan*)
      (let ((apps
             (get-emacs-plan-tasks-element
              *emacs-planner-current-plan*
              (group-name (current-group)))))
        (emacs-eval-nooutput (join-string-list "(co-task" (prin1-to-string (group-name last)) ")"))
        (emacs-eval-nooutput (join-string-list "(ci-task" (prin1-to-string (group-name current)) ")"))
        (echo-string
         (current-screen)
         (message "~{~%~a~}" apps))
        (mapc #'(lambda (cmd)
                  (run-wcli-command cmd t)) apps)
        (echo-windows))))

;; (mapc #'run-wcli-command
;;       (make-list-from-emacs-eval
;;           (concat "(planner-page-get-env \""
;;                   *emacs-planner-current-plan*
;;                   "\" \""
;;                   (group-name (current-group))
;;                   "\")")))


;; (add-hook
;;  *run-cli-program-hook*
;;  #'(lambda (cmd)                        ;add command into emacs.
;;      (emacs-eval
;;       (concat "(planner-page-create-env-ele \""
;;               cmd
;;               "\" \""
;;               *emacs-planner-current-plan*
;;               "\" \""
;;               (group-name (current-group))
;;               "\")"))))

(defcommand planner/create-note-from-tast () ()
  (run-or-pull
   (concat "emacsclient -d " (getenv "DISPLAY") " -c ")
   '(:class "Emacs"))
  (emacs-eval
   (join-string-list
    "(stumpwm/planner-create-note-from-task"
    (prin1-to-string (group-name (current-group)))
    ")")))

(add-hook *focus-group-hook* 'get-group-time)

;; (defcommand select-plan-task () ()
;;   (labels ((pick (options)
;;              (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
;;                (cond
;;                  ((null selection)
;;                   (throw 'stumpwm::error "Abort."))
;;                  ((stringp (second selection))
;;                   (second selection))
;;                  (t
;;                   (pick (task-of-plan selection)))))))
;;     (let ((choice (pick *today-plans*)))
;;       (run-shell-command choice))))

(defun find-groups (grnames)
  (remove-if
   #'null
   (mapcar
    #'(lambda (g)
       (find-group (current-screen) g))
    grnames)))


(defun windows-from-groups (groups)
  (loop for gr in groups append (group-windows gr)))

(defun move-windows-to-tmpgroup (windows)
  (if windows
      (let ((hold-group (add-group (current-screen) ".hold" :background t)))
        (dolist (win windows)
                (move-window-to-group win hold-group)))))

(defun kill-groups (groups to-group)
  (dolist (g groups)
    (kill-group g to-group)))

(defcommand grouplist-to (&optional (fmt *group-format*)) (:rest)
  "Allow the user to select a group from a list, like windowlist but
  for groups"
  (let ((group (second (menu-with-timeout
                        (mapcar (lambda (g)
                                  (list (format-expand *group-formatters* fmt g) g))
                                (screen-groups (current-screen)))))))
    (when group
      (switch-to-group group))))


(defcommand select-plan-task () ()
  (let* ((*message-window-gravity* :center)
         (selection
          (menu-with-timeout (get-emacs-plans-today) :prompt "Which Plan u want to work ?")))
          ;; (select-from-menu (current-screen) (get-emacs-plans-today) "Which Plan u want to work ?")))
    (if (null selection)
        (throw 'stumpwm::error "Abort.")
        (progn
          (unless (and (boundp '*emacs-planner-current-plan*)
                       (string= *emacs-planner-current-plan* selection))
            (setf *emacs-planner-current-plan* selection)
            ;; (move-windows-to-tmpgroup
            ;;  (windows-from-groups
            ;;   (find-groups (get-emacs-tasks *emacs-planner-current-plan*))))
            (kill-groups
             (find-groups (get-emacs-tasks *emacs-planner-current-plan*))
             (add-group (current-screen) ".hold" :background t))
            (setf *emacs-planner-current-plan* selection)
            (dolist (task (get-emacs-tasks  selection))
                    (add-group (current-screen) task :background t)))
          (grouplist-to)))))




(defun group-kill (dead-group)
"Kill the current group. All windows in the current group are migrated
to the next group."
  (let* ((groups (screen-groups (current-screen)))
         ;; If no "visible" group is found, try with all groups
         (to-group (or (next-group dead-group (non-hidden-groups groups))
                       (next-group dead-group groups))))
    (if to-group
        (if (or (not %interactivep%)
            (not (group-windows dead-group))
            (y-or-n-p
             (format nil "You are about to kill non-empty group \"^B^3*~a^n\"
The windows will be moved to group \"^B^2*~a^n\"
^B^6*Confirm?^n " (group-name dead-group) (group-name to-group))))
            (progn
              (switch-to-group to-group)
              (kill-group dead-group to-group)
              (message "Deleted"))
            (message "Canceled"))
        (message "There's only one group left"))))

(defcommand gkill-grouplist (&optional (fmt *group-format*)) (:rest)
  "Allow the user to select a group from a list, like windowlist but
  for groups"
  (let ((group (second (select-from-menu
		(current-screen)
		(mapcar (lambda (g)
			  (list (format-expand *group-formatters* fmt g) g))
			(screen-groups (current-screen)))))))
    (when group
      (group-kill group))))
