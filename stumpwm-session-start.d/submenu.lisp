
(in-package :stumpwm)

(defparameter *app-menu*
  '(("INTERNET"
     ;; sub menu
     ("Firefox" "firefox")
     ("Skype" "skype"))
    ("FUN"
     ;; sub menu
     ("option 2" "xlogo")
     ("GnuChess" "xboard"))
    ("WORK"
     ;;submenu
     ("OpenOffice.org" "openoffice"))
    ("GRAPHICS"
     ;;submenu
     ("GIMP" "gimp"))
    ("K3B" "k3b")))


(defcommand mymenu () ()
  (labels ((pick (options)
             (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 ((stringp (second selection))
                  (second selection))
                 (t
                  (pick (cdr selection)))))))
    (let ((choice (pick *app-menu*)))
      (run-shell-command choice))))


(defvar *menu-selection-timeout* 2 "menu selection timeout")
(defvar *menu-selection-file* #p"/tmp/sel" "menu selection timeout")

(defun save-db (filename db)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print db out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(let (db)
  (defun get-sel (options &optional (storage *menu-selection-file*))
    (unless db
      (setf db (load-db storage)))
    (assoc options db :test #'equal))

  (defun set-sel (selection options &optional (storage *menu-selection-file*))
    (unless db
      (setf db (load-db storage)))
    (if (acons options selection db :test #'equal)
        (save-db storage db))))

(defun timed-selection (options &optional (seconds *menu-selection-timeout*))
  (handler-case
      (sb-ext:with-timeout seconds            ;so it will not hang to wait for forever.
        (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
          selection))
        (sb-ext:timeout ()
          nil)))

(defun menu-with-timeout (options &optional (seconds *menu-selection-timeout*) storage default)
  (let ((automatic-selection
         (or (get-sel options storage)
             (if default
                 (if (numberp default)
                     (cond
                       ((and (< default (length options))
                             (> default  0))
                        (nth default options))
                       ((>= default (length options))
                        (last options))
                       (t (car options)))
                     (if (member default options :test #'equal)
                         default))
                 (car options))))
        (selection (timed-selection options seconds)))
    (if selection
        (progn
         (unless (equal selection automatic-selection)
           (set-sel selection options storage))
         selection)
        automatic-selection)))


;; (menu-with-timeout '("x" "y" "z") 2 #p"/tmp/x")
