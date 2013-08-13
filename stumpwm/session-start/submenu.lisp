
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




(defvar *menu-selection-timeout* 7 "menu selection timeout")
(defvar *menu-selection-file* (data-dir-file "selections" "dump") "menu selection timeout")


(defun strip-cdr (list)
  (mapcar #'(lambda (e)
              (if (consp e)
                  (car e)
                  e)) list))

(defun save-db (filename db)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print db out))))

(defun load-db (filename)
  (if (if (probe-path filename)
          t
          (save-db filename nil))
      (with-open-file (in filename)
        (with-standard-io-syntax
          (read in)))))

(let (db)
  (defun db ()
    db)

  (defun reset-db ()
    (setf db nil))

  (defun get-sel (options storage)
    (unless db
      (setf db (load-db storage)))
    (cdr (assoc (strip-cdr options) db :test #'equal)))

  (defun set-sel (selection options storage)
    (setf db (acons (strip-cdr options)
                    (if (consp selection) (car selection) selection) db))
    (show-dbg "in set-sel")
    (save-db storage db))

  (defun save-storage (&optional (storage *menu-selection-file*))
    (save-db storage db)))

(defun timed-selection (options seconds prompt)
  (handler-case
      (sb-ext:with-timeout seconds            ;so it will not hang to wait for forever.
        (let ((selection (stumpwm::select-from-menu (current-screen) options prompt)))
          selection))
        (sb-ext:timeout ()
          nil)))

(defun menu-with-timeout (options &key
                          (default 0)
                          (storage *menu-selection-file*)
                          (seconds *menu-selection-timeout*)
                          (prompt "sel:"))
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
        (selection (timed-selection options seconds prompt)))
    (if selection
        (progn
          ;; (unless (equal selection automatic-selection)
          (show-dbg "in menu-with-timeout")
          (set-sel selection options storage)
          selection)
        (car (member automatic-selection options
                     :key #'(lambda (e) (if (consp e) (car e) e)) :test #'equal)))))


;; (menu-with-timeout '("x" "y" "z") :seconds 2 :storage #p"/tmp/x")
;; (menu-with-timeout '(("x") ("y") ("z")) :storage #p"/tmp/x")
;; (menu-with-timeout *app-menu* :storage #p"/tmp/x" :prompt "asfdsf:")

