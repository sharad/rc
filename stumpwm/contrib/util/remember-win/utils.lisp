

(in-package #:remember-win)


(export '(*menu-selection-file* *menu-selection-file* *show-dbg-enable*))

(defvar *show-dbg-enable* nil "Enable show-dbg")

(defun string-to-utf8 (string)
  "Convert the string to a vector of octets."
  #+ccl (ccl:encode-string-to-octets string :external-format :utf-8)
  #+clisp (ext:convert-string-to-bytes string charset:utf-8)
  #+sbcl (sb-ext:string-to-octets
          string
          :external-format :utf-8)
  #+lispworks
  (ef:encode-lisp-string string :utf-8)
  #-(or ccl clisp sbcl lispworks)
  (map 'list #'char-code string))


;; utilities.
(defun compact (list)
  (remove-if #'null list))

;; Debugging utilities.
(defun show-dbg (msg &key (prompt "test:"))
  (if *show-dbg-enable*
      (read-one-line (current-screen) prompt :initial-input (or msg "NOTHING"))))


;; Basic operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun string-ignore= (char string1 string2)
  (apply #'string= (mapcar #'(lambda (s)
                               (remove char s))
                           (list string1 string2))))

(defun string-ignore-space= (s1 s2)
  (string-ignore= #\Space s1 s2))

(defun space-stripped (s)
  (remove #\Space s))


;; (defun select-by-menu (list &optional (property 'pa::name))
;;     (let* ((table (mapcar #'(lambda (obj)
;;                              (cons (property-value obj property) obj))
;;                          list))
;;           (retval
;;            (menu-with-timeout table :prompt "Select: ")))
;;            ;; (stumpwm::select-from-menu (current-screen) table "Select: ")))
;;       (if (consp retval)
;;           (cdr retval)
;;           retval)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defvar *menu-selection-timeout* 7 "menu selection timeout")
;; (defvar *menu-selection-file* (stumpwm::data-dir-file "selections" "dump") "menu selection timeout")

(defvar *menu-selection-file* #p"/tmp/selections" "menu selection timeout")


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
  (if (if (stumpwm::probe-path filename)
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
    (save-db storage db)
    selection)

  (defun save-storage (&optional (storage *menu-selection-file*))
    (save-db storage db)))

(defun timed-selection (options seconds prompt &key (else nil))
  (handler-case
      (sb-ext:with-timeout seconds            ;so it will not hang to wait for forever.
        (let ((selection (stumpwm::select-from-menu (current-screen) options prompt)))
          selection))
    (sb-ext:timeout ()
      (if (functionp else)
          (funcall else)
          else))))

(defun nth-max (index seq)
  (if (< (length seq) index)
      (car (last seq))
      (nth index seq)))


(defun menu-with-timeout (options &key
                          (default 0)
                          (storage *menu-selection-file*)
                          (seconds *menu-selection-timeout*)
                          (prompt "sel:"))
  (let ((selection
         (timed-selection options seconds prompt
                          :else #'(lambda ()
                                    (get-sel options storage)
                                    (if default
                                        (if (numberp default)
                                            (nth-max default options)
                                            (find default options :test #'equal :key #'(lambda (e) (if (consp e) (car e) e))))
                                        (car options))))))
    (show-dbg (format nil "menu-with-timeout: selection ~a" selection))
    (show-dbg "in menu-with-timeout")
    (set-sel selection options storage)))


;; (menu-with-timeout '("x" "y" "z") :seconds 2 :storage #p"/tmp/x")
;; (menu-with-timeout '(("x") ("y") ("z")) :storage #p"/tmp/x")
;; (menu-with-timeout *app-menu* :storage #p"/tmp/x" :prompt "asfdsf:")

