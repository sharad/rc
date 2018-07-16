

(require '@)

(defvar @act-base
  (@extend
   :name "act base."
   :finilize-args ()))

(def@ @act-base :finalize ()
      ())

(defmacro defsubclass-gen@ (object gen-method params &rest body )
  `(progn
     (def@ ,object ,gen-method (name ,@params)

           (let ((drived-obj
                  (@extend ,object
                           :name (concat (@ ,object :name) name))))

             (with-@@ drived-obj
                 ,@body)

             drived-obj))))
(put 'defsubclass-gen@ 'lisp-indent-function 3)

(setf @activity-class
  (@extend @act-base
           :name "activity class"))

(setf @event-class
  (@extend @act-base
           :name "event class"))

(setf @transition-class
  (@extend @act-base
           :name "transition class"))

(setf @dest-class
  (@extend @act-base
           :name "dest class"))

(defsubclass-gen@ @dest-class :gen-builder ()
  (def@ @@ :receive (fmt &rest args)
    (apply #'format
           fmt args)))

(defsubclass-gen@ @dest-class :gen-msg ()
  (def@ @@ :receive (fmt &rest args)
        (apply #'message
               fmt args)))

(defsubclass-gen@ @dest-class :gen-warning ()
  (def@ @@ :receive (fmt &rest args)
        (apply #'lwarn
               'activity
               'warning
               fmt args)))

(defsubclass-gen@ @dest-class :gen-error ()
                  (def@ @@ :receive (fmt &rest args)
                    (apply #'lwarn
                           'activity
                           'error
                           fmt args)))


;; (@! (@! @dest-class :gen-warning ) :receive "Hello")
;; (@! (@! @dest-class :gen-msg ) :receive "Hello")


(setf @note-class
  (@extend @act-base
           :name "note class"
           :dests '()))

(def@ @note-class :send (fmt args)
      (if (and (boundp '@:dests)
               (consp @:dests))
          (dolist (dest @:dests)
            (if dest
                (@! dest :receive fmt args)
              (message "dest is nil, not sending msg.")))
        (error "No @:destinations present.")))

(defsubclass-gen@ @note-class :gen-format-msg ()
  (push
   (@! @dest-class :gen-msg "msg")
   @:dests)
  )

(defsubclass-gen@ @note-class :gen-org-log-note ()
  (push
   (@! @dest-class :gen-msg "msg")
   @:dests)
  )

(defsubclass-gen@ @note-class :gen-org-dual-log-note ()
  (push
   (@! @dest-class :gen-msg "msg")
   @:dests)
  )

(defsubclass-gen@ @note-class :gen-org-intreactive-log-note ()
  (push
   (@! @dest-class :gen-msg "msg")
   @:dests)
  )

(@! (@! @note-class :gen-format-msg "message") :send "Test %d" 00)

(@ @note-class :gen-format-msg)




(macroexpand-all
 '(defsubclass-gen@ @note-class :gen-format-msg ()
   (push
    (@! @dest-class :gen-msg "msg")
    @:dests)
   ))



(progn
  (progn
    (let* ((v @note-class))
      (@--set v :gen-format-msg
              (function
               (lambda (@@ name)
                (let ((@@@ @note-class))
                  (let ((@@ @@))
                    (let ((drived-obj (@extend @note-class :name (concat (@ @note-class :name) name))))
                      (let ((@@ drived-obj))
                        (let* ((v1 (@! @dest-class :gen-msg "msg"))
                               (v @@))
                          (@--set v :dests (cons v1 (@ v :dests)))))
                      drived-obj)))))))
    :gen-format-msg))





(defun @--walk (sexp skip replace &optional head)
  "Replace all symbols by calling REPLACE on them."
  (macrolet ((wrap (exp)
               (let ((xv (gensym)))
                 `(let ((,xv ,exp)) (if head (list ,xv) ,xv)))))
    (cond
      ((symbolp sexp) (funcall replace sexp head))
      ((atom sexp) (wrap sexp))
      ((member (first sexp) skip) (wrap sexp))
      ((wrap
        (append (@--walk (first sexp) skip replace t)
                (loop for element in (cdr sexp)
                   collect (@--walk element skip replace nil))))))))


;; (@! (@! @dest-class :gen-warning "warning") :receive "Hello")
;; (@! (@! @dest-class :gen-msg "msg") :receive "Hello")
