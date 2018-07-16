

(require '@)

(defvar @act-base
  (@extend
   :name "act base."
   :finilize-args ()))

(def@ @act-base :keyp (key)
      (memq key (@:keys)))

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

(progn
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
             fmt args))))

;; (@! (@! @dest-class :gen-warning ) :receive "Hello")
;; (@! (@! @dest-class :gen-msg ) :receive "Hello")


(progn
 (setf @note-class
       (@extend @act-base
                :name "note class"))
 (setf (@ @note-class :dests) '())

 (def@ @note-class :send (fmt args)
       (if (and (memq :dests (@:keys))
                (consp @:dests))
           (dolist (dest @:dests)
             (if dest
                 (if (@! dest :keyp :receive)
                     (@! dest :receive fmt args)
                   (message
                    "dest %s [%s] not has :receive method, not sending msg."
                    (@ dest :name)
                    (@! dest :keys)))
               (message "dest is nil")))
         (error "No @:dests %d boundp(%s) consp(%s) present."
                (length @:dests)
                (boundp '@:dests)
                (consp @:dests))))

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
   ))


(when nil

  (@! (@! @note-class :gen-format-msg "message") :send "Test %d" 00)

  (length (@ (@! @note-class :gen-format-msg "message") :dests))

  (@ @note-class :gen-format-msg)



  ;; (@! (@! @dest-class :gen-warning "warning") :receive "Hello")
  ;; (@! (@! @dest-class :gen-msg "msg") :receive "Hello")
  )
