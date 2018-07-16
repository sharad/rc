

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
           :name "note class"))

(def@ @note-class :send (fmt args)
      (if @:dests
          (dolist (dest @:dests)
            (if dest
                (@! dest :receive fmt args)
              (message "dest is nil, not sending msg.")))
        (error "No @:destinations present.")))

(defsubclass-gen@ @note-class :gen-format-msg ()
  (push
   (@! @dest-class :gen-msg "msg")
   @:dests)
  (def@ @@ :send (fmt &rest args)
    (apply '@:dest fmt args)))

(defsubclass-gen@ @note-class :gen-org-log-note ()
  (push
   (@! @dest-class :gen-msg)
   @:dests)
  (def@ @@ :send (marker fmt &rest args)
    (apply '@:dest fmt args)))

(defsubclass-gen@ @note-class :gen-org-dual-log-note ()
  (push
   (@! @dest-class :gen-msg)
   @:dests)
  (def@ @@ :send (fmt &rest args)
    (apply '@:dest fmt args)))

(defsubclass-gen@ @note-class :gen-org-intreactive-log-note ()
  (push
   (@! @dest-class :gen-msg)
   @:dests)
  (def@ @@ :send (fmt &rest args)
    (apply '@:dest fmt args)))

(@! @note-class :gen-format-msg "message")

(@ @note-class :gen-format-msg)

;; (@! (@! @dest-class :gen-warning "warning") :receive "Hello")
;; (@! (@! @dest-class :gen-msg ) :receive "Hello")
