

(require '@)

(defvar @act-base
  (@extend
   :name "act base."
   :finilize-args ()))

(def@ @act-base :finalize ()
      ())

(defmacro defsubclass-gen@ (object method params &rest body )
  `(progn
     (def@ ,object ,method (name ,@params)

           (let ((drived-obj
                  (@extend ,object
                           :name (concat (@ ,object :name) name))))

             (with-@@ drived-obj
                 ,@body)

             drived-obj))))
(put 'defsubclass-gen@ 'lisp-indent-function 3)

(defvar @activity-class
  (@extend :name "activity class"))

(defvar @event-class
  (@extend :name "event class"))

(defvar @transition-class
  (@extend @activity
           :name "transition class"))

(defvar @dest-class
  (@extend :name "dest class"))

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


(defvar @note-class
  (@extend :name "note class"))

(def@ @note-class :send (fmt args)
      (if @:dests
          (dolist (dest @:dests)
            (if dest
                (@! dest :receive fmt args)
              (message "dest is nil, not sending msg.")))
        (error "No @:destinations present.")))

(defsubclass-gen@ @note-class :gen-format-msg ()
  (push
   (@! @dest-class :gen-msg)
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
