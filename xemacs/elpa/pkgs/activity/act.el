

(require '@)

(provide 'act)

(defvar @act-base
  (@extend
   :name "act base."
   :finilize-args ()))

(def@ @act-base :keyp (key)
      (memq key (@:keys)))

(def@ @act-base :finalize ()
      ())

(defmacro fun-param (&rest params)
  `(remove-if
    #'(lambda (p) (member p '(&optional &rest)))
    ',params))

(defmacro fun-apply-param (params)
  `(remove-if
    #'(lambda (p) (member p '(&optional &rest)))
    ',params))

(fun-apply-param (&rest x y))

(defmacro defsubobj@ (object name params &rest body)
  `(let ((drived-obj
          (@extend ,object
                   :name (concat (@ ,object :name) " > " ,name))))

     (with-@@ drived-obj
       ,@(if (stringp (car body))
            `((setf @:doc ,(car body))))
       ,@(if (stringp (car body)) (cdr body) body)

       (if (@:keyp :dispatch)
           (if t ;; (fboundp @:dispatch)
               (apply @:dispatch ,`(fun-apply-param ,params))
             (message "%s: no :dispatch function defined."
                      @:name))
         (message "%s: no :dispatch prop defined."
                  @:name)))

     drived-obj))
(put 'defsubobj@ 'lisp-indent-function 3)

(macroexpand-1
 '(defsubobj@ @dest-class name (x &optional y)
   (def@ @@ :receive (fmt &rest args)
     (apply (function message) fmt args))))



(defmacro defsubclass-gen@ (object gen-method params &rest body )
  `(progn
     (def@ ,object ,gen-method (name ,@params)
           ,@(if (stringp (car body)) (list (car body)) ())
           (defsubobj@ ,object name ,params
             ,@(if (stringp (car body)) (cdr body) body)))))
(put 'defsubclass-gen@ 'lisp-indent-function 3)

(progn
  ;; destination
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

  (when nil
    (macroexpand-1
     (macroexpand-1
      '(defsubclass-gen@ @dest-class :gen-msg (x &optional y)
        (def@ @@ :receive (fmt &rest args)
          (apply #'message
                 fmt args)))))


    (progn
      (def@ @dest-class :gen-msg (name x &optional y)
            (let ((drived-obj (@extend @dest-class :name (concat (@ @dest-class :name) " > " name))))
              (with-@@ drived-obj
                  (def@ @@ :receive (fmt &rest args)
                    (apply (function message) fmt args))
                (if (@:keyp :dispatch)
                    (if t
                        (@:dispatch x &optional y)
                      (message "%s: no :dispatch function defined." @:name))
                  (message "%s: no :dispatch prop defined." @:name)))
              drived-obj)))



    (progn (def@ @dest-class :gen-msg (name x) (defsubobj@ @dest-class name (x) (def@ @@ :receive (fmt &rest args) (apply (function message) fmt args)))))
    (progn (def@ @dest-class :gen-msg (name) (defsubobj@ @dest-class name () (def@ @@ :receive (fmt &rest args) (apply (function message) fmt args)))))



    (macroexpand-1
     '(defsubobj@ @dest-class name (x &optional y)
       (def@ @@ :receive (fmt &rest args)
         (apply (function message) fmt args))))





    (@ (@! @dest-class :gen-msg "msg") :receive)
    )

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

(progn
  ;; note
  (setf @note-class
        (@extend @act-base
                 :name "note class"))
  (setf (@ @note-class :dests) '())

  (def@ @note-class :send (fmt &rest args)
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
    "Generator for format message note"
    (push
     (@! @dest-class :gen-msg "msg")
     @:dests)
    )

  (defsubclass-gen@ @note-class :gen-org-log-note ()
    "Generator for org log note"
    (push
     (@! @dest-class :gen-msg "msg")
     @:dests)
    )

  (defsubclass-gen@ @note-class :gen-org-dual-log-note ()
    "Generator for dual org log note"
    (push
     (@! @dest-class :gen-msg "msg")
     @:dests)
    )

  (defsubclass-gen@ @note-class :gen-org-intreactive-log-note ()
    "Generator for Interactive org log note"
    (push
     (@! @dest-class :gen-msg "msg")
     @:dests)
    ))

(progn
  ;; activity
  (setf @activity-class
        (defsubobj@ @act-base "activity class" ()
          "Activity class"
          (def@ @@ :init ()
            (@^:init)
            (setf @:occuredon (current-time)))))

  (setf @event-class
        (defsubobj@ @activity-class "event class" ()
          "Event class"
          (def@ @@ :note ()
            )))

  (setf @transition-class
        (defsubobj@ @event-class "transition class" ()
          "Transition class"
          (def@ @@ :note ()
            ))))



(progn
  ;; detectors
  (setf @activity-dectector-class
        (defsubobj@ @act-base "activity detector class" ()
          "Activity detector class"
          (def@ @@ :note ()
            )))

  (setf @event-dectector-class
        (defsubobj@ @activity-dectector-class "event detector class" ()
          "Event detector class"
          (def@ @@ :note ()
            )))

  (setf @transition-dectector-class
        (defsubobj@ @event-dectector-class "transition detector class" ()
          "Transition detector class"
          (def@ @@ :note ()
            ))))
























(defsubclass-gen@ @transition-dectector-class :gen-buffer-trans (&optional note)

  (def@ @@ :make-event ()
    "Make buffer change event."
    (let ((curr (current-buffer)))
      (message "running :make-event")
      (unless (eql
               @:prev
               curr)
        (@! @:buff-tran :send @:prev curr)
        (setf @:prev curr))))




  (def@ @@ :dispatch (&optional xnote)
    ;; (@^:dispatch)

    (setf @:prev (current-buffer))

    (setf @:buff-tran
          (defsubobj@ @transition-class "buffer transition" (&optional xnote)

            (setf @:occuredon "x")
            (def@ @@ :send (prev next)
              (@! @:note :send "switched buffer %s to %s on %s"
                  prev next @:occuredon))

            (def@ @@ :dispatch (&optional note)
              (@^:init)
              (setf @:note
                    (@! @note-class :gen-format-msg "test")
                    ;; (or note
                    ;;     (@! @note-class :gen-format-msg "test"))
                    ))))
    ;; (@:install)
    ))

(setf @buff-transition-detector
      (@! @transition-dectector-class :gen-buffer-trans "test" nil))

(defun make-event ()
  (message "running make-event")
  (@! @buff-transition-detector :make-event))

(defun buff-transition-detector-install ()
  (add-hook 'buffer-list-update-hook     'make-event)
  (add-hook 'elscreen-screen-update-hook 'make-event)
  (add-hook 'elscreen-goto-hook          'make-event))

(defun buff-transition-detector-uninstall ()
  (remove-hook 'buffer-list-update-hook     'make-event)
  (remove-hook 'elscreen-screen-update-hook 'make-event)
  (remove-hook 'elscreen-goto-hook          'make-event))



(when nil
  (buff-transition-detector-install)

  (buff-transition-detector-uninstall)

  (@! @buff-transition-detector :make-event)

  (@ @buff-transition-detector :prev)


  (@ (@! @dest-class :gen-msg "msg") :receive)


  (@ (@! @note-class :gen-format-msg "test") :dests)
  )












(when nil

  (@! (@! @note-class :gen-format-msg "message") :send "Test %d" 00)

  (@ (car (@ (@! @note-class :gen-format-msg "message") :dests)) :name)

  (@ @note-class :gen-format-msg)


  (defsubclass-gen@ @note-class :gen-few-msg ()
    (push
     (@! @dest-class :gen-msg "msg")
     @:dests)
    (push
     (@! @dest-class :gen-warning "warning")
     @:dests)
    )

  (@! (@! @note-class :gen-few-msg "message") :send "Test %d" 00)

  (@! (car (@ (@! @note-class :gen-few-msg "message") :dests)) :receive "TEst")

  ;; (@! (@! @dest-class :gen-warning "warning") :receive "Hello")
  ;; (@! (@! @dest-class :gen-msg "msg") :receive "Hello")
  )


(macroexpand-all
 '(defsubclass-gen@ @note-class :gen-org-intreactive-log-note ()
   (push
    (@! @dest-class :gen-msg "msg")
    @:dests)
   ))

(macroexpand-1
 '(defsubclass-gen@ @note-class :gen-org-intreactive-log-note (hello)
   (push
    (@! @dest-class :gen-msg "msg")
    @:dests)
   ))

;; (progn (def@ @note-class :gen-org-intreactive-log-note (name hello) (defsubobj@ @note-class name (hello) (push (@! @dest-class :gen-msg "msg") @:dests))))
