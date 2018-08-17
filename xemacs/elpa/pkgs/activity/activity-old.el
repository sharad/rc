



(setf @buffer-transition-singleton
      (@extend @transition-singleton))

(def@ @buffer-transition-singleton :init (new-buffer)
      (@^:init new-buffer)
      (setf @:note
            (@! @activity-note :new
                (list
                 @message-note-destination
                 @org-heading-note-destination))))

(def@ @buffer-transition-singleton :message ()
      (format "changed from %s buffer to %s buffer on %s"
              (if  @:old (buffer-name @:old) "none")
              (buffer-name @:new)
              (@:occuredon)))

(def@ @buffer-transition-singleton :object-sexp ()
      (list
       'buffer-transition
       :old (if @:old (buffer-name @:old) nil)
       :new (buffer-name @:new)
       (list
        'activity
        occuredon (@:occuredon))))

(def@ @buffer-transition-singleton :execute ()
      (if (equal @:old (current-buffer))
          (message "not dispatching")
        (progn
          (setf
           @:old @:new
           @:new (current-buffer))
          (@:dispatch))))

(defun buffer-transition-singleton-execute ()
  (@! @buffer-transition-singleton :execute))



(defvar @mail-event
  (@extend @event :name "mail event"))


(defvar @send-mail-event
  (@extend @mail-event :name "send mail event"))

(def@ @send-mail-event :init (to subject)
      (@^:init)
      (setf
       @:to     to
       @subject subject))

(def@ @send-mail-event :message ()
      (format "sending mail to %s with subject %s on %s"
              @:to
              @:subject
              (@:occuredon)))

(def@ @send-mail-event :make-event ()
      (@:notify))




(defvar @read-mail-event
  (@extend @mail-event :name "read mail event"))

(def@ @read-mail-event :init (to subject)
      (@^:init)
      (setf
       @:to     to
       @subject subject))

(def@ @read-mail-event :message ()
      (format "reading mail from %s with subject %s on %s"
              @:to
              @:subject
              (@:occuredon)))

(def@ @send-mail-event :make-event ()
      )

(defvar @clock-transition-singleton
  (@extend @transition @dispatchable-immediate
           :clock-marker nil
           :heading nil))

(def@ @clock-transition-singleton :init (old-marker newnews-marker)
      (@^:init old-marker newnews-marker))

(def@ @clock-out-activity :message ()
      (if @:next-clock
          (format
           "clocking out from [%s] to clocking in to [%s]"
           @:heading
           (@! @:next-clock :headign))
        (format
         "clocking out from [%s]"
         @:heading)))

(def@ @clock-out-activity :init ()
      (message "test1"))

(setf test (@! @clock-out-activity :new))

(@! @clock-out-activity :message)

































(when nil

  (setf @baseobj (@extend))

  (def@ @baseobj :initialize ()
        )

  (setf @obj1 (@extend @baseobj :name "obj1"))
  (def@ @obj1 :init ()
        (message "Hello from obj1")
        (@^:init))

  (setf @obj2 (@extend @baseobj :name "obj2"))
  (def@ @obj2 :init ()
        (message "Hello from obj2")
        (@^:init))

  (setf @drivedobj1 (@extend @obj1 @obj2))
  (def@ @drivedobj1 :init ()
        (message "Hello from drivedobj1")
        (@^:init))


  (setf instdrivedobj1 (@! @drivedobj1 :new))

  (length  (@ instdrivedobj1 :proto))

  (length (@ @drivedobj1 :proto))

  )




(when nil

  (defvar @lister
    (@extend :name "lister"))

  (setf (@ @lister :list) '(a b))

  (def@ @lister :init ()
        (@^:init))

  (def@ @lister :print ()
        (message "list: %s" @:list))

  (def@ @lister :add (el)
        (push el @:list))

  (def@ @lister :del (el)
        (setf @:list (remove el @:list)))

  (defvar @lister1 (@! @lister :new '(c d)))

  (setf @lister1 (@! @lister :new))

  (@! @lister :print)

  (@! @lister1 :print)

  (@! @lister :add 'z)

  (@! @lister1 :add 'x)

  (@! @lister1 :del 'x)

  )


(when nil
  (defvar @dispatchable
    (@extend @activity :name "Class Dispatchable"
             :dispatchers nil))

  (def@ @dispatchable :add-dispatcher (callback)
        (push callback @:dispatchers))

  (def@ @watchable :remove-dipatcher (callback)
        (setf @:dispatchers (remove callback @:dispatchers)))

  (defmacro def-dispatcher@ (object name params &rest body)
    `(progn
       (def@ ,object ,name params
             ,@body)
       (@! ,object
           :add-dispatcher
           (@ ,object ,name))))
  (put 'def-dispatcher@ 'lisp-indent-function 3)

  (defmacro undef-dispatcher@ (object name)
    `(progn
       (@! ,object
           :remove-dispatcher
           (@ ,object ,name))))
  (put 'undef-dispatcher@ 'lisp-indent-function 1)

  (def-dispatcher@ @dispatchable :log-in-message ()
    (message (@:message)))

  (def-dispatcher@ @dispatchable :log-to-clock ()
    (when (marker-buffer org-clock-marker)
      (org-insert-log-note
       org-clock-hd-marker
       (@:message)
       'note)))

  (def@ @dispatchable :init ()
        (@^:init))

  ;; (setf (@ @dispatchable :dispatchers) nil)

  
  (defvar @dispatchable-immediate
    (@extend @dispatchable :name "Class Deferred Dispatchable"))

  (def@ @dispatchable-immediate :dispatch ()
        (message "calling @dispatchable-immediate :dispatch")
        (dolist (cb @:dispatchers)
          ;; (message "calling %s" cb)
          (funcall cb @@)))
  
  (defvar @dispatchable-defferred
    (@extend @dispatchable :name "Class Deferred Dispatchable"))

  (def@ @dispatchable-defferred :dispatch (sec)
        (run-with-idle-plus-timer sec nil
                                  (lambda ()
                                    (dolist (cb @:dispatchers)
                                      (funcall cb @@)))))

  )



(progn
  ;; error
  
  ;; e.g.
  (defvar @immutable (@extend))

  (def@ @immutable :set (property _value)
        (error "Object is immutable, cannot set %s" property))

  (def@ @ :freeze ()
    "Make this object immutable."
    (push @immutable @:proto))
  
  ;; e.g.
  (defvar @watchable (@extend :watchers nil))

  (def@ @watchable :watch (callback)
        (push callback @:watchers))

  (def@ @watchable :unwatch (callback)
        (setf @:watchers (remove callback @:watchers)))

  (def@ @watchable :set (property new)
        (dolist (callback @:watchers)
          (funcall callback @@ property new))
        (@^:set property new))
  
  ;; e.g.
  (defvar @rectangle (@extend :name "Class Rectangle"))
  (def@ @rectangle :init (width height)
        (@^:init)
        (setf @:width width @:height height))

  ;; (@! (@! @rectangle :new 13.2 2.1) :area)
  
 )
