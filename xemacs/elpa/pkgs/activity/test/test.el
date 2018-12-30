(require '@)


(setf @test
      (@extend :name "test"))

(setf (@ @test :_node)
      (@extend @test :name "node"
               :slot 'a))

(def@ @test :node ()
      @:_node)




(defmacro def@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (setf (@ ,object ,method)
           (function* (lambda ,(cons '@@ params)
             ,@(if (stringp (car body)) (list (car body)) ())
             (let ((@@@ ,object))
               (with-@@ @@
                   ,@(if (stringp (car body)) (cdr body) body))))))
     ,method))

(def@ @activity :add-def (method fn-body)
      (eval
       `(progn
          (setf (@ @@ ,method)
                (function* ,fn-body)))))

(defmacro defun@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (@! ,object :add-def ',method
         ',`(lambda ,(cons '@@ params)
              ,@(if (stringp (car body)) (list (car body)) ())
              (with-@@ @@
                    ,@(if (stringp (car body)) (cdr body) body)
                  )))))





(defun@ @activity :init-y ()
  (unless (boundp '@:y)
    (@:fn))
  @:y)

(defun@ @activity :note ()
        (unless (boundp '@:_note)
          (@:init-note))
        @:_note)

(@! @activity :note)

(macroexpand-all
 '(defun@ @test :init-x ()
   "test"
   (message "test%s" @:x)
   (message "test %s" @:slot)))

(progn (@! @test :add-def (quote :init-x) (quote (lambda (@@) "test" (with-@@ @@ (message "test%s" 2) (message "test %s" 1))))))

(progn (@! @test :add-def :init-x (quote (lambda (@@) "test" "test" (message "test%s" (@:x)) (message "test %s" @:slot)))))

(setf (@ @test :x) 1)
(setf (@ @test :slot) 2)

(@! @test :init-x)



(with-@@ @test
    (message "test%s" (@:node))
  (message "test %s" @:_node))



(progn (@! @test :add-def :init-x () '(lambda (@@) (let ((@@ @@)) (message "test%s" (@! @@ :x)) (message "test %s" (@ @@ :slot))))))



(@! @test :add-def :init-z '()
    '9)

(defmacro test2 (x)
  `(with-@@ '@@ ,x ))

(defmacro test1 (x)
  `(,x))

(macroexpand-all
 '(test2 z))


(macroexpand-1
 '(with-@@ @@
   (message "test" (@:x))))




(@ @test :init-x)

(def@ @test :init-y ()
      10)





(@ @test :init-x)


(defun@ @test :init-x ()
        (message "test%s" (@:x))
        (message "test %s" @:slot))

(defun@ @activity :note ()
        (unless (boundp '@:_note)
          (@:init-note))
        @:_note)

(@! @test :init-x)

(defun@ (@! @test :node) :init-x ()
        (message "test x"))

(@! (@! @test :node) :init-x)

(defun@ (@ @test :_node) :init-z ()
        (message "test %s" @:slot))

(defun@ (@! @test :node) :init-alpha ()
        (message "test %s" @:slot))


(let ((note (@! @activity :note)))
 (def@ note :destination ()
      (if (boundp '@:_destination)
          @:_destination
        (@:init-destination))))




(def@ @test :def (method fn)
      (setf (@ @@ method) fn))

(@! (@ @test :_node) :def :init-x
    (lambda (@@) (message "test %s" (@ @@ :slot))))

(@! (@ @test :_node) :init-x)

(@ (@ @test :_node) :slot)

(@! (@ @test :_node) :keys)


(def@ (@ @test :_node) :init-y ()
      (message  "test %s" @:slot))


(@! (@ @test :_node) :init-y)

(@ (@ @test :_node) :init-y)


(@! @test :keys)



(defun xdef (x)
  (lexical-let ((N x))
    (function*
     (lambda (x N)
      (message "N: %s x: %s" N x)))))

(funcall (xdef "Test") "AAII" "x")


(function* 'a)



(when nil                               ;buff-trans
  (when nil
    (setf @buff-transition-span-detector
        (@! @transition-span-dectector-class :gen-buffer-trans "test")))

  (when nil
  (progn
    (defvar idle-start (current-time))
    (defvar idle-detect-timer nil)
    (defun print-last-idle ()
      (let* ((currtime (float-time (current-time)))
             (idle-starttime (float-time idle-start))
             (idle-secs (- currtime idle-starttime)))
        (message "idle for %d secs" idle-secs)))
    (defun idle-set ()
      (message "adding idle-set on read-char")
      (setq idle-start (current-time))
      (add-hook
       'pre-command-hook
       'print-last-idle-start-timer))
    (defun print-last-idle-start-timer (&rest args)
      (interactive)
      (print-last-idle)
      (message "removing idle-set on read-char")
      (remove-hook
       'pre-command-hook
       'print-last-idle-start-timer)
      (when idle-detect-timer
        (cancel-timer idle-detect-timer)
        (setq idle-detect-timer nil))
      (message "starting idle-set timer")
      (setq
       idle-detect-timer
       (run-with-idle-timer
        7 nil
        'idle-set)))

    (print-last-idle-start-timer)))

(when nil
  (progn
    ;; https://emacs.stackexchange.com/questions/32040/setting-and-clearing-an-is-idle-variable-when-going-in-and-out-of-idle-mode

    ;; Even though (current-idle-time) only seems to return a meaningful value
    ;; within the context of an idle handler, I figured out how to use it for my
    ;; purpose ...

    ;; Initialize to idle mode upon emacs startup.
    (defvar my-last-idle t
      "*Last idle time value.")

    (defun my-pre-command-hook ()
      (setq my-last-idle nil))

    (defun my-idle-timer-hook ()
      (setq my-last-idle (current-idle-time)))

    (add-hook 'pre-command-hook 'my-pre-command-hook)
    (run-with-idle-timer 1 t 'my-idle-timer-hook)

    ;; Then, my sigusr1/sigusr2 handler can do this ...

    (when my-last-idle)))
      ;; Do my signal-handling stuff.
      ;; ... etc. ...


(when nil
  (defun time-tracker-test ()
    (interactive)
    (lexical-let* ((delay 10)
                   (start (current-time))
                   (active-time 0)
                   (idle-time 0)
                   (later
                    (run-with-timer
                     delay
                     nil
                     (lambda ()
                       (let ((active-time
                              (-
                               (float-time (current-time))
                               (float-time start))))
                         (message
                          "active-time %d, idle-time %d"
                          active-time
                          idle-time))))))
      later))


  (read-event)




  (time-tracker-test)))











(when nil                               ;buff-trans



;; (@! @buffer-transition-span-detector :initialize)
;; (@! @org-clock-uninteractive-log-note :send "Changed to buffer %s from %s" 1 1)
;; (@! @buffer-transition :dispatch  "Changed to buffer %s from %s" (get-buffer "*scratch*") (current-buffer))

  (when nil


    (when nil
      (activity-register
       #'(lambda ()
           (@! @buffer-transition-span-detector :initialize)))

      (@! @buffer-transition-span-detector :uninitialize))


    (progn

      (setf @buff-trans
            (@! @transition-span-dectector-class :gen-buffer-trans-span-detector "test"))

      (setf @buff-trans (@! @transition-span-dectector-class :gen-buffer-trans-span-detector "test"))
      (@! @buff-trans :initialize))

    (@! @buff-trans :uninitialize)

    (length switch-buffer-functions)
    (setq switch-buffer-functions nil)


    ;; switch-buffer-functions

    (setf (@ @buffer-transition-span-detector :debug-switch-buf) nil)

    (@ @buff-trans :timer-gap)
    (functionp (@ @buff-trans :detect-buffer-chg-use))

    (car switch-buffer-functions)

    (add-hook 'switch-buffer-functions
              #'(lambda (prev curr)
                  (message "prev %s, curr %s" prev curr)))

    (defun enable-detect-buffer-chg-use ()
      (@:cancel-detect-buffer-chg-use)
      (add-hook 'post-command-hook #'add-idle-timer-hook)
      (add-hook 'switch-buffer-functions #'run-detect-buffer-chg))


    (defun disable-detect-buffer-chg-use ()
      (@:cancel-detect-buffer-chg-use)
      (remove-hook 'post-command-hook #'add-idle-timer-hook)
      (remove-hook 'switch-buffer-functions #'run-detect-buffer-chg)))

  ;; (def@ @@ :dispatch (&optional note)
  ;;   (@:initialize))

  ;; (@:dispatch note)

  ;; (macroexpand-1
  ;;  '(def@ @@ :notify-buf-chg (fmt &rest args) (apply #'message fmt args)))
  )
