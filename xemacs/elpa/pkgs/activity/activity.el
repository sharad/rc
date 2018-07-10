;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d@gmail.com>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(require '@)

(require 'activity-note)

(provide 'activity)

(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")

(progn
  
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






(defvar @methods-enforce
  (@extend :name "methods enforce"
           :occuredon (current-time)))

(def@ @methods-enforce :init ()
      (@^:init)
      (setf @:occuredon (current-time)))

(def@ @methods-enforce :enforce (object name)
      `(progn
         (def@ ,object ,name
           (error "define method %s for object %s"
                  ,name ,object))))

(def@ @methods-enforce :un-enforce (object name)
      `(progn
         ))


(defvar @activity
  (@extend @methods-enforce
           :name "Class Activity"
           :occuredon (current-time)))

(def@ @activity :init ()
      (@^:init)
      (setf @:occuredon (current-time)))

(def@ @activity :log ()
      (message "Time %s" (@:occuredon)))

(def@ @activity :message ()
      (error "No :message function found."))

(def@ @activity :object-sexp ()
      (error "No :object-sexp function found."))

(def@ @activity :occuredon ()
      (format-time-string "%Y-%m-%d" @:occuredon))

;; (describe-@ @activity :name)



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



(defvar @transition
  (@extend @activity :name "Class Transition"))

(setf (@ @transition :old) nil)

(def@ @transition :init (new)
      (@^:init)
      (setf @:new new))


(defvar @transition-singleton
  (@extend @transition :name "Class Transition"))

(def@ @transition-singleton :init (new)
      (@^:init)
      (setf @:new new))


(setf @buffer-transition-singleton
      (@extend @transition-singleton @dispatchable-immediate))

(def@ @buffer-transition-singleton :init (new-buffer)
      (@^:init new-buffer))

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



(defvar @event
  (@extend @activity :name "Event"))


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
      )

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


;; (call-next-method)


(defun activity-bind-hooks ()
  "Watch for activity in buffers."
  ;; (add-hook 'after-save-hook 'activity-save nil t)
  ;; (add-hook 'auto-save-hook 'activity-save nil t)
  ;; (add-hook 'first-change-hook 'activity-ping nil t)
  )

(defun activity-unbind-hooks ()
  "Stop watching for activity in buffers."
  ;; (remove-hook 'after-save-hook 'activity-save t)
  ;; (remove-hook 'auto-save-hook 'activity-save t)
  ;; (remove-hook 'first-change-hook 'activity-ping t)
  )

(defun activity-turn-on (defer)
  "Turn on Activity."
  (activity-bind-hooks))

(defun activity-turn-off ()
  "Turn off Activity."
  (activity-unbind-hooks))

;;;###autoload
(define-minor-mode activity-mode
  "Toggle Activity (Activity mode)."
  :lighter    " act"
  :init-value nil
  :global     nil
  :group      'activity
  (cond
    (noninteractive (setq activity-mode nil))
    (activity-mode (activity-turn-on t))
    (t (activity-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-activity-mode activity-mode
  (lambda () (activity-mode 1)))












(progn

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


(progn

  (defvar @lister
    (@extend :name "lister"))

  ()

 )

;;; activity.el ends here
