;;; activity-base.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(provide 'activity-base)

(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")

(defmacro defsubobj@ (object name params &rest body)
  `(let ((drived-obj
          (@extend ,object
                   :name (concat (@ ,object :name) " > " ,name))))

     (with-@@ drived-obj
       ,@(if (stringp (car body))
            `((setf @:doc ,(car body))))
       ,@(if (stringp (car body)) (cdr body) body))

     drived-obj))
(put 'defsubobj@ 'lisp-indent-function 2)

(defmacro defsubclass-gen@ (object gen-method params &rest body )
  `(progn
     (def@ ,object ,gen-method (name ,@params)
           ,@(if (stringp (car body)) (list (car body)) ())
           (defsubobj@ ,object name ,params
             ,@(if (stringp (car body)) (cdr body) body)))))
(put 'defsubclass-gen@ 'lisp-indent-function 3)






(defvar @activity-base
  (defsubobj@ @ "activity-base" ()
    "Activity Base"

    (def@ @@ :keyp (key)
      (memq key (@:keys)))

    (def@ @@ :finalize ()
      ())

    (def@ @@ :init ()
      (@^:init)
      (setf @:_occuredon (current-time)))

    (def@ @@ :occuredon ()
      (format-time-string "%Y-%m-%d %H:%M:%S" @:_occuredon))

    (def@ :dispatch ()
      (@:init))

    (@:dispatch)))






(progn
  ;; destination
  (setf @dest-class
        (@extend @activity-base
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


(progn
  ;; note
  (setf @note-class
        (@extend @activity-base
                 :name "note class"))
  (setf (@ @note-class :dests) '())

  (def@ @note-class :send (fmt &rest args)
        (if (and (memq :dests (@:keys))
                 (consp @:dests))
            (dolist (dest @:dests)
              (if dest
                  (if (@! dest :keyp :receive)
                      ;; (@! dest :receive fmt args)
                      (apply (@ dest :receive) dest fmt args)
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
        (defsubobj@ @activity-base "activity class" ()
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
        (defsubobj@ @activity-base "activity detector class" ()
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


;;; act.el ends here

;; based on note type correct destination should be chosen.
;; objects
;; 1. activity
;;   event
;;     mail send
;;     mail read
;;   transition
;;     buffer transition
;;     clock transition
;; note
;; note-destination
;;

;; just write prototype code
;; like sending function and their parameters etc
;; later it could be found to manage it to implement.

;; how I can pass different types of value e.g. (fmt arg1 arg2 ) and sexp
;; and it will be handled by corresponding handler ?

;;
;; * Do not forget about pragmatism
;; ** First complete working model, somehow
;; ** Things should be definable outside of this library based on framework provided here.


;;; activity-base.el ends here
