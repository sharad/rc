;;; act.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: nil; -*-

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

(provide 'act)

(defvar @act-base
  (@extend
   :name "act base."
   :finilize-args ()))

(def@ @act-base :keyp (key)
      (memq key (@:keys)))

(def@ @act-base :finalize ()
      ())

(defmacro defsubobj@ (object name params &rest body)
  `(let ((drived-obj
          (@extend ,object
                   :name (concat (@ ,object :name) " > " ,name))))

     (with-@@ drived-obj
       ,@(if (stringp (car body))
            `((setf @:doc ,(car body))))
       ,@(if (stringp (car body)) (cdr body) body))

     drived-obj))
(put 'defsubobj@ 'lisp-indent-function 3)

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


(progn
  (setf @test-base
        (defsubobj@ @ "test-base"
          "test base"

          (def@ @@ :init ()
            (message "@test-base :init start")
            (@^:init)
            (message "@test-base :init finish"))

          (def@ @@ :dispatch ()
            (message "@test-base :dispatch start")
            (@:init)
            (message "@test-base :dispatch finish"))

          (@:dispatch)))

  (setf @test-base1
        (defsubobj@ @test-base "test base1"
          "test base1"

          (def@ @@ :init ()
            (message "@test-base1 :init start")
            (@^:init)
            (message "@test-base1 :init finish"))

          (def@ @@ :dispatch ()
            (message "@test-base1 :dispatch start")
            (@^:init)
            (message "@test-base1 :dispatch finish"))

          (@:dispatch))))




(progn
  (setf @test-base
        (let ((drived-obj
               (@extend @ :name "test-base")))
          (def@ drived-obj :init ()
            (message "@test-base :init start")
            (@^:init)
            (message "@test-base :init finish"))

          (def@ drived-obj :dispatch ()
            (message "@test-base :dispatch start")
            (@:init)
            (message "@test-base :dispatch finish"))

          (@! drived-obj :dispatch)))

  (setf @test-base1
        (let ((drived-obj
               (@extend @test-base :name "test-base1")))

          (def@ drived-obj :init ()
            (message "@test-base1 :init start")
            (@^:init)
            (message "@test-base1 :init finish"))

          (def@ drived-obj :dispatch ()
            (message "@test-base1 :dispatch start")
            (@^:init)
            (message "@test-base1 :dispatch finish"))

          (@! drived-obj :dispatch))))



(progn
  (setf @test-base
        (let ((@@
               (@extend @ :name "test-base")))
          (def@ @@ :init ()
                (message "@test-base :init start")
                (@^:init)
                (message "@test-base :init finish"))

          (def@ @@ :dispatch ()
                (message "@test-base :dispatch start")
                (@:init)
                (message "@test-base :dispatch finish"))

          (@! @@ :dispatch)))

  (setf @test-base1
        (let ((@@
               (@extend @test-base :name "test-base1")))

          (def@ @@ :init ()
                (message "@test-base1 :init start")
                (@^:init)
                (message "@test-base1 :init finish"))

          (def@ @@ :dispatch ()
                (message "@test-base1 :dispatch start")
                (@^:init)
                (message "@test-base1 :dispatch finish"))

          (@! @@ :dispatch))))

(progn
  (setf @test-base
        (@extend @ :name "test-base"))
  (def@ @test-base :init ()
        (message "@test-base :init start")
        (@^:init)
        (message "@test-base :init finish"))
  (def@ @test-base :dispatch ()
        (message "@test-base :dispatch start")
        (@:init)
        (message "@test-base :dispatch finish"))
  (@! @test-base :dispatch)

  (setf @test-base1
        (@extend @test-base :name "test-base1"))
  (def@ @test-base1 :init ()
        (message "@test-base1 :init start")
        (@^:init)
        (message "@test-base1 :init finish"))
  (def@ @test-base1 :dispatch ()
        (message "@test-base1 :dispatch start")
        (@:init)
        (message "@test-base1 :dispatch finish"))
  (@! @test-base1 :dispatch))




(macroexpand-1
 '(with-@@ baee
   (def@ @@ :init ()
     (@^:init)
     (@:fun)
     @:attr)))


(let ((@@ (@extend @ :name "test")))
  (def@ @@ :init nil (message "@test-base1 :init start") (funcall (@ @@@ :init :super t) @@) (message "@test-base1 :init finish"))
  (@! @@ :init))





(macroexpand-1
 '(def@ OBJ :init (x)
   (@^:init)
   (@:fun)
   @:attr))

(progn
  (setf
   (@ OBJ :init)
   (function*
    (lambda (@@ x)
     (let ((@@@ OBJ))
       (with-@@ @@ (@^:init) (@:fun) @:attr))))) :init)


(progn
  (setf (@ OBJ :init)
        (function*
         (lambda (@@)
          (let ((@@@ OBJ))
            (let ((@@ @@))
              (funcall (@ @@@ :init :super t) @@)
              (@! @@ :fun)
              (@ @@ :attr)))))) :init)
























(defun @--replace (symbol head)
  "Replace @: and @^: symbols with their lookup/funcall expansions."
  (let ((name (symbol-name symbol)))
    (cond
      ((string-match "^@@+$" name)
       (intern (concat name "@")))
      ((string-prefix-p "@:" name)
           (let ((property (intern (substring name 1))))
             (if head
                 `(@! @@ ,property)
               `(@ @@ ,property))))
      ((string-prefix-p "@^:" name)
       (let ((property (intern (substring name 2))))
         (if head
             `(funcall (@ @@@ ,property :super t) @@)
           `(@ @@ ,property :super t))))
      (t (if head (list symbol) symbol)))))


(progn
  (setf @test-base
        (defsubobj@ @ "test-base"
            "test base"

          (def@ @@ :init ()
            (message "@test-base :init start")
            (@^:init)
            (message "@test-base :init finish"))

          (def@ @@ :dispatch ()
            (message "@test-base :dispatch start")
            (@:init)
            (message "@test-base :dispatch finish"))

          (@:dispatch)))

  (setf @test-base1
        (defsubobj@ @test-base "test base1"
          "test base1"

          (def@ @@ :init ()
            (message "@test-base1 :init start")
            (@^:init)
            (message "@test-base1 :init finish"))

          (def@ @@ :dispatch ()
            (message "@test-base1 :dispatch start")
            (@^:init)
            (message "@test-base1 :dispatch finish"))

          (@:dispatch))))



(defun @--replace (symbol head)
  "Replace @: and @^: symbols with their lookup/funcall expansions."
  (let ((name (symbol-name symbol)))
    (cond
      ((string-match "^@@+$" name)
       (intern (concat name "@@")))
      ((string-prefix-p "@:" name)
       (let ((property (intern (substring name 1))))
         (if head
             `(@! @@ ,property)
           `(@ @@ ,property))))
      ((string-prefix-p "@^:" name)
       (let ((property (intern (substring name 2))))
         (if head
             `(funcall (@ @@@ ,property :super t) @@)
           `(@ @@ ,property :super t))))
      (t (if head (list symbol) symbol)))))

(defmacro with-@@ (object &rest body)
  "Provide the @: and @^: DSL utilities for OBJECT in BODY."
  (declare (indent defun))
  `(let ((@@ ,object))
     ,@(cdr (@--walk (cons 'progn body) '(quote with-@@) #'@--replace))))


(macroexpand-all
 '(defsubobj@ @ "test-base"
   "test base"

   (def@ @@ :init ()
     (message "@test-base :init start")
     (@^:init)
     (message "@test-base :init finish"))



   (@:dispatch)))





;;; act.el ends here
