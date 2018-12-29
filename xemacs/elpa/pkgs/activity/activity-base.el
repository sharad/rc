;;; activity-base.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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


(defvar activity-subdirs
  (mapcar
   #'(lambda (dir)
       (expand-file-name dir (file-name-directory load-file-name)))
   '("node-dest" "activities")))

;;;###autoload
(defun activity-add-subdirs-load-path ()
  (dolist (dir activity-subdirs)
    (add-to-list 'load-path dir)))

;;;###autoload
(activity-add-subdirs-load-path)



(defmacro @extend-object (object &rest body)
  `(with-@@ object
     ,@(if (stringp (car body))
           `((setf @:doc ,(car body))))
     ,@(if (stringp (car body)) (cdr body) body)))
(put '@extend-object 'lisp-indent-function 1)

(defmacro @drive-object (object name &rest body)
  `(let ((drived-obj
          (@extend ,object
                   :name ,name)))

     (with-@@ drived-obj
              ,@(if (stringp (car body))
                    `((setf @:doc ,(car body))))
              ,@(if (stringp (car body)) (cdr body) body))

     drived-obj))
(put '@drive-object 'lisp-indent-function 2)

(defmacro defobjgen@ (object gen-method params &rest body )
  `(progn
     (def@ ,object ,gen-method (name ,@params)
       ,@(if (stringp (car body))
             (list (car body)) ())

       (@drive-object ,object name
         ,@(if (stringp (car body)) (cdr body) body)))))
(put 'defobjgen@ 'lisp-indent-function 3)

(progn
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\<\\(def@\\) +\\([^ ()]+\\)"
                             (1 'font-lock-keyword-face)
                             (2 'font-lock-function-name-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(@\\^?:[^ ()]+\\)\\>"
                             (1 'font-lock-builtin-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\<\\(defobjgen@\\) +\\([^ ()]+\\)"
                             (1 'font-lock-keyword-face)
                             (2 'font-lock-function-name-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(@drive-object\\)\\>"
                             (1 'font-lock-builtin-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(@extend-object\\)\\>"
                             (1 'font-lock-builtin-face)))))

(progn
  (string-match "(\\<\\(def@\\)\\> +\\([^ ()]+\\)" "(def@ x")

  (string-match "(\\<\\(def@\\) +\\([^ ()]+\\)" "(def@ x")

  (string-match "\\<\\(@\\^?:[^ ()]+\\)\\>" "@:aa")

  (string-match "\\(@\\^?:[^ ()]+\\)\\>" "@:aa"))


(setf @activity-base
      (@drive-object @ "activity-base"
                       "Activity Base"

                       (setf @:activation-list nil)

                       (def@ @@ :keyp (key)
                         (memq key (@:keys)))

                       (def@ @@ :finalize ()
                         ())

                       (def@ @@ :init ()
                         (@^:init)
                         (message "@activity-base :init")
                         (setf @:_occuredon (current-time)))

                       (def@ @@ :occuredon ()
                         (format-time-string "%Y-%m-%d %H:%M:%S" @:_occuredon))

                       (def@ @@ :dispatch ()
                         (@:init))

                       (@:dispatch)))


(setf @activities
      (@drive-object @activity-base "activities"
                     (setf @:insinuate nil
                           @:uninsinuate nil)

                     (def@ @@ :activate ()
                       (dolist (act @:insinuate)
                         (funcall act)))

                     (def@ @@ :deactivate ()
                       (dolist (act @:uninsinuate)
                         (funcall act)))

                     (def@ @@ :insinuate-add (fun)
                       (push fun @:insinuate))))

;;;###autoload
(defun activate-activity ()
  (interactive)
  (activity-add-subdirs-load-path)
  (@! @activities :activate))

;;;###autoload
(defun deactivate-activity ()
  (interactive)
  (@! @activities :deactivate))



(setf @dest-class
      (@drive-object @activity-base "dest-base-class"
                     "Destination Base Class"

                     (defobjgen@ @@ :gen-builder ()
                                    (def@ @@ :receive (fmt &rest args)
                                      (apply #'format
                                             fmt args)))

                     (defobjgen@ @@ :gen-msg ()
                                    (def@ @@ :receive (fmt &rest args)
                                      (apply #'message
                                             fmt args)))

                     (defobjgen@ @@ :gen-warning ()
                                    (def@ @@ :receive (fmt &rest args)
                                      (apply #'lwarn
                                             'activity
                                             'warning
                                             fmt args)))

                     (defobjgen@ @@ :gen-error ()
                                    (def@ @@ :receive (fmt &rest args)
                                      (apply #'lwarn
                                             'activity
                                             'error
                                             fmt args)))

                     (def@ @@ :dispatch ()
                       (@:init))

                     (@:dispatch)))



(setf @note-class
      (@drive-object @activity-base "note-base-class"
                     "Note Base Class"

                     (setf @:dests '())

                     (def@ @@ :send (&rest args)
                       "Node send method"
                       (if (and
                            ;; (memq :dests (@:keys))
                            @:dests
                            (consp @:dests))
                           (dolist (dest @:dests)
                             (if dest
                                 (if (@! dest :keyp :receive)
                                     ;; (@! dest :receive fmt args)
                                     (apply (@ dest :receive) dest args)
                                   (message
                                    "for %s dest %s [%s] not has :receive method, not sending msg."
                                    @:name
                                    (@ dest :name)
                                    (@! dest :keys)))
                               (message "dest is nil")))
                         (error "%s has No @:dests %d boundp(%s) consp(%s) present."
                                @:name
                                (length @:dests)
                                (boundp '@:dests)
                                (consp @:dests))))

                     ;; (defobjgen@ @@ :gen-format-msg ()
                     ;;                      "Generator for format message note"
                     ;;   (push
                     ;;    (@! @dest-class :gen-msg "msg")
                     ;;    @:dests))

                     ;; (defobjgen@ @@ :gen-org-log-note ()
                     ;;                      "Generator for org log note"
                     ;;   (push
                     ;;    (@! @dest-class :gen-msg "msg")
                     ;;    @:dests))

                     ;; (defobjgen@ @@ :gen-org-dual-log-note ()
                     ;;                      "Generator for dual org log note"
                     ;;   (push
                     ;;    (@! @dest-class :gen-msg "msg")
                     ;;    @:dests))

                     ;; (defobjgen@ @@ :gen-org-intreactive-log-note ()
                     ;;                      "Generator for Interactive org log note"
                     ;;   (push
                     ;;    (@! @dest-class :gen-msg "msg")
                     ;;    @:dests))

                     (def@ @@ :dispatch ()
                       (@:init))

                     (@:dispatch)))


(progn
  ;; activity
  (setf @activity-class
        (@drive-object @activity-base "activity class"
          "Activity class"
          (def@ @@ :init ()
            (@^:init)
            (message "@activity-class :init")
            (setf @:occuredon (current-time)))))

  (setf @event-class
        (@drive-object @activity-class "event class"
          "Event class"
          (def@ @@ :note ()
            )))

  (setf @transition-class
        (@drive-object @event-class "transition class"
          "Transition class"
          (def@ @@ :note ()
            ))))



(progn
  ;; detectors
  (setf @activity-dectector-class
        (@drive-object @activity-base "activity detector class"
          "Activity detector class"
          (def@ @@ :note ()
            )))

  (setf @event-dectector-class
        (@drive-object @activity-dectector-class "event detector class"
          "Event detector class"
          (def@ @@ :note ()
            )))

  (setf @transition-dectector-class
        (@drive-object @event-dectector-class "transition detector class"
          "Transition detector class"
          (def@ @@ :note ()
            )))

  (setf @event-span-dectector-class       ;TODO START
        (@drive-object @event-dectector-class "duration detector class"
          "Duration detector class"
          (def@ @@ :note ()
            )
          (def@ @@ :dispatch ()
            (setf
             @:start-time    0
             @:stop-time     0
             @:active-time   0
             @:inactive-time 0)
            )))

  (setf @transition-span-dectector-class       ;TODO START
        (@drive-object @transition-dectector-class "duration detector class"
          "Duration detector class"
          (def@ @@ :note ()
            )
          (def@ @@ :dispatch ()
            (setf
             @:start-time    0
             @:stop-time     0
             @:active-time   0
             @:inactive-time 0)
            ))))




(setf @postpone-event-class
      (@drive-object @activity-base "activity detector class"
        "Activity detector class"
        (def@ @@ :note ()
          )))

(setf @save-event-class
      (@drive-object @activity-base "activity detector class"
        "Activity detector class"
        (def@ @@ :note ()
          )))

(setf @idleness-dectector-class
      ;; set pre-command-hook
      ;; and measure time
      ;; collect in list
      ;; provide list return-reset functions
      (@drive-object @activity-base "activity detector class"
        "Activity detector class"
        (def@ @@ :note ()
          )))

(setf @activity
      (@drive-object @activity-base "activity"
        "Activity class"
        (def@ @@ :init ()
          (@^:init)
          (message "@activity-class :init")
          (setf @:occuredon (current-time)))

        (def@ @@ :start ()
          )

        (def@ @@ :stop ()
          )

        (def@ @@ :hook-register ()
          )))




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
