;;; activity-note.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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
(require 'activity-base)

(provide 'activity-note)


(def@ @activity :init-note ()
      (let ((note
             (@extend @@
              :name "activity note"
              :destinations nil)))

        (def@ note :add-dest (dest)
              (message "add-dest: before adding %d" (length @:destinations))
              (push dest @:destinations)
              (message "add-dest: adding %s destination" (car @:destinations)))

        (def@ note :send (fmt &rest args)
              (if @:destinations
                  (dolist (dest @:destinations)
                    (if dest
                        (@! dest :receive fmt args)
                      (message "dest is nil, not sending msg."))
                    (message "dest %s: received msg: %s"
                             (if dest (@ dest :name))
                             (apply #'format fmt args)))
                (error "No @:destinations present.")))

        (def@ note :init (dests)
              (let (msg-dest)
                (setf msg-dest
                      (@extend @note-destination
                               :name "message note destination"))
                (push msg-dest
                      @:destinations)))

        (setf @:note note)))

(def@ @activity :init-destination ()
      (let ((destionation
             (@extend @@
              :name "activity note destionation"
              :destinations nil)))

        (def@ @note-destination :receive (msg)
              (error "implement receive method"))


        (setf @:destionation destionation)))







;; message destionations
(def@ @activity-note :make-msg-note-dest ()
      (let  ((msg-note-dest
              (@extend @note-destination
                       :name "message note destination")))
        (def@ msg-note-dest :receive (fmt &rest args)
              (apply #'message fmt args))
        msg-note-dest))

(def@ @activity-note :make-debug-note-dest ()
      (let ((debug-note-dest
             (@extend @note-destination
                      :name "message note destination")))
        (def@ debug-note-dest :receive (fmt &rest args)
              (lwarn 'activity 'debug fmt args))
        debug-note-dest))

(def@ @activity-note :make-warning-note-dest ()
      (let ((warning-note-dest
             (@extend @note-destination
                      :name "message note destination")))
        (def@ warning-note-dest :receive (fmt &rest args)
              (lwarn 'activity 'warning fmt args))
        warning-note-dest))


(def@ @activity-note :make-error-note-dest ()
      (let ((error-note-dest
             (@extend @note-destination
                      :name "message note destination")))
        (def@ error-note-dest :receive (fmt &rest args)
              (lwarn 'activity 'error fmt args))
        error-note-dest))


;; org heading destinations
(def@ @activity-note :make-org-heading-note-dest ()
      (let ((org-heading-note-dest
             (@extend @note-destination
                      :name "message note destination")))

        ;; TODO
        (def@ org-heading-note-dest :init (marker &optional ignore-error)
              (setf
               @:marker       marker
               @:ignore-error ignore-error))

        (def@ org-heading-note-dest :receive (fmt &rest args)
              ;; (org-insert-log-note
              ;;  @:marker
              ;;  txt &optional purpose effective-time state previous-state)
              (let ((marker
                     (cond
                       ((markerp @:marker) @:marker)
                       ((symbolp @:marker) (symbol-value @:marker))
                       (t (error "unknown value of @:marker %s" @:marker)))))
                (if (markerp @:marker)
                    (org-insert-log-note
                     (if @:marker)
                     (format fmt args)
                     'note)
                  (unless @:ignore-error
                    (error "unknown value of @:marker %s" @:marker)))))
        org-heading-note-dest))


;; fixed destinations
(defvar @org-sink-note-destination nil "Org sink heading")

(defun @set-org-sink-note-destination (marker)
 (setf @org-sink-note-destination
       (@! @org-heading-note-destination :new marker)))

(defvar @org-clock-note-destination nil "Org current clock heading")

(defun set-org-clock-note-destination ()
  (setf @org-clock-note-destination
        (@! @org-heading-note-destination :new 'org-clock-hd-marker t)))





(setf @plain-note
  (@extend @activity-note
           :name "plain-note"))

(@! @plain-note :add-dest @message-note-destination)
(@! @plain-note :add-dest @org-clock-note-destination)
(@! @plain-note :add-dest @org-sink-note-destination)

(car (@ @plain-note :destinations))

(progn


  (@! @plain-note :send "Hello %s" "test")
  )

;;; activity-note.el ends here
