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
(defvar @event
  (@extend @activity :name "Event"))

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

;;; activity-base.el ends here
