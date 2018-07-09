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

(provide 'activity-note)




(defvar @note-destination
  (@extend :name "note destination"))

(def@ @note-destination :receive (msg)
      (error "implement receive method"))

(defvar @activity-note
  (@extend :name "activity note"
           :destinations nil))

(def@ @activity-note :init (dests)
      (setf @:destionations dests))

(def@ @activity-note :add-dest (dest)
      (push dest @:destionations))

(def@ @activity-note :send (fmt &rest args)
      (if @:destionations
          (dolist (dest @:destionations)
            (@! dest :receive fmt args)
            (message "dest %s: received msg: %s"
                     (@ dest :name)
                     (format fmt args)))
        (error "No @:destionations present.")))

;; message destionations
(defvar @message-note-destination
  (@exten @note-destination
          :name "message note destination"))

(def@ @message-note-destination :receive (fmt &rest args)
      (message fmt args))

;; debug destionations
(defvar @debug-note-destination
  (@exten @note-destination
          :name "message note destination"))

(def@ @debug-note-destination :receive (fmt &rest args)
      (lwarn 'activity 'debug fmt args))

;; warning destionations
(defvar @warning-note-destination
  (@exten @note-destination
          :name "message note destination"))

(def@ @warning-note-destination :receive (fmt &rest args)
      (lwarn 'activity 'warning fmt args))

;; error destionations
(defvar @error-note-destination
  (@exten @note-destination
          :name "message note destination"))

(def@ @error-note-destination :receive (fmt &rest args)
      (lwarn 'activity 'error fmt args))

;; org heading destinations

(defvar @org-heading-note-destination
  (@exten @note-destination
          :name "message note destination"))

(def@ @org-heading-note-destination :init (marker &optional ignore-error)
      (setf
       @:marker       marker
       @:ignore-error ignore-error))

(def@ @org-heading-note-destination :receive (fmt &rest args)
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


;; fixed destinations
(defvar @org-sink-note-destination nil "Org sink heading")

(defun @set-org-sink-note-destination (marker)
 (setf @org-sink-note-destination
       (@! @org-heading-note-destination :new marker)))

(defvar @org-clock-note-destination nil "Org current clock heading")

(defun set-org-clock-note-destination ()
  (setf @org-clock-note-destination
        (@! @org-heading-note-destination :new 'org-clock-hd-marker)))





;;; activity-note.el ends here
