;;; org-activity-note.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

;; (require 'activity-base)

(provide 'org-activity-note)



(defsubclass-gen@ @dest-class :gen-org-dest (marker)
  (def@ @@ :dispatch (marker)
    (@:init)
    (setf @:marker marker))

  (def@ @@ :valid-markerp ()
    (cond
      ((markerp @:marker) @:marker)
      ((functionp @:marker)
       (let ((m (funcall @:marker))
             (if (markerp m) m))))
      ((symbolp @:marker)
       (let ((m (symbol-value @:marker))
             (if (markerp m) m))))
      (t )))

  (def@ @@ :get-marker ()
    (cond
      ((markerp @:marker) @:marker)
      ((functionp @:marker)
       (let ((m (funcall @:marker)))
             (if (markerp m)
                 m
               (error "f no marker %s" @:marker))))
      ((symbolp @:marker)
       (let ((m (symbol-value @:marker)))
             (if (markerp m)
                 m
               (error "s no marker %s" @:marker))))
      (t
       (error "can not find marker %s" @:marker))))

  (def@ @@ :receive (fmt &rest args)
    (org-insert-log-note (@:get-marker) (apply #'format fmt args) 'note))

  (@:dispatch marker))



(defsubclass-gen@ @note-class :gen-org-note (marker)
  "Generator for org note message"
  (push
   (@! @dest-class :gen-org-dest "msg" marker)
   @:dests))


(defsubclass-gen@ @note-class :gen-dual-org-note (marker1 marker2)
  "Generator for dual org note message"

  (def@ @@ :dispatch (marker1 marker2)
    (setf @:dest1 (@! @dest-class :gen-org-dest "org note dest1" marker1))
    (setf @:dest2 (@! @dest-class :gen-org-dest "org note dest2" marker2)))

  (def@ @@ :send1 (fmt &rest args)
    (apply (@ @:dest1 :receive) fmt args))

  (def@ @@ :send2 (fmt &rest args)
    (apply (@ @:dest2 :receive) fmt args))

  (@:dispatch marker1 marker2))


(defvar @org-clock-note
  (@! @note-class :gen-org-note
      "org-clock-note"
      #'(lambda ()
          (or
           org-clock-hd-marker
           org-clock-marker)))
  "Org clock activity node")

(when nil

  (setf @org-clock-note
        (@! @note-class :gen-org-note
            "org-clock-note"
            #'(lambda ()
                (or
                 org-clock-hd-marker
                 org-clock-marker))))

  (length (@ @org-clock-note :dests))

  (memq :dests (@! @org-clock-note :keys))

  (consp (@ @org-clock-note :dests))

  (@ @org-clock-note :name)

  (@! @org-clock-note :send "Hello")

  (@!
   (@! @dest-class :gen-org-dest "msg"
       #'(lambda ()
           (or
            org-clock-hd-marker
            org-clock-marker)))
   :receive "Test"))

;;; org-activity-note.el ends here
