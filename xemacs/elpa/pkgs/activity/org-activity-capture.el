;;; org-activity-log-note.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(require 'activity-base)

(provide 'org-activity-log-note)



(defsubclass-gen@ @dest-class :gen-org-capture-dest (marker)
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
    ;; TODO
    ;; add necessary code for interactive note.
    (progn
      (move-marker org-capture-note-return-to (point))
      (org-clock-lotus-with-current-clock ;change it to consider marker marker
          (org-add-log-setup-with-timed-new-win win-timeout
                                                'note nil nil nil
                                                (concat
                                                 "# Task: " (org-get-heading t) "\n\n"
                                                 (apply #'format fmt args))))))

  (@:dispatch marker))



(defsubclass-gen@ @note-class :gen-org-capture-note (marker)
  "Generator for org note message"
  (push
   (@! @dest-class :gen-org-dest "msg" marker)
   @:dests))

(defvar @org-clock-capture
  (@! @note-class :gen-org-capture-note
      "org-clock-log-note"
      #'(lambda ()
          (or
           org-clock-hd-marker
           org-clock-marker)))
  "Org clock activity node")


;; (@! @org-clock-note :send "Hello")

;;; org-activity-log-note.el ends here
