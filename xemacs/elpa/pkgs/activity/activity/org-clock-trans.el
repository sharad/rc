;;; org-clock-trans.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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
(require 'org-activity-note)

(provide 'org-clock-trans)



(setf @org-clock-trans-class
      (@extend-object @transition-class "org-clock-trans"))

(defobjgen@ @org-clock-trans-class :gen-org-clock-trans (marker1 marker2)
  "Org clock transition class"

  (def@ @@ :marker-heading (marker)
    "heading of %s" marker)

  (def@ @@ :send (prev next)
    (@! @:note :send1 "clocking out to clock into %s on %s"
        (@:marker-heading next) (@:occuredon))

    (@! @:note :send2 "clocking in after clocking out from %s on %s"
        (@:marker-heading prev) (@:occuredon)))

  (def@ @@ :dispatch (marker1 marker2)
    (@:init)
    (setf @:note
          (@! @note-class :gen-dual-org-note marker1 marker2)))

  (@:dispatch marker1 marker2))



(defobjgen@ @transition-dectector-class :gen-org-clock-trans (prev next)

  (def@ @@ :make-event (prev next)
    "Make buffer change event."
    (let ((trans
           (@! @org-clock-trans-class :gen-org-clock-trans prev next)))
      (message "running :make-event")
      (@! trans :send prev next)))

  (def@ @@ :dispatch (prev next)
    (@:make-event prev next)))

;; call where clock transition happening
;; (@! @transition-dectector-class m1 m2)


;;; org-clock-trans.el ends here
