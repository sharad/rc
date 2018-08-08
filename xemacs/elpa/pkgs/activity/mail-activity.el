;;; mail-event.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(require 'activity-base)

(provide 'mail-event)


;; https://emacs.stackexchange.com/questions/101/how-can-i-create-an-org-link-for-each-email-sent-by-mu4e
;; http://kitchingroup.cheme.cmu.edu/blog/2014/06/08/Better-integration-of-org-mode-and-email/
;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el








(defsubclass-gen@ @event-dectector-class :gen-mail-read-event ()
  (def@ @@ :make-event ()
    "Make mail read event."
    (@! (@! @:tran :new) :send ))

  (def@ @@ :dispatch ()
    (setf @:tran @org-clock-note))

  (@:dispatch))


(defsubclass-gen@ @event-dectector-class :gen-mail-send-event ()
  (def@ @@ :make-event ()
    "Make mail send event."
    (@! (@! @:tran :new) :send ))

  (def@ @@ :dispatch ()
    (setf @:tran @org-clock-note))

  (@:dispatch))

;;; mail-event.el ends here
