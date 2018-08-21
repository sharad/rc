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

(require 'org-activity-note)

(provide 'mail-event)


;; https://emacs.stackexchange.com/questions/101/how-can-i-create-an-org-link-for-each-email-sent-by-mu4e
;; http://kitchingroup.cheme.cmu.edu/blog/2014/06/08/Better-integration-of-org-mode-and-email/
;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el


(defun activity~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
               (or (equal c ?>)
                   (equal c ?<)))
             msgid))

(defun lotus-plist-get-members (plist keys)
  (mapcar
   '(lambda (k)
     (plist-get plist k))
   keys))

(defsubclass-gen@ @event-dectector-class :gen-mail-read-event-detector ()

  (def@ @@ :make-message ()
    (let* ((msgid (message-fetch-field "Message-ID"))
           (subject (message-fetch-field "Subject"))
           (from (message-fetch-field "From"))
           (to (message-fetch-field "To"))
           (link (concat "mu4e:msgid:" (activity~wipe-brackets msgid))))
      (list
       :subject
       subject
       :from
       from
       :to
       to)))

  (def@ @@ :make-event ()
    "Make mail read event."
    (let ((note (@! @:note :new)))
      (message "Helloooo")
      (message "Helloo %s" (@:make-message))
      (apply (@ note :send)
             note
             "Reading mail %s from %s to %s"
             ;;TODO arrange for plist
             (lotus-plist-get-members
              (@:make-message)
              '(:subject :from :to)))))

  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-clock-note)

    ;; gnus-Article-prepare-hook
    ;; gnus-Select-article-hook
    (add-hook
     'gnus-Article-prepare-hook
     (lambda ()
       (@! @@ :make-event))))

  (@:dispatch))


(defsubclass-gen@ @event-dectector-class :gen-mail-send-event-detector ()
  (def@ @@ :make-message ()
    (let* ((msgid (message-fetch-field "Message-ID"))
           (subject (message-fetch-field "Subject"))
           (from (message-fetch-field "From"))
           (to (message-fetch-field "To"))
           (link (concat "mu4e:msgid:" (activity~wipe-brackets msgid))))
      (list
       :subject
       subject
       :from
       from
       :to
       to)))

  (def@ @@ :make-event ()
    "Make mail send event."
    (let ((note (@! @:note :new)))
      (apply (@ note :send)
             note
             "Reading mail %s from %s to %s"
             ;;TODO arrange for plist
             (lotus-plist-get-members
              (@:make-message)
              '(:subject :from :to)))))

  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-clock-note)
    (add-hook
     'message-mode-hook
     (lambda ()
       (@! @@ :make-event))))

  (@:dispatch))


(setf @mail-read-event-detector-instance
      (@! @event-dectector-class :gen-mail-read-event-detector "gnus read mail event"))

(@! @mail-read-event-detector-instance :make-event)
;;; mail-event.el ends here
