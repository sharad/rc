;;; mail-event.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(provide 'mail-event)

(require 'org-capture-note)

;; https://emacs.stackexchange.com/questions/101/how-can-i-create-an-org-link-for-each-email-sent-by-mu4e
;; http://kitchingroup.cheme.cmu.edu/blog/2014/06/08/Better-integration-of-org-mode-and-email/
;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el
;; TODO: see it https://orgmode.org/manual/Template-expansion.html#Template-expansion


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

(defobjgen@ @event-dectector-class :gen-mail-read-event-detector ()

  (def@ @@ :make-message ()
    (let* ((msgid (message-fetch-field "Message-ID" t))
           (subject (message-fetch-field "Subject" t))
           (from (message-fetch-field "From" t))
           (to (message-fetch-field "To" t))
           (link (concat "mu4e:msgid:" (activity~wipe-brackets msgid))))
      (list
       :subject subject
       :from from
       :to to)))

  (def@ @@ :make-event ()
    "Make mail read event."
    (let ((note (@! @:note :new)))
      (@:message "Helloooo")
      (@:message "Helloo %s" (@:make-message))
      (funcall
       (@ note :send)
       note
       '(clock)
       (apply
        #'format
        (string-join
         '("* Reading mail subject: %s"
           "from: %s"
           "to: %s")
         "\n")
        (lotus-plist-get-members
         (@:make-message)
         '(:subject :from :to))))))

  (def@ @@ :make-event-gnus ()
    (when (and
           gnus-article-buffer
           (get-buffer gnus-article-buffer))
      (with-current-buffer (get-buffer gnus-article-buffer)
        (let ((subject
               (message-fetch-field "Subject" t)))
          (@:message "checking: %s" subject)
          (@:make-event)))))

  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))
  ;; gnus-Article-prepare-hook
  ;; gnus-Select-article-hook
  ;; (add-hook
  ;;  'gnus-article-prepare-hook
  ;;  (lambda () (@! @@ :make-event-gnus)))


  (@:dispatch))


(defobjgen@ @event-dectector-class :gen-mail-send-event-detector ()
  (def@ @@ :make-message ()
    (let* ((msgid (message-fetch-field "Message-ID" t))
           (subject (message-fetch-field "Subject" t))
           (from (message-fetch-field "From" t))
           (to (message-fetch-field "To" t))
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
             '(clock)
             (apply
              #'format
              (string-join
               '("* Sent mail subject: %s"
                 "to: %s")
               "\n")
              (lotus-plist-get-members
               (@:make-message)
               '(:subject :to))))))

  (def@ @@ :make-event-gnus ()
    (when (and gnus-message-buffer
               (get-buffer gnus-message-buffer))
      (with-current-buffer (get-buffer gnus-message-buffer)
        (let ((subject
               (message-fetch-field "Subject" t)))
          (@:message "sending mail: %s" subject)
          (@:make-event)))))

  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))

  (@:dispatch))


(defun mail-event-run-action ()
  (@! @mail-read-event-detector-instance :make-event-gnus))

;;;###autoload
(defun activity-mail-event-activate ()
  (setf @mail-read-event-detector-instance
        (@! @event-dectector-class :gen-mail-read-event-detector "gnus read mail event"))

  (setf @mail-send-event-detector-instance
        (@! @event-dectector-class :gen-mail-send-event-detector "gnus send mail event"))

  (add-hook
   'gnus-article-prepare-hook
   #'mail-event-run-action))

;;;###autoloda
(defun activity-mail-event-deactivate ()
  (remove-hook
   'gnus-article-prepare-hook
   #'mail-event-run-action))

;;;###autoload
(activity-register
 "mail-event"
 #'activity-mail-event-activate #'activity-mail-event-deactivate)

;; (nbutlast gnus-article-prepare-hook 1)

;;; mail-event.el ends here
