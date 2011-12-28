;;; message.el --- GNUS Message

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords:

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

;;

;;; Code:

(defvar *use-msmtp-for-senmail* nil "msmtp to use")

;; where I am using msmtp
(setq *use-msmtp-for-senmail* (equal (system-name) "asfsdspratap"))

(if *use-msmtp-for-senmail* ;; where I am using msmtp
    (setq  ;; for msmtp
     ;; see http://www.gnus.org/manual/message_36.html
     message-sendmail-f-is-evil t
     message-sendmail-envelope-from nil)
    (setq
     ;; see http://www.gnus.org/manual/message_36.html
     message-sendmail-f-is-evil nil
     message-sendmail-envelope-from 'head))


;;{{ For SMTP msmtp

;; (if (equal (system-name) office-host-name)
(if nil ;(equal (system-name) office-host-name)
    (setq message-send-mail-function 'message-send-mail-with-sendmail
          sendmail-program "/usr/bin/msmtp" ;; we substitute sendmail with msmtp
          ; message-sendmail-extra-argouments "--tls-certcheck off"
          message-sendmail-extra-argouments nil
          message-sendmail-f-is-evil t
          message-sendmail-envelope-from 'header
          message-alternative-emails (regexp-opt (list email-addr office-email) )))

;;}} For SMTP msmtp



(gnus-registry-initialize)


(user-provide 'message)
;;; message.el ends here
