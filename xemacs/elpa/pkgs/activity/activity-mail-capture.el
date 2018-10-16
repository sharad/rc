;;; activity-mail-capture.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(provide 'activity-mail-capture)


;; copied from https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el


;;; Store emails that are send/forwarded/replied to

;; Invoking activity-org-mu4e-capture-next-message or setting
;; activity-org-mu4e-must-capture-message before composing a message will call
;; org-capture after the email was sent successfully (using the capture template
;; from activity-mu4e-org-mode-capture-template-for-sent-email)

(defvar activity-org-mu4e-must-capture-message nil
  "If set, the next composed mu4e message will automatically be captured with the template specified in activity-mu4e-org-mode-capture-template-for-sent-email")

(defvar activity-mu4e-captured-message-p nil
  "Plist with info about the most recently sent mu4e email for OrgMode purposes")

(defvar activity-mu4e-org-mode-capture-template-for-sent-email "e"
  "The specific template from org-capture-templates to use when capturing a sent email automatically")

(add-hook 'message-sent-hook 'activity-org-mu4e-store-link-on-sent-message)

(add-hook 'message-mode-hook (lambda ()
                               (message-add-action
                                'activity-org-mu4e-capture-cancel
                                'send 'postpone 'kill)

                               (message-add-action
                                'activity-capture-sent-message-if-needed
                                'send)))

(defun activity~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
               (or (equal c ?>)
                   (equal c ?<)))
             msgid))

(defun activity-org-mu4e-store-link-on-sent-message ()
  "Store the sent message in many useful places"
  (interactive)
  (let* ((msgid (message-fetch-field "Message-ID"))
         (description (message-fetch-field "Subject"))
         (link (concat "mu4e:msgid:" (activity~wipe-brackets msgid)))
         (org-link-string (org-make-link-string link description))
         (captured-message-p
          `(:type mu4e
                  :description ,description
                  :link ,link
                  :annotation ,org-link-string
                  :message-id ,msgid))
         (stored-link (list link description)))
    (push stored-link org-stored-links)
    (setq org-store-link-plist captured-message-p
          activity-mu4e-captured-message-p org-store-link-plist)))

(defun activity-capture-sent-message-if-needed ()
  (interactive)
  (if activity-org-mu4e-must-capture-message
      (let* ((org-store-link-plist activity-mu4e-captured-message-p)
             (org-capture-link-is-already-stored t))
        (org-capture nil activity-mu4e-org-mode-capture-template-for-sent-email))))

(defun activity-org-mu4e-capture-cancel ()
  (interactive)
  (setq
   activity-org-mu4e-must-capture-message nil
   global-mode-string (delq 'activity-org-capture-mode-line-string global-mode-string)))
(activity-org-mu4e-capture-cancel)

(when nil
 (defun activity-org-mu4e-capture-next-message ()
   (setq activity-org-mu4e-must-capture-message t
         activity-org-capture-mode-line-string "Org capturing current mail")
   (or global-mode-string (setq global-mode-string '("")))
   (or (memq 'activity-org-capture-mode-line-string global-mode-string)
       (setq global-mode-string
             (append global-mode-string '(activity-org-capture-mode-line-string)))))

 (defun activity-mu4e-compose-new-with-follow-up ()
   (interactive)
   (activity-org-mu4e-capture-next-message)
   (mu4e-compose-new))

 (defun activity-mu4e-compose-reply-with-follow-up ()
   (interactive)
   (activity-org-mu4e-capture-next-message)
   (mu4e-compose-reply))

 (defun activity-mu4e-forward-with-follow-up ()
   (interactive)
   (activity-org-mu4e-capture-next-message)
   (mu4e-compose-forward)))



(when nil
  (add-hook 'message-mode-hook (lambda ()
                                 (message-add-action
                                  `(read-from-minibuffer "send postpone kill: " )
                                  'send 'postpone 'kill)

                                 (message-add-action
                                  `(read-from-minibuffer "send : " )
                                  'send))))
;;; activity-mail-capture.el ends here
