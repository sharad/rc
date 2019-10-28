;; -*- lexical-binding: t -*-

;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el

;;; Store emails that are send/forwarded/replied to

;; Invoking lotus-org-mu4e-capture-next-message or setting lotus-org-mu4e-must-capture-message before composing a message will call org-capture after the email was sent successfully (using the capture template from lotus-mu4e-org-mode-capture-template-for-sent-email)

(defvar lotus-org-mu4e-must-capture-message nil
  "If set, the next composed mu4e message will automatically be captured with the template specified in lotus-mu4e-org-mode-capture-template-for-sent-email")

(defvar lotus-mu4e-captured-message-p nil
  "Plist with info about the most recently sent mu4e email for OrgMode purposes")

(defvar lotus-mu4e-org-mode-capture-template-for-sent-email "e"
  "The specific template from org-capture-templates to use when capturing a sent email automatically")

(add-hook 'message-sent-hook 'lotus-org-mu4e-store-link-on-sent-message)

(add-hook 'message-mode-hook (lambda ()
			       (message-add-action 'lotus-org-mu4e-capture-cancel
						   'send 'postpone 'kill)

			       (message-add-action 'lotus-capture-sent-message-if-needed
						   'send)))

(defun djr~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
	       (or (equal c ?>)
		   (equal c ?<)))
	     msgid))

(defun lotus-org-mu4e-store-link-on-sent-message ()
  "Store the sent message in many useful places"
  (interactive)
  (let* ((msgid (message-fetch-field "Message-ID"))
	 (description (message-fetch-field "Subject"))
	 (link (concat "mu4e:msgid:" (djr~wipe-brackets msgid)))
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
	  lotus-mu4e-captured-message-p org-store-link-plist)))

(defun lotus-capture-sent-message-if-needed ()
  (interactive)
  (if lotus-org-mu4e-must-capture-message
      (let* ((org-store-link-plist lotus-mu4e-captured-message-p)
	     (org-capture-link-is-already-stored t))
	(org-capture nil lotus-mu4e-org-mode-capture-template-for-sent-email))))

(defun lotus-org-mu4e-capture-cancel ()
  (interactive)
  (setq lotus-org-mu4e-must-capture-message nil
	global-mode-string (delq 'djr-org-capture-mode-line-string global-mode-string)))
(lotus-org-mu4e-capture-cancel)

(defun lotus-org-mu4e-capture-next-message ()
  (setq lotus-org-mu4e-must-capture-message t
	djr-org-capture-mode-line-string "Org capturing current mail")
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'djr-org-capture-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(djr-org-capture-mode-line-string)))))

(defun lotus-mu4e-compose-new-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-new))

(defun lotus-mu4e-compose-reply-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-reply))

(defun lotus-mu4e-forward-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-forward))

(provide 'org-note-mail)
