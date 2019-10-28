;;; message-config.el --- GNUS Message

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

(require 'citation-config)


;; send using /usr/bin/sendmial
(setq send-mail-function 'sendmail-send-it)

;; Increase the score for followups to a sent article.
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)


;;{{ http://www.gnus.org/manual/gnus_401.html
(when (xrequire 'ispell)
  ;; Ispell.el assumes you use ispell, if you choose aspell say
  ;; (setq ispell-program-name "timeout -k 12 10 aspell")
  (setq ispell-program-name "aspell")
  ;; in your Emacs configuration file.  If you want your outgoing
  ;; messages to be spell-checked, say
  (add-hook 'message-send-hook 'ispell-message)
  ;; In your ~/.gnus.el, if you prefer on-the-fly spell-checking say
  (add-hook 'message-mode-hook (lambda () (flyspell-mode 1))))
(add-hook 'message-mode-hook (lambda () (footnote-mode 1)))
;;}}


;;{{ Address Book http://www.emacswiki.org/emacs/ExternalAbook
(when (xrequire 'external-abook)
  (setq external-abook-command "timeout 4 /usr/bin/lbdbq '%s*' | sed 1d | cut -d'	' -f1,2") ;;"contacts -lf '%%e\t%%n' %s")

  (eval-after-load "message"
    '(progn
      (add-to-list 'message-mode-hook
       '(lambda ()
         (define-key message-mode-map "\C-c\t" 'external-abook-try-expand))))))
;;}}


;;{{ from: http://tilde.co.kr/.gnus
;;; Signature
(setq gnus-signature-separator
      '("^-- $"
        "^-- *$"
        "^-------*$"
        "^ *--------*$"
        "^________*$"
        "^========*$"))
;;}}


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
     message-sendmail-envelope-from 'header
     mail-specify-envelope-from t
     mail-envelope-from 'header))


;;{{ For SMTP msmtp

;; (if (equal (system-name) office-host-name)
(if nil ;(equal (system-name) office-host-name)
    (setq message-send-mail-function 'message-send-mail-with-sendmail
          sendmail-program "/usr/bin/msmtp" ;; we substitute sendmail with msmtp
          ; message-sendmail-extra-argouments "--tls-certcheck off"
          message-sendmail-extra-arguments nil
          message-sendmail-f-is-evil t
          message-sendmail-envelope-from 'header
          message-alternative-emails (regexp-opt (list email-addr office-email) )))

;;}} For SMTP msmtp


;;
;; (xrequire 'eieio)
;; (xrequire 'registry)

;; (gnus-registry-initialize)




;; When composing a mail, start the auto-fill-mode.
(add-hook 'message-mode-hook ;          'turn-on-auto-fill)
          '(lambda ()
            (turn-on-auto-fill)
            (setq fill-column 70)))
;; (add-hook 'message-mode-hook 'footnote-mode)

;; Generate the mail headers before you edit your message.
(setq message-generate-headers-first t)


;; The message buffer will be killed after sending a message.
(setq message-kill-buffer-on-exit t)



;;{{ http://www.gnus.org/manual/gnus_401.html
;; Question 5.9
;; Sometimes I accidentally hit r instead of f in newsgroups. Can Gnus warn me, when I'm replying by mail in newsgroups?
;; Answer
;; Put this in ~/.gnus.el:
(setq gnus-confirm-mail-reply-to-news t)
;; People tell me my Message-IDs are not correct, why aren't they
;; and how to fix it?
;; Answer
;; The message-ID is an unique identifier for messages you send. To
;; make it unique, Gnus need to know which machine name to put after
;; the "@". If the name of the machine where Gnus is running isn't
;; suitable (it probably isn't at most private machines) you can tell
;; Gnus what to use by saying:
(setq message-user-fqdn (concat "personal.machine.of." myshortname ".com"))
;;}}








;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; General speedups.

;; Add formalities for me.
(defadvice gnus-summary-reply (after formalities () activate)
  ;; (cjb-add-formalities)
  "Thanks."
  (dummy-add-formalities)
  )

(defun dummy-add-formalities ()
  "Thanks."
  (message-position-point)
  )

(defun cjb-add-formalities ()
  "Add the sender's first name and my tag to e-mail."
  ;; Modified from <http://www.repose.cx/conf/.elisp/de-gnus.el>
  (save-excursion
    (message-goto-signature)
    ;; (previous-line 1)
    (forward-line)
    (when (not (looking-at "Sharad"))
         (insert "\n\n- Sharad.")))
    (let* ((to (message-fetch-field "To"))
         (address-comp (mail-extract-address-components to))
         (name (car address-comp))
         (first (or (and name (concat "" (car (split-string name)))) "")))

         (when first
           ;; Go to the first line of the message body.
           (message-goto-body)
           (insert "Hi,\n\n")
           (kill-line)
           (kill-line)
           (kill-line)
           (message-goto-signature)
           (forward-line -4)
           (newline)
	)))
;;}}






;; Display the signatures in a less readable font.
(xrequire 'sigbegone)

;;{{ For SMTP msmtp
;; Now, we’d like to use Gnus to send email through msmtp. Add the
;; following lines to the .gnus.el file.
;; with Emacs 23.1, you have to set this explicitly (in MS Windows)
;; otherwise it tries to send through OS associated mail client

;;need to tell msmtp which account we're using
;; (setq message-sendmail-extra-arguments '("-a" "anderson"))
;; (setq message-sendmail-extra-arguments '(" -oem -oi"))
;; (setq user-mail-address office-email)

;; you might want to set the following too
;; (setq mail-host-address office-host-name)
;; (setq user-full-name "Sharad Pratap")
(setq message-cite-reply-above nil
      message-cite-reply-above t
      ;; http://emacsworld.blogspot.in/2011/11/gnus-tip-customising-position-of-point.html
      message-cite-reply-position 'traditional
      message-cite-reply-position 'above)     ;default

(defun lotus-message-signature-present ()
  (save-excursion
    (if (message-goto-signature)
        (eobp))))

(defun jreply (&optional keys)
  (interactive )
  ;; "asdfsdgfd"
  (let* ((resume "sharad")
         (resume-make-keys (format "make -sC %s name=%s keys" resume-workdir resume))
         (keys (or keys  (read-string "keys: " (shell-command-to-string resume-make-keys)))))
    (if (and (message-goto-body)
             (message-in-body-p))
        (progn
          ;;(sharad-message-citation-delete)
          (when (lotus-message-signature-present)
            (message-kill-to-signature)
            (message-remove-signature))
          (insert "\n")
          (insert-reply-object "sharad" "cover" keys nil "txt")
          (goto-char (point-max))
          (insert "\n\n")
          (insert-reply-object "sharad" "resume" keys t "pdf" "Sharad Pratap - Résumé")
          (insert "\n")
          (message-goto-body)
          (sharad-message-citation-delete)
          (xsteve-message-citation)
          (message-add-header (concat "X-rk:" keys)))
        "Not in message.")))

;; (jreply)




(setq gnus-posting-styles

      ;; As you might surmise from this example, this alist consists
      ;; of several styles. Each style will be applicable if the first
      ;; element “matches”, in some form or other. The entire alist
      ;; will be iterated over, from the beginning towards the end,
      ;; and each match will be applied,
      ;; _WHICH_MEANS_THAT_ATTRIBUTES_IN_LATER_STYLES_THAT_MATCH_OVERRIDE_THE_SAME_ATTRIBUTES_IN_EARLIER_MATCHING_STYLES. So
      ;; ‘comp.programming.literate’ will have the ‘Death to
      ;; everybody’ signature and the ‘What me?’ Organization header.

      ;; based on reply article
      `(
        (t                              ;global

         ,@(if (member (system-name) office-host-names)
               `(
                 (name ,myname)
                 (signature "Regards,\n-sharad")
                 (signature-file "~/.setup/osetup/data/emacs.d/gnus.d/message.d/signatures.d/office")
                 ;; ("Jabber-ID" ,office-email)
                 (address ,office-email)
                 ;; ("From" ,office-email)
                 )
               `((name ,myname)
                 (signature "Regards,\n-sharad")
                 ("Jabber-ID" ,jabber-id)
                 (address ,email-addr)
                 ;; ("From" ,email-addr)
                 ))

         ("Posting-style" "t")

         ;; ("nnml:.*"
         ;;  (From (with-current-buffer gnus-article-buffer
         ;;          (message-fetch-field "to"))))

         ;; Note: about Form header it if it is set it override
         ;; `address' header that override user-mail-address, so Form
         ;; > address > user-mail-address

         ;; Rule means that you use the
         ;; To address as the From address in all your outgoing
         ;; replies, which might be handy if you fill many roles. You
         ;; may also use message-alternative-emails instead.

         ;; (From
         ;;  (if (and message-reply-headers
         ;;           (get-buffer gnus-article-buffer)) ; check it if it is current buffer
         ;;      (with-current-buffer gnus-article-buffer
         ;;        (message-fetch-field "to"))))

         ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/Posting-Styles.html
         (From
          (let* ((default-email (concat myname " <" (if (member (system-name) office-host-names) office-email email-addr) ">"))
                 (to (if (get-buffer gnus-article-buffer) ; check it if it is current buffer
                         (with-current-buffer gnus-article-buffer
                           (message-fetch-field "to"))))
                 (email (if to (car (mail-header-parse-address to))))
                 (email-name (if email (assoc email lotus-gnus-name-emails-map))))
            (if email
                (if email-name
                    (concat (cdr email-name) " <" (car email-name) ">")
                    default-email)
                default-email)))


                                        ; try to get only to address, not all in CC Bcc)

         ;; (eval ;; (if (equal (system-name) ,office-host-name)
         ;;  (unless (equal (system-name) ,office-host-name)
         ;;    (progn
         ;;      (set (make-local-variable 'message-send-mail-function) 'message-send-mail-with-sendmail)
         ;;      (set (make-local-variable 'sendmail-program) "/usr/bin/msmtp") ;; we substitute sendmail with msmtp
         ;;      (set (make-local-variable 'message-sendmail-extra-arguments) nil)
         ;;      (set (make-local-variable 'message-sendmail-f-is-evil) t)
         ;;      (set (make-local-variable 'message-sendmail-envelope-from) 'header))))

         )


        (message-news-p
         (name ,myname)
         (signature "Regards,\n-sharad")
         ("Jabber-ID" ,jabber-id)
         ("Posting-style" "message-news-p")

         ;; (address ,email-addr)
         ;; ("From" ,email-addr)

         (From
          (let* ((default-email (concat myname " <" (if (member (system-name) office-host-names) office-email email-addr) ">"))
                 (to (if (get-buffer gnus-article-buffer) ; check it if it is current buffer
                         (with-current-buffer gnus-article-buffer
                           (message-fetch-field "to"))))
                 (email (if to (car (mail-header-parse-address to))))
                 (email-name (if email (assoc email lotus-gnus-name-emails-map))))
            (if email
                (if email-name
                    (concat (cdr email-name) " <" (car email-name) ">")
                  default-email)
              default-email)))

         (address
          (let* ((default-email (concat myname " <" (if (member (system-name) office-host-names) office-email email-addr) ">"))
                 (to (if (get-buffer gnus-article-buffer) ; check it if it is current buffer
                         (with-current-buffer gnus-article-buffer
                           (message-fetch-field "to"))))
                 (email (if to (car (mail-header-parse-address to))))
                 (email-name (if email (assoc email lotus-gnus-name-emails-map))))
            (if email
                (if email-name
                    (concat (cdr email-name) " <" (car email-name) ">")
                  default-email)
              default-email)))

         (eval
          (progn
            (set (make-local-variable 'gnus-message-archive-group)
                 '(,(format-time-string "sent.%Y-%m")
                   "sent"
                   "sent-news"))
            (set (make-local-variable 'message-citation-line-function) 'message-insert-formatted-citation-line)
            (set (make-local-variable 'message-cite-reply-above) nil)
            (set (make-local-variable 'message-cite-reply-position) 'traditional))))


        (message-mail-p
         ;; message is mail and this is not my system taj then do not save Gcc copy in sent-mail
         ("Posting-style" "message-mail-p")
         (From
          (let* ((default-email (concat myname " <" (if (member (system-name) office-host-names) office-email email-addr) ">"))
                 (to (if (get-buffer gnus-article-buffer) ; check it if it is current buffer
                         (with-current-buffer gnus-article-buffer
                           (message-fetch-field "to"))))
                 (email (if to (car (mail-header-parse-address to))))
                 (email-name (if email (assoc email lotus-gnus-name-emails-map))))
            (if email
                (if email-name
                    (concat (cdr email-name) " <" (car email-name) ">")
                  default-email)
              default-email)))

         (address
          (let* ((default-email (concat myname " <" (if (member (system-name) office-host-names) office-email email-addr) ">"))
                 (to (if (get-buffer gnus-article-buffer) ; check it if it is current buffer
                         (with-current-buffer gnus-article-buffer
                           (message-fetch-field "to"))))
                 (email (if to (car (mail-header-parse-address to))))
                 (email-name (if email (assoc email lotus-gnus-name-emails-map))))
            (if email
                (if email-name
                    (concat (cdr email-name) " <" (car email-name) ">")
                  default-email)
              default-email)))

         (eval (unless (equal (system-name) "taj")
                 (set (make-local-variable 'gnus-message-archive-group)
                      '("sent"
                        "sent-mail"
                        ,(format-time-string "sent.%Y-%m")
                        ,@(if (member (system-name) office-host-names)
                              '("Office.Meru.Sent Items" "Office.Fortinet.Sent Items")))))))

        ("Gmail.*"
         (name ,myname)
         (signature "Regards,\n-sharad")
         ;; (address ,email-addr)
         ("Posting-style" "Gmail.*")
         )

        ;; ("Gmail.official"
        ;;  (address "Sharad Pratap <sharad@pratap.net.in>"))

        ("Office.*"
         (name ,myname)
         (signature "Regards,\n-sharad")
         (signature-file "~/.setup/osetup/data/emacs.d/gnus.d/message.d/signatures.d/office")
         (address ,office-fortinet-email)
         ("From" ,office-fortinet-email)
         ("Posting-style" "Office.*")
         (eval (set (make-local-variable 'gnus-message-archive-group)
                    '(,(format-time-string "sent.%Y-%m")
                      "sent"
                      "sent-mail"
                      "Office.Fortinet.Sent Items"
                      "Office.Meru.Sent Items"))))

        ("Office.Fortinet.*\\|nnvirtual:Inbox-Sent\\|nnvirtual:Incoming"
         (name ,myname)
         (signature "Regards,\n-sharad")
         (signature-file "~/.setup/osetup/data/emacs.d/gnus.d/message.d/signatures.d/office")
         (address ,office-fortinet-email)
         ("From" ,office-fortinet-email)
         ("Posting-style" "Office.Fortinet.*")
         (eval (set (make-local-variable 'gnus-message-archive-group)
                    '(,(format-time-string "sent.%Y-%m")
                      "sent"
                      "sent-mail"
                      "Office.Fortinet.Sent Items"))))

        ("Office.Meru.*"
         (name ,myname)
         (signature "Regards,\n-sharad")
         (signature-file "~/.setup/osetup/data/emacs.d/gnus.d/message.d/signatures.d/office")
         (address ,office-meru-email)
         ("From" ,office-meru-email)
         ("Posting-style" "Office.Meru.*")
         (eval (set (make-local-variable 'gnus-message-archive-group)
                    '(,(format-time-string "sent.%Y-%m")
                      "sent"
                      "sent-mail"
                      "Office.Meru.Sent Items"))))

        ;; J sites
        ((header "Received monster.co.in\\|naukri.com") ;reply
         (signature nil)
         ("Posting-style" "(header \"Received\" \"monster.co.in\\|naukri.com\")")
         (eval (progn
                 ;; (set (make-local-variable 'message-cite-function) 'sc-cite-original)
                 ;; (set (make-local-variable 'message-cite-reply-above) t)
                 (set (make-local-variable 'message-citation-line-function) 'message-insert-formatted-citation-line)
                 (set (make-local-variable 'message-cite-reply-above) t)
                 (set (make-local-variable 'message-cite-reply-position) 'above)
                 (remove-hook 'message-setup-hook 'xsteve-message-citation t)
                 ;; (add-hook 'gnus-message-setup-hook 'jreply nil t)
                 (remove-hook (make-local-variable 'message-setup-hook) 'xsteve-message-citation)
                 (add-hook (make-local-variable 'gnus-message-setup-hook) 'jreply nil t)
                 ))
         ;; (xsteve-message-citation)))
         ;; (body :file "~/Documents/Template/j/reply")
         ;; (body jreply)
         ;; (signature (concat "Regards,\n" ,myname))
         ;; (eval (add-hook 'message-setup-hook 'xsteve-message-citation t t)) ;; set in global hook
         ;; (eval (add-hook 'message-signature-setup-hook 'xsteve-message-citation nil t))
         ;; (eval (set (make-local-variable 'message-cite-function) 'sc-cite-original))
         (x-url ,myurl))

        ((save-excursion
           (let ((article-buf
                  (car (remove-if-not
                        '(lambda (bn)
                          (string-match "*Article" bn 0))
                        (mapcar 'buffer-name (buffer-list))))))
             (when article-buf
               (set-buffer article-buf)
               (> (count-lines (point-min) (point-max)) 30))))
         (eval
          (progn
            (set (make-local-variable 'message-cite-reply-above) t)
            (set (make-local-variable 'message-cite-reply-position) 'above))))


        ;; (".*"
        ;;  (From
        ;;   (with-current-buffer gnus-article-buffer
        ;;     (message-fetch-field "to")))











        ))
;; {{ http://www.gnu.org/software/emacs/manual/html_node/gnus/Delayed-Articles.html
;; for delayed reply
(gnus-delay-initialize)
;; }}

;;}} For SMTP msmtp

;; {{ Uninteractive sending
;; gnus-summary-resend-message @ gnus-msg.el
(defun gnus-mail-send-uninteractive (address n)
  "Resend the current article to ADDRESS."
  (interactive
   (list (message-read-from-minibuffer
	  "Resend message(s) to: "
	  (when (and gnus-summary-resend-default-address
		     (gnus-buffer-live-p gnus-original-article-buffer))
	    ;; If some other article is currently selected, the
	    ;; initial-contents is wrong. Whatever, it is just the
	    ;; initial-contents.
	    (with-current-buffer gnus-original-article-buffer
	      (nnmail-fetch-field "to"))))
	 current-prefix-arg))
  (let ((message-header-setup-hook (copy-sequence message-header-setup-hook))
        (message-sent-hook (copy-sequence message-sent-hook)))
    ;; `gnus-summary-resend-message-insert-gcc' must run last.
    (add-hook 'message-header-setup-hook
	      'gnus-summary-resend-message-insert-gcc t)
    (add-hook 'message-sent-hook
	      `(lambda ()
           (let ((rfc2047-encode-encoded-words nil))
             ,(if gnus-agent
                  '(gnus-agent-possibly-do-gcc)
                '(gnus-inews-do-gcc)))))
    (dolist (article (gnus-summary-work-articles n))
      (gnus-summary-select-article nil nil nil article)
      (with-current-buffer gnus-original-article-buffer
	(let ((gnus-gcc-externalize-attachments nil)
	      (message-inhibit-body-encoding t))
	  (message-resend address)))
      (gnus-summary-mark-article-as-forwarded article))))
;; }}


;; {{
;; message-completion-alist
;; (("^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . notmuch-address-expand-name)
;;  ("^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):" . message-expand-group)
;;  ("^\\(Resent-\\)?\\(To\\|B?Cc\\):" . message-expand-name)
;;  ("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . message-expand-name)
;;  ("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):" . message-expand-name))
;; }}

(provide 'message-config)
;;; message-config.el ends here
