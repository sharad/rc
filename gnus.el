;; My .gnus explained

;;{{

;; ;; ;; Keep a backup of the received mails for 60 days and delete that
;; ;; ;; mails after 60 days without a confirmation.
;; ;; ;; (setq mail-source-delete-incoming 60)
;; ;; ;; (setq mail-source-delete-old-incoming-confirm nil)

;; ;; ;; Do not use the html part of a message, use the text part if possible!
;; ;; (setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; ;; ;; Mails and News that you send are stored in the folders "sent-mail"
;; ;; ;; or "sent-news"
;; ;; (setq gnus-message-archive-group
;; ;;       '((if (message-news-p)
;; ;;             "sent-news"
;; ;;           "sent-mail")))

;; ;; ;; The scoring system sorts articles and authors you read often to the beginning of the available mails.
;; ;; ;; Less interesting stuff is located at the end.
;; ;; (setq gnus-use-adaptive-scoring t)
;; ;; (setq gnus-score-expiry-days 14)
;; ;; (setq gnus-default-adaptive-score-alist
;; ;;   '((gnus-unread-mark)
;; ;;     (gnus-ticked-mark (from 4))
;; ;;     (gnus-dormant-mark (from 5))
;; ;;     (gnus-saved-mark (from 20) (subject 5))
;; ;;     (gnus-del-mark (from -2) (subject -5))
;; ;;     (gnus-read-mark (from 2) (subject 1))
;; ;;     (gnus-killed-mark (from 0) (subject -3))))
;; ;;     ;(gnus-killed-mark (from -1) (subject -3))))
;; ;;     ;(gnus-kill-file-mark (from -9999)))
;; ;;     ;(gnus-expirable-mark (from -1) (subject -1))
;; ;;     ;(gnus-ancient-mark (subject -1))
;; ;;     ;(gnus-low-score-mark (subject -1))
;; ;;     ;(gnus-catchup-mark (subject -1))))

;; ;; (setq gnus-score-decay-constant 1) ;default = 3
;; ;; (setq gnus-score-decay-scale 0.03) ;default = 0.05

;; ;; (setq gnus-decay-scores t) ;(gnus-decay-score 1000)


;; ;; ;; Use a global score file to filter gmane spam articles. That is a
;; ;; ;; really cool feature.
;; ;; (setq gnus-global-score-files
;; ;;        '("~/gnus/scores/all.SCORE"))

;; ;; ;; all.SCORE contains:
;; ;; ;;(("xref"
;; ;; ;;  ("gmane.spam.detected" -1000 nil s)))
;; ;; (setq gnus-summary-expunge-below -999)


;; ;; ;; ;; Summary line format strings
;; ;; ;; (setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
;; ;; ;; (setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")


;; ;; ;; ;; Threading visual appearance
;; ;; ;; (setq gnus-summary-same-subject "")
;; ;; ;; (setq gnus-sum-thread-tree-root "")
;; ;; ;; (setq gnus-sum-thread-tree-single-indent "")
;; ;; ;; (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
;; ;; ;; (setq gnus-sum-thread-tree-vertical "|")
;; ;; ;; (setq gnus-sum-thread-tree-single-leaf "`-> ")





;; ;; ;; ;; Toggle the Gcc Header. The Gcc Header specifies a local mail box
;; ;; ;; ;; that receives a copy of the sent article.
;; ;; ;; (defun message-toggle-gcc ()
;; ;; ;;   "Insert or remove the \"Gcc\" header."
;; ;; ;;   (interactive)
;; ;; ;;   (save-excursion
;; ;; ;;     (save-restriction
;; ;; ;;       (message-narrow-to-headers)
;; ;; ;;       (if (message-fetch-field "Gcc")
;; ;; ;;           (message-remove-header "Gcc")
;; ;; ;;         (gnus-inews-insert-archive-gcc)))))
;; ;; ;; (define-key message-mode-map [(control ?c) (control ?f) (control ?g)] 'message-toggle-gcc)


;; ;; ;; ;; Bind M-h to a function that shows the latest received mails.
;; ;; ;; (defun xsteve-show-nnmail-split-history ()
;; ;; ;;   (interactive)
;; ;; ;;   (let ((hi (sort (mapcar 'caar nnmail-split-history) 'string<))
;; ;; ;;         (elem)
;; ;; ;;         (count)
;; ;; ;;         (total))
;; ;; ;;     (while hi
;; ;; ;;       (if (string= elem (car hi))
;; ;; ;;           (setq count (+ count 1))
;; ;; ;;         (setq elem (car hi))
;; ;; ;;         (when total
;; ;; ;;           (setcar total (concat (car total) ": " (number-to-string count))))
;; ;; ;;         (setq count 1)
;; ;; ;;         (add-to-list 'total elem))
;; ;; ;;       (setq hi (cdr hi)))
;; ;; ;;     (if total
;; ;; ;;         (setcar total (concat (car total) ": " (number-to-string count)))
;; ;; ;;       (setq total '("No new Mail")))
;; ;; ;;     (message (format"%s%s" xsteve-check-mail-time (nreverse total)))))

;; ;; ;; (defun xsteve-get-new-news-set-time ()
;; ;; ;;   (setq xsteve-check-mail-time (format-time-string "[%H:%M] ")))

;; ;; ;; (unless (boundp 'xsteve-check-mail-time)
;; ;; ;;   (xsteve-get-new-news-set-time))

;; ;; ;; (add-hook 'gnus-get-new-news-hook 'xsteve-get-new-news-set-time)

;; ;; ;; (define-key gnus-group-mode-map [(meta h)] 'xsteve-show-nnmail-split-history)


;; ;; ;; Store gnus specific files to ~/gnus
;; ;; (setq gnus-directory "~/gnus")
;; ;; (setq message-directory "~/gnus/mail")
;; ;; (setq nnml-directory "~/gnus/nnml-mail")
;; ;; (setq gnus-article-save-directory "~/gnus/saved")
;; ;; (setq gnus-kill-files-directory "~/gnus/scores")
;; ;; (setq gnus-cache-directory "~/gnus/cache")



;; ;; ;; Use the gnus registry
;; ;; (require 'gnus-registry)
;; ;; (gnus-registry-initialize)

;; ;; ;; Select the header that should be shown. Yes I am interested in the
;; ;; ;; used mail or news client from other people ;-)
;; ;; (setq gnus-visible-headers
;; ;;       "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")



;; ;; ;; Use the topic mode
;; ;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


;; ;; ;; Added some keybindings to the gnus summary mode
;; ;; (define-key gnus-summary-mode-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
;; ;; (define-key gnus-summary-mode-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
;; ;; (define-key gnus-summary-mode-map [(control down)] 'gnus-summary-next-thread)
;; ;; (define-key gnus-summary-mode-map [(control up)] 'gnus-summary-prev-thread)


;; ;; ;; I use gnus-alias to make it possible to use different mail
;; ;; ;; addresses for me. I have changed the mail addresses below to some
;; ;; ;; invalid ones. I can use C-c C-p to select an identity from the
;; ;; ;; given list. It is also possible via gnus-alias-identity-rules to
;; ;; ;; select the correct mail address from a given context.
;; ;; ;; (require 'gnus-alias)
;; ;; ;; (setq gnus-alias-identity-alist
;; ;; ;;       '(("riic-xsteve" "" "Stefan ReichÃ¶r " "" nil "" "")
;; ;; ;;         ;;("web.de" "" "Stefan ReichÃ¶r " "" nil "" "Stefan.")
;; ;; ;;         ("sigriic" "" "Stefan ReichÃ¶r " "" nil "" "~/data/.signature-riic")))

;; ;; ;; (setq gnus-alias-identity-rules '(("xtla.el"
;; ;; ;;                                    ("to" "xtla-el-devNOSPAM@gna.org" current)
;; ;; ;;                                    "xsteve")))

;; ;; ;; (setq gnus-alias-default-identity "xsteve")
;; ;; ;; (gnus-alias-init)
;; ;; ;; (define-key message-mode-map "\C-c\C-p" 'gnus-alias-select-identity)


;; ;; ;; My spam settings. I use the spam processing for the gmane groups.
;; ;; (if (require 'spam)
;; ;;     (setq spam-directory "~/gnus/spam/"
;; ;;           gnus-spam-process-newsgroups
;; ;;           '(("^gmane\\."
;; ;;              ((spam spam-use-gmane))))))


;; ;; ;; ;; Make it easier to find ham messages in my spam folder nnml:spam.
;; ;; ;; ;; The following setup highlights some words that I expect in ham messages.
;; ;; ;; (setq gnus-summary-nospam-highlight-list '("[PATCH]" "svn" "x-dict" "pwsafe"
;; ;; ;;                                            "emacs" "python"))
;; ;; ;; (defun gnus-summary-hl-nospam ()
;; ;; ;;   (interactive)
;; ;; ;;   (highlight-regexp (regexp-opt gnus-summary-nospam-highlight-list)))

;; ;; ;; (defun gnus-summary-hl-nospam-in-spam-group ()
;; ;; ;;   (when (string= gnus-newsgroup-name "nnml:spam")
;; ;; ;;     (message "Highlighting words for possible ham mails")
;; ;; ;;     (gnus-summary-hl-nospam)))
;; ;; ;; (add-hook 'gnus-summary-prepare-hook 'gnus-summary-hl-nospam-in-spam-group)





;;{{ sendmail
;; ;; SENDING MAIL
;; ;; Here is the code found on my .gnus file:
;;
;; (if (xrequire 'smtpmail) ;; Default smtpmail.el configurations.
;;     (progn
;;
;;       ;; Available SMTP accounts.
;;       (defvar smtp-accounts
;;         '(
;;           ;; (ssl "sh4r4d _at_ _G-mail_" "smtp.gmail.com" 465 "key" nil)
;;           ;; (ssl "sh4r4d _at_ _G-mail_" "smtp.gmail.com" 465 "key" nil)
;;           (ssl "sh4r4d _at_ _G-mail_" "smtp.gmail.com" 587 "key" nil)
;;           (ssl "sh4r4d _at_ _G-mail_" "smtp.gmail.com" 587 "key" nil)
;;           ;; (ssl "mymail@otherserver.com" "smtp.otherserver.com" 25 "key" nil)
;;           ))
;;
;;       ;; This lists my SMTP accounts, one line for each server, as you can
;;       ;; see both the servers are using SSL.
;;
;;       (setq send-mail-function 'smtpmail-send-it
;;                  message-send-mail-function 'smtpmail-send-it
;;                  mail-from-style nil
;;                  user-full-name "Sharad Pratap"
;;                  user-mail-address email-addr
;;                  message-signature-file "~/.emacs-etc/signature"
;;                  smtpmail-debug-info t
;;                  smtpmail-debug-verb t)
;;
;;            ;; The code above just sets some default values. My mail signature is
;;            ;; found on the file ~/emacs/signature, I also specify my user name
;;            ;; user-full-name and my e-mail address user-mail-address so that Gnus
;;            ;; can fill the From header field automatically for me.
;;
;;            ;; The Debug options is also nice so that you get some feedback about
;;            ;; what is happening while Gnus is sending the e-mail for you.
;;
;;            (defun set-smtp-plain (server port)
;;              "Set related SMTP variables for supplied parameters."
;;              (setq smtpmail-smtp-server server
;;                    smtpmail-smtp-service port
;;                    smtpmail-auth-credentials "~/.authinfo.gpg"
;;                    smtpmail-starttls-credentials nil)
;;              (message "Setting SMTP server to `%s:%s %s'."
;;                       server port address))
;;
;;            ;; Those are two functions used to send mail, one with and one without
;;            ;; SSL support. Note that smtpmail-auth-credentials is telling Gnus
;;            ;; where to find the username and password.
;;
;;            (defun change-smtp ()
;;              "Change the SMTP server according to the current from line."
;;              (save-excursion
;;                (loop with from = (save-restriction
;;                                    (message-narrow-to-headers)
;;                                    (message-fetch-field "from"))
;;                      for (acc-type address . auth-spec) in smtp-accounts
;;                      when (string-match address from)
;;                      do (cond
;;                          ((eql acc-type 'plain)
;;                           (return (apply 'set-smtp-plain auth-spec)))
;;                          ((eql acc-type 'ssl)
;;                           (return (apply 'set-smtp-ssl auth-spec)))
;;                          (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
;;                      finally (error "Cannot interfere SMTP information."))))

;;            (defun set-smtp-ssl (server port key cert)
;;              "Set related SMTP and SSL variables for supplied parameters."
;;              (setq starttls-use-gnutls t
;;                    starttls-gnutls-program "gnutls-cli"
;;                    starttls-extra-arguments nil
;;                    smtpmail-smtp-server server
;;                    smtpmail-smtp-service port
;;                    smtpmail-starttls-credentials (list (list server port key cert))
;;                    smtpmail-auth-credentials "~/.authinfo.gpg")
;;              (message
;;               "Setting SMTP server to `%s:%s %s'. (SSL enabled.)"
;;               server port address))

;;            (add-hook 'message-send-hook 'change-smtp)

;;            ;; Finally we create a hook for when me message is going to be
;;            ;; sent. This hook basically tries to match the From header with the
;;            ;; e-mail address you specified at smtp-account variable and chooses
;;            ;; the server accordingly.
;;            ))
;;}}





;; ;;  |  ;; GNUS
;; ;;  |  (setq gnus-select-method '(nntp "news.ne.mediaone.net"))
;; ;;  |  (setq gnus-button-url 'shell-execute-url)
;; ;;  |  ;(if (string= "win32" window-system)
;; ;;  |  ;    (setq browse-url-netscape-program "c:\\bin\\netscape.bat")
;; ;;  |  ;  (setq browse-url-netscape-program "netscape")
;; ;;  |  ;  )
;; ;;  |  (setq gnus-check-new-newsgroups 'ask-server)
;; ;;  |  (setq gnus-read-active-file 'some)
;; ;;  |  (setq gnus-thread-sort-functions
;; ;;  |        '(gnus-thread-sort-by-number
;; ;;  |  	gnus-thread-sort-by-subject
;; ;;  |  	gnus-thread-sort-by-score))
;; ;;  |  (setq gnus-asynchronous t)
;; ;;  |  (setq gnus-use-cache 'passive)
;; ;;  |  (setq gnus-use-long-file-name t)
;; ;;  |  (setq gnus-default-article-save 'gnus-summary-save-in-file) ; no encoding
;; ;;  |  (setq gnus-interactive-catchup nil)
;; ;;  |  (setq gnus-interactive-exit nil)
;; ;;  |  (setq gnus-novice-user nil)
;; ;;  |  (setq gnus-carpal-mode-hook t)
;; ;;  |  ;(setq gnus-use-nocem t)
;; ;;  |  ;(setq gnus-nocem-expiry-wait 7)
;; ;;  |  (add-hook 'nntp-server-opened-hook 'harald-send-nntp-authinfo)
;; ;;  |  (defvar harald-nntp-authinfo-file "~/news.password")
;; ;;  |  (defun harald-send-nntp-authinfo ()
;; ;;  |    (if (file-exists-p harald-nntp-authinfo-file)
;; ;;  |        (save-excursion
;; ;;  |  	(set-buffer (get-buffer-create " *nntp-authinfo*"))
;; ;;  |  	(buffer-disable-undo (current-buffer))
;; ;;  |  	(erase-buffer)
;; ;;  |  	(insert-file-contents harald-nntp-authinfo-file)
;; ;;  |  	(goto-char (point-min))
;; ;;  |  	(and (search-forward nntp-address nil t)
;; ;;  |  	     (re-search-forward
;; ;;  |  	      "login[ \t]+\\(\\S +\\)\\s +password[ \t]+\\(\\S +\\)"
;; ;;  |  	      nil t)
;; ;;  |  	     (let ((login (match-string 1))
;; ;;  |  		   (passwd (match-string 2)))
;; ;;  |  	       (nntp-send-command "^.*\r?\n" "AUTHINFO USER" login)
;; ;;  |  	       (nntp-send-command "^.*\r?\n" "AUTHINFO PASS" passwd)))
;; ;;  |  	(kill-buffer (current-buffer))
;; ;;  |  	)))




;;}}




;;{{[ARTICLE]
;http://www.chemie.fu-berlin.de/chemnet/use/info/gnus/gnus_6.html#SEC107

;;The gnus-article-display-hook is called after the article has been
;;inserted into the article buffer. It is meant to handle all
;;treatment of the article before it is displayed. By default it
;;contains gnus-article-hide-headers, which hides unwanted headers.


;; Article Keymap
;; A few additional keystrokes are available:
;; SPACE
;;     Scroll forwards one page (gnus-article-next-page).
;; DEL
;;     Scroll backwards one page (gnus-article-prev-page).
;; C-c ^
;;     If point is in the neighborhood of a Message-Id and you press
;;     r, Gnus will try to get that article from the server
;;     (gnus-article-refer-article).
;; C-c C-m
;;     Send a reply to the address near point (gnus-article-mail). If
;;     given a prefix, include the mail.
;; s
;;     Reconfigure the buffers so that the summary buffer becomes
;;     visible (gnus-article-show-summary).
;; ?
;;     Give a very brief description of the available keystrokes
;;     (gnus-article-describe-briefly).

;;}}







;;{{ Programming
;; (save-restriction
;;   (message-narrow-to-headers)
;;   (let ((email (message-fetch-field "to")))
;;     (replace-regexp-in-string s (concat s "@sdfsdgfsdg.com" s))))

;  (replace-regexp-in-string "[ \t]+$" "" s))
;;}}


;;{{

(require-dir-libs "~/\.xemacs/pkgrepos/mypkgs/gnus-session-start")

;;}}



;; ("attachment" "bindings" "color" "date" "face" "gnus" "group" "mailto" "misc" "nm" "notification" "pers" "reply" "search" "summary" "zzee")



;; (directory-files *gnus-session-file-dir* t "^[a-zA-Z0-9-]+\.elc$")
