;; My .gnus explained
;; I subscribed a bunch of mailing lists via news.gmane.org.
;; I use the gmane server as my primary news source.


;; http://www.delorie.com/gnu/docs/emacs/gnus_316.html Q1.11
;; (require 'tm-setup)
;; (require 'gnus)
;; (require 'mime-compose)

(setq gnus-select-method '(nntp "news.gmane.org"))

;; stolen from:
;; http://linil.wordpress.com/2008/01/18/gnus-gmail
(setq
 gnus-invalid-group-regexp "[:`'\"]\\|^$"
 ;; gnus-group-sort-function gnus-thread-sort-functions
 )

(add-to-list
 'gnus-secondary-select-methods
 '(nnimap "localhost"
   (nnimap-address "localhost")
   ;; (nnimap-server-port 993)
   ;; (nnimap-server-port 443)
   (nnimap-server-port 143)
   (nnimap-stream network)
   (nnimap-authenticator login)
   (nnimap-authinfo-file "~/.authinfo.gpg")
   (nnir-search-engin imap)))

(add-to-list
 'gnus-secondary-select-methods
 `(nnvirtual
   ,(if (equal (system-name) office-host-name)
        "Office\\.INBOX\\|Office\\.sent-mail"
        "Gmail\\.INBOX\\|Gmail\\.sent-mail")))

(setq gnus-permanently-visible-groups ".*INBOX")

        ;; "^nnimap+localhost:Office\\.INBOX\\|^nnimap+localhost:Office\\.sent-mail$"
        ;; "^nnimap+localhost:Gmail\\.INBOX\\|^nnimap+localhost:Gmail\\.sent-mail$")))

;;{{ http://eschulte.github.com/emacs-starter-kit/starter-kit-gnus.html
;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq
   gnus-sum-thread-tree-indent "  "
   gnus-sum-thread-tree-root "● "
   gnus-sum-thread-tree-false-root "◯ "
   gnus-sum-thread-tree-single-indent "◎ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "╰─► "))


;;{{ Group setting
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;}}

;;{{ make it working only for Outlook Office
(setq gnus-message-archive-method
      '(nnimap "localhost"
        (nnimap-address "localhost")
        ;; (nnimap-server-port 993)
        ;; (nnimap-server-port 443)
        (nnimap-server-port 143)
        ;; (nnimap-stream ssl)
        (nnimap-authinfo-file "~/.authinfo.gpg")))

;; (setq gnus-message-archive-group        ;even I have handled it in gnus-posting-style
;;       `((if (message-news-p)
;;             "sent-news"
;;             ,(if (equal (system-name) office-host-name)
;;                  "Office.Sent Items"
;;                  "sent"))))

(setq gnus-message-archive-group        ;even I have handled it in gnus-posting-style
      (if (message-news-p)
          "sent-news"
          (if (equal (system-name) office-host-name)
              "Office.Sent Items"
              "sent")))


;; http://www.gnus.org/manual/gnus_153.html
(setq gnus-gcc-mark-as-read t)
;    If non-nil, automatically mark Gcc articles as read.

(setq gnus-gcc-externalize-attachments 'all)
;    If nil, attach files as normal parts in Gcc copies; if a regexp
;    and matches the Gcc group name, attach files as external parts;
;    if it is all, attach local files as external parts; if it is
;    other non-nil, the behavior is the same as all, but it may be
;    changed in the future.
;;}}


;; ;; ;; (add-to-list 'gnus-secondary-select-methods
;; ;; ;;              '(nntp "gnu"))

;; ;; ;; Set the prefix when using jump to select a newsgroup.
;; ;; ;; needs a newer gnus
;; ;; ;; (setq gnus-group-jump-to-group-prompt '((0 . "nnml:mail.") (1 .  "gmane.")
;; ;; ;;                                         (2 . "nnshimbun+")
;; ;; ;;                                         (3 .  "nnfolder+archive:")))
;; ;; ;; (setq gnus-group-jump-to-group-prompt "nnimap:gmail.")

;; ;; ;; (setq
;; ;; ;;  gnus-select-method '(nntp "us.usenet-news.net")
;; ;; ;;  message-send-mail-function 'smtpmail-send-it
;; ;; ;;  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; ;; ;;  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "sh4r4d@gmail.com" nil))
;; ;; ;;  smtpmail-default-smtp-server "smtp.gmail.com"
;; ;; ;;  smtpmail-smtp-server "smtp.gmail.com"
;; ;; ;;  smtpmail-smtp-service 587
;; ;; ;;  smtpmail-local-domain "taj")

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




;; Increase the score for followups to a sent article.
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)


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


;; Integration to bbdb and dired
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)


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


;; ;; ;; ;; Setup the search via gnus-namazu. First create the index via the command line.

;; ;; ;; ;; # generate the database: look at gnus-directory, mine is "~/gnus"
;; ;; ;; ;; # ~/gnus/nnml-mail contains the mails
;; ;; ;; ;; mkdir ~/gnus/namazu
;; ;; ;; ;; mknmz -a -h -O ~/gnus/namazu ~/gnus/nnml-mail


;; ;; ;; ;; Enable gnus-namazu. You can start a search vie C-c C-n.

;; ;; ;; (require 'gnus-namazu)
;; ;; ;; (gnus-namazu-insinuate)
;; ;; ;; (setq gnus-namazu-index-update-interval nil)
;; ;; ;; ;; call explicitely M-x gnus-namazu-update-all-indices


;; ;; ;; ;; Update the namazu index every day at 6:00am
;; ;; ;; (defun xsteve-gnus-namazu-update-all-indices ()
;; ;; ;;   (interactive)
;; ;; ;;   (gnus-namazu-update-all-indices t))

;; ;; ;; (defun xsteve-gnus-update-namazu-index ()
;; ;; ;;   (run-at-time "6:00am" nil 'xsteve-gnus-namazu-update-all-indices))

;; ;; ;; (require 'midnight)
;; ;; ;; (add-hook 'midnight-hook 'xsteve-gnus-update-namazu-index)



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
;;           ;; (ssl "sh4r4d@gmail.com" "smtp.gmail.com" 465 "key" nil)
;;           (ssl "sh4r4d@gmail.com" "smtp.gmail.com" 587 "key" nil)
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




;;{{ http://www.gnus.org/manual/gnus_401.html
(when (xrequire 'ispell)
  ;; Ispell.el assumes you use ispell, if you choose aspell say
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


;;{{Exiting http://www.stanford.edu/~rgm/comp/dotgnus.html
(setq gnus-interactive-exit t)
;; Quit active Gnus if exiting Emacs.
;; Note that an abortive exit will kill Gnus. Win some, lose some.
;; Now fixed by having Emacs prompt for confirmation before hook runs.
(defun my-gnus-kill-on-exit-emacs-fn ()
  "Kill Gnus when exiting Emacs. Added to `my-before-kill-emacs-hook'."
  (setq gnus-interactive-exit nil)
  (gnus-group-exit))

(add-hook 'my-before-kill-emacs-hook 'my-gnus-kill-on-exit-emacs-fn)


(defun my-gnus-after-exiting-gnus-hook-fn ()
  "Function added to `gnus-after-exiting-gnus-hook'."
  (remove-hook 'my-before-kill-emacs-hook 'gnus-group-exit)
  (mapcar (lambda (buff)
            (and (get-buffer buff) (kill-buffer buff)))
          '("bbdb" "*BBDB*" "*Compile-Log*" "posts"))
;;;   (let ((gnus-startup-jingle
;;;          (expand-file-name
;;;           "Library/WindowMaker/Sounds/Windows/chimes.wav"
;;;           (or (getenv "GNUSTEP_USER_ROOT") "~/GNUstep"))))
;;;     (gnus-play-jingle))
  )

(add-hook 'gnus-after-exiting-gnus-hook 'my-gnus-after-exiting-gnus-hook-fn)
;;}}




;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; General speedups.

(setq gnus-read-active-file nil
      gnus-check-new-newsgroups nil ; 'ask-server
      gnus-nov-is-evil nil
      gnus-save-newsrc-file t
      message-from-style nil) ;;'angles)

; (setq user-full-name "Chris Ball")

;; Using %G (default is %g) avoids the nnfoo:bar+ prefix
;; (setq gnus-group-line-format "%M\%S\%p\%5y: %G\n")


;; I keep hitting "b" by mistake in the group view, and it messes things up.
(define-key gnus-group-mode-map "b" 'gnus-group-get-new-news)

;; Use a second connection to grab the next article when I read one, so
;; I don't have to wait for it be downloaded.
(setq gnus-asynchronous t)



(defun gnus-demon-scan-mail-or-news-and-update ()
  "Scan for new mail/news and update the *Group* buffer"
  (when (gnus-alive-p)
    (save-window-excursion
      (save-excursion
 (set-buffer gnus-group-buffer)
 (gnus-group-get-new-news)))))

(defun gnus-demon-scan-and-update ()
  (gnus-demon-scan-mail-or-news-and-update))


;;}}

;;{{
(setq
 ;see http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC13

 ;;  1.9 Auto Save

 ;; Whenever you do something that changes the Gnus data (reading
 ;; articles, catching up, killing/subscribing groups), the change is
 ;; added to a special dribble buffer. This buffer is auto-saved the
 ;; normal Emacs way. If your Emacs should crash before you have saved
 ;; the `.newsrc' files, all changes you have made can be recovered
 ;; from this file.

 ;; If Gnus detects this file at startup, it will ask the user whether
 ;; to read it. The auto save file is deleted whenever the real
 ;; startup file is saved.

 ;; If gnus-use-dribble-file is nil, Gnus won't create and maintain a
 ;; dribble buffer. The default is t.

 ;; Gnus will put the dribble file(s) in gnus-dribble-directory. If
 ;; this variable is nil, which it is by default, Gnus will dribble
 ;; into the directory where the `.newsrc' file is located. (This is
 ;; normally the user's home directory.) The dribble file will get the
 ;; same file permissions as the .newsrc file.

 ;; If gnus-always-read-dribble-file is non-nil, Gnus will read the
 ;; dribble file on startup without querying the user.

 gnus-dribble-directory "~/.gnus-data"
 gnus-always-read-dribble-file t
 ;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC12
 ;; You can turn off writing the `.newsrc' file by setting
 ;; gnus-save-newsrc-file to nil, which means you can delete the file
 ;; and save some space, as well as exiting from Gnus faster. However,
 ;; this will make it impossible to use other newsreaders than
 ;; Gnus. But hey, who would want to, right?
 gnus-save-newsrc-file nil)
;;}}

;;{{
;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC41
;;  2.17.3 Group Timestamp
;; It can be convenient to let Gnus keep track of when you last read a group. To set the ball rolling, you should add gnus-group-set-timestamp to gnus-select-group-hook:

;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; After doing this, each time you enter a group, it'll be recorded.
;; This information can be displayed in various ways--the easiest is to use the `%d' spec in the group line format:

;; (setq gnus-group-line-format
;;       "%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")

;; This will result in lines looking like:

;; *        0: mail.ding                                19961002T012943
;;          0: custom                                   19961002T012713

;; As you can see, the date is displayed in compact ISO 8601 format. This may be a bit too much, so to just display the date, you could say something like:

;; (setq gnus-group-line-format
;;       "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n")

(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      ;"%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")
      "%M\%S\%p\%P\%5y: %(%-100,100g%) %6,6~(cut 2)d\n")

;;}}


;;{{Face http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC20
;; 2.1.3 Group Highlighting

;; Highlighting in the group buffer is controlled by the
;; gnus-group-highlight variable. This is an alist with elements that
;; look like (form . face). If form evaluates to something non-nil,
;; the face will be used on the line.

;; Here's an example value for this variable that might look nice if the background is dark:



(face-spec-set 'my-group-face-1
               '((t (:foreground "Red" :bold t))))
(face-spec-set 'my-group-face-2
               '((t (:foreground "SeaGreen" :bold t))))
(face-spec-set 'my-group-face-3
               '((t (:foreground "SpringGreen" :bold t))))
(face-spec-set 'my-group-face-4
               '((t (:foreground "SteelBlue" :bold t))))
(face-spec-set 'my-group-face-5
               '((t (:foreground "SkyBlue" :bold t))))

(setq gnus-group-highlight
      '(((> unread 200) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))

;; Also see section 8.6 Faces and Fonts.

;; Variables that are dynamically bound when the forms are evaluated include:

;; group
;;     The group name.
;; unread
;;     The number of unread articles in the group.
;; method
;;     The select method.
;; mailp
;;     Whether the group is a mail group.
;; level
;;     The level of the group.
;; score
;;     The score of the group.
;; ticked
;;     The number of ticked articles in the group.
;; total
;;     The total number of articles in the group. Or rather, MAX-NUMBER minus MIN-NUMBER plus one.
;; topic
;;     When using the topic minor mode, this variable is bound to the current topic being inserted.

;; When the forms are evaled, point is at the beginning of the line of
;; the group in question, so you can use many of the normal Gnus
;; functions for snarfing info on the group.

;; gnus-group-update-hook is called when a group line is changed. It
;; will not be called when gnus-visual is nil. This hook calls
;; gnus-group-highlight-line by default.

;;}}


;;{{[GENERAL]
;; http://www.chemie.fu-berlin.de/chemnet/use/info/gnus/gnus_3.html#SEC18
(setq
 ;If non-nil, the startup message won't be displayed. That way, your
 ;boss might not notice that you are reading news instead of doing
 ;your job.
 gnus-inhibit-startup-message t)
 ;Message displayed by Gnus when no groups are available.
 ;gnus-no-groups-message "No Gnus is good news"
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

;;{{
(setq gnus-local-domain
      (or (getenv "DOMAINNAME") office-fqdn))
;;}}





;;{{ Programming
;; (save-restriction
;;   (message-narrow-to-headers)
;;   (let ((email (message-fetch-field "to")))
;;     (replace-regexp-in-string s (concat s "@sdfsdgfsdg.com" s))))

;  (replace-regexp-in-string "[ \t]+$" "" s))
;;}}


;;{{

;; (when (file-directory-p *gnus-session-file-dir*)
;;   (byte-recompile-directory *gnus-session-file-dir* 0)
;;   (mapc '(lambda (file)
;;           (user-require
;;            (intern (file-name-sans-extension
;;                     (file-name-nondirectory file)))))
;;         (directory-files *gnus-session-file-dir* t "^[a-zA-Z0-9-]+\.elc$")))

(require-dir-libs "~/\.xemacs/pkgrepos/mypkgs/gnus-session-start")

;;}}

;; (mapcar '(lambda (file)
;;         (message (file-name-sans-extension
;;                     (file-name-nondirectory file))))
;;       (directory-files *gnus-session-file-dir* t "^[a-zA-Z0-9-]+\.elc$"))


;; ("attachment" "bindings" "color" "date" "face" "gnus" "group" "mailto" "misc" "nm" "notification" "pers" "reply" "search" "summary" "zzee")



;; (directory-files *gnus-session-file-dir* t "^[a-zA-Z0-9-]+\.elc$")

(setq gnus-registry-install t
      nnmail-crosspost t
      gnus-agent nil)

