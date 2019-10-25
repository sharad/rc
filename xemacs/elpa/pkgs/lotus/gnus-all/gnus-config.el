
;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
            (local-set-key "="  'toggle-article-window)
            ;; (local-set-key "n"  'gnus-summary-next-article)
            ;; (local-set-key "p"  'gnus-summary-prev-article)
            ;; (local-set-key "!"  'gnus-summary-put-mark-as-ticked-next)
            ;; (local-set-key "d"  'gnus-summary-put-mark-as-expirable-next)
            ;; (local-set-key "u"  'gnus-summary-clear-mark-forward)
            ;; (local-set-key "r"  'gnus-summary-dwim-reply)
            ;; (local-set-key "R"  'gnus-summary-dwim-reply-with-original)
            ;; ;; creating real problem
            ;; ;; (local-set-key "x"  'gnus-summary-delete-article)
            ;; (local-set-key "g"  'gnus-summary-goto-group)
            ;; (local-set-key "?"  'gnus-info-find-node)
            ;; (local-set-key "l"  'gnus-summary-exit)
            ;; (local-set-key "s"  'gnus-summary-save-and-expire)
            ;; (local-set-key "v"  'gnus-article-view-part)
            ;; (local-set-key "c"  'gnus-summary-mail-other-window)
            ;; (local-set-key "$f" 'gnus-summary-sort-by-author)
            ;; (local-set-key "$a" 'gnus-summary-sort-by-original)
            ;; (local-set-key "$d" 'gnus-summary-sort-by-date)
            ;; (local-set-key "$s" 'gnus-summary-sort-by-subject)
            ;; (local-set-key "$z" 'gnus-summary-sort-by-chars)
            ;; (local-set-key "$e" 'gnus-summary-sort-by-score)
            (if (gnus-news-group-p gnus-newsgroup-name)
                (local-set-key "f"  'gnus-summary-followup)
              (local-set-key "f"  'gnus-summary-mail-forward))))

;;}}

(provide 'bindings-config)
;;; citation-config.el --- citation from X Steve

;;; Commentary:

;;

;;; Code:


;;; contacts-config.el ends here








;;; gnus-mesgtmpl-config.el --- sdf



;;; gnus-mesgtmpl-config.el ends here

;;
;;
;;
;;
;;





(make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
(setq
 mail-user-agent 'gnus-user-agent
 gnus-home-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory)
 gnus-startup-file   (expand-file-name ".cache/autoconfig/gnus/newsrc" user-emacs-directory))

(setq
 gnus-directory      (nnheader-concat gnus-home-directory "News/"))
(setq
 nndraft-directory  (nnheader-concat gnus-directory "drafts/"))

(global-set-key-if-unbind (kbd "H-s") 'gnus-group-save-newsrc)

(xrequire 'gnus-eyecandy) ; -enhance the group buffer by adding icons.
(xrequire 'gnus-filterhist) ; -add a buffer which display the message filtering
(xrequire 'gnus-junk) ; -semi-automatic replies to junk e-mails;
(xrequire 'gnus-pers) ; -an alternative to gnus-posting-styles.
(xrequire 'message-x) ; -customizable completion in message headers;
(xrequire 'nnir) ; -searchable mail backend;
(xrequire 'nnnil) ; -empty, read-only backend;
(xrequire 'nntodo) ; -manage to-do items;
(xrequire 'spam-stat) ; -spam-detector based on statistics.

;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus

(setq gnus-read-active-file nil
      gnus-check-new-newsgroups nil ; 'ask-server
      gnus-nov-is-evil nil
      gnus-save-newsrc-file t
      message-from-style nil) ;;'angles)

; (setq user-full-name "Chris Ball")



(setq gnus-asynchronous t)

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

 gnus-dribble-directory (expand-file-name ".cache/autoconfig/gnus/gnus-data" user-emacs-directory)
 gnus-always-read-dribble-file t
 ;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC12
 ;; You can turn off writing the `.newsrc' file by setting
 ;; gnus-save-newsrc-file to nil, which means you can delete the file
 ;; and save some space, as well as exiting from Gnus faster. However,
 ;; this will make it impossible to use other newsreaders than
 ;; Gnus. But hey, who would want to, right?
 gnus-save-newsrc-file nil)
;;}}

;;{{[GENERAL]
(setq
 ;If non-nil, the startup message won't be displayed. That way, your
 ;boss might not notice that you are reading news instead of doing
 ;your job.
 gnus-inhibit-startup-message t)
 ;Message displayed by Gnus when no groups are available.
 ;gnus-no-groups-message "No Gnus is good news"
;;}}



(setq gnus-registry-install t
      nnmail-crosspost t
      gnus-agent nil)

;;{{Exiting http://www.stanford.edu/~rgm/comp/dotgnus.html
(setq gnus-interactive-exit t)
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


;;{{
(setq gnus-local-domain
      (or (getenv "DOMAINNAME") office-fqdn))
;;}}




;;{{ other file

(setq

 ;; message-send-mail-function 'message-send-mail-with-sendmail
 ;; message-sendmail-envelope-from 'header
 ;; message-sendmail-f-is-evil nil

 ;; tls-checktrust 'ask
 ;; tls-program '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
 ;;               "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3"
 ;;               "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof")
 gnus-agent-synchronize-flags nil
 ;; gnus-agent-queue-mail 'always
 ;; gnus-agent-prompt-send-queue t
 ;; gnus-asynchronous t
 ;; gnus-agent-go-online t
 ;; mm-text-html-renderer 'gnus-w3m
 gnus-summary-display-arrow t
 gnus-completing-read-function 'gnus-ido-completing-read
 mail-user-agent 'gnus-user-agent
 read-mail-command 'gnus
 ;;gnus-treat-display-smileys nil
 )
(autoload 'sendmail-send-it "sendmail")


;;}}

(provide 'gnus-misc-config)
;;; gnus-schedule-config.el --- sched






;;; Commentary:

;;

;;; Code:



(deh-require-maybe (and diary-lib mm-decode)
  ;; diary-from-outlook-gnus is an interactive compiled Lisp function in
  ;; `diary-lib.el'.

  ;; (diary-from-outlook-gnus &optional NOCONFIRM)

  ;; Maybe snarf diary entry from Outlook-generated message in Gnus.
  ;; Unless the optional argument NOCONFIRM is non-nil (which is the case when
  ;; this function is called interactively), then if an entry is found the
  ;; user is asked to confirm its addition.
  ;; Add this function to `gnus-article-prepare-hook' to notice appointments
  ;; automatically.

  (require 'mm-decode)
  (require 'mm-util)

  (defun diary-from-outlook-gnus-safe ()
    (ignore-errors
     (diary-from-outlook-gnus)))
  (remove-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus)
  ;; this function `diary-from-outlook-gnus'
  ;; when failed with error "no buffer name with multipart/related"
  ;; it left article in the end, so I have to remove it.
  (add-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus-safe)

  ;; using icalendar.el wotrking

  (deh-require-maybe mm-decode
    (defvar icalendar-outlook-file nil)
    (defun my-save-icalendar (handle)
      (let ((diary icalendar-outlook-file))
        (when (and (equal (car (mm-handle-type handle)) "text/calendar")
                   (gnus-mime-view-part-internally handle)
                   (mm-with-part handle (icalendar-import-buffer diary)))
          (message "Saved calendar entry in %s" diary))))

    (setq gnus-article-mime-part-function 'my-save-icalendar)

    (add-hook
     'gnus-mime-multipart-functions
     '("text/calendar" . my-save-icalendar))))




    (deh-require-maybe (and gnus-sum nntodo todo-gnus)
      ;; http://www.emacswiki.org/emacs/TodoGnus
      (setq nntodo-mbox-file "~/.nntodo")

      ;; Then go into the Server Buffer (with ^) and add there a new Server
      ;; (with a; nntodo as server method) Now go back to the group buffer
      ;; and add your new todo-group (with G m and nntodo as the
      ;; method). You can’t access an empty group, so first you eigther have
      ;; to create a message (C-u a in the group buffer, when over the
      ;; group’s name) or copy/move a message (B c or B m) to the group.

      ;; You maybe don’t want todo groups to be hidden, if there are
      ;; no unread items.

      (setq gnus-permanently-visible-groups "^nntodo+")

      ;; Also it could be usefull to see always all todo items,
      ;; regardless if they are marked as unread or read:

      (setq gnus-parameters
            '(("^nntodo+"
               (display . all)))))







(provide 'gnus-schedule-config)
;;; schedule.el ends here
;;; group-config.el --- GNUS group related behaviours.






;;; Commentary:

;;

;;; Code:

(require 'host-info)
(require 'common-info)
(require 'passwds)

;; gnus-group-sort-function gnus-thread-sort-functions
(setq  gnus-invalid-group-regexp "[:`'\"]\\|^$")

(setq gnus-permanently-visible-groups ".*INBOX")

;; "^nnimap+localhost:Office\\.INBOX\\|^nnimap+localhost:Office\\.sent-mail$"
;; "^nnimap+localhost:Gmail\\.INBOX\\|^nnimap+localhost:Gmail\\.sent-mail$")))


;;{{ Group setting
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;}}

;;{{

(setq gnus-gcc-mark-as-read t)
;    If non-nil, automatically mark Gcc articles as read.

(setq gnus-gcc-externalize-attachments nil)
;    If nil, attach files as normal parts in Gcc copies; if a regexp
;    and matches the Gcc group name, attach files as external parts;
;    if it is all, attach local files as external parts; if it is
;    other non-nil, the behavior is the same as all, but it may be
;    changed in the future.

;;}}

(define-key gnus-group-mode-map "b" 'gnus-group-get-new-news)


;;{{Face http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC20




(when t
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
          (t . my-group-face-5))))

;;}}



;;{{
;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC41

(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      ;"%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")
      "%M\%S\%p\%P\%5y: %(%-100,100g%) %6,6~(cut 2)d\n")

;;}}

;;{{ http://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
(deh-section "GNUS Group Parameters."
  )

(setq nnshimbun-group-parameters-alist
 '(
   ("^nnshimbun.*:" index-range all prefetch-articles off
    encapsulate-images on expiry-wait 6)))

;;}}

(provide 'group-config)
;;; group-config.el ends here

;;; Commentary:

;;

;see Chvmail-user-agent
(setq mail-user-agent 'gnus-user-agent)

(defun johnsu01/mailto (url)
  "Follow a mailto URL as passed from Iceweasel, prompting for a posting style."
  (let ((gnus-newsgroup-name
         (completing-read "Use posting style of group: "
                          gnus-active-hashtb nil
                          (gnus-read-active-file-p))))
    (setq url (url-unhex-string url))
    ;; (browse-url-mail url)
    (gnus-url-mailto url))
  ;; message-mail does not do anything with the body argument, so we have to.
  (if (string-match (regexp-quote "?") url)
      (let* ((start (match-end 0))
             (args (url-parse-query-string
                    (substring url start nil)))
             (body (cadr (assoc-string "body" args t))))
        (when body
          (switch-to-buffer (car (message-buffers)))
          (save-excursion
            (message-goto-body)
            (insert body))))))

(provide 'mailto-config)
;;; message-config.el --- GNUS Message





;;{{ Address Book http://www.emacswiki.org/emacs/ExternalAbook
(when (xrequire 'external-abook)
  (setq external-abook-command "timeout 4 /usr/bin/lbdbq '%s*' | sed 1d | cut -d'	' -f1,2") ;;"contacts -lf '%%e\t%%n' %s")

  (eval-after-load "message"
    '(progn
       (add-to-list 'message-mode-hook
                    '(lambda ()
                       (define-key message-mode-map "\C-c\t" 'external-abook-try-expand))))))
;;}}

;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus

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






(xrequire 'sigbegone)



;;}} For SMTP msmtp




(provide 'message-config)
;;; message-config.el ends here
;;
;;
;;
;;
;;

;;
;;
;;
;;
;;


;;{{ from: http://www.emacswiki.org/emacs/GnusNotify

(defvar gnus-mst-display-new-messages "New Mails" "doc")
(defvar gnus-mst-notify-groups "*" "doc")

(when (xrequire 'gnus-notify)
  (setq gnus-mst-display-new-messages "New mails"
        gnus-mst-notify-groups
        (if (equal (system-name) "spratap")
            '("nnimap+localhost:Office.INBOX" "nnimap+localhost:Office.lists.info.india" "nnimap+localhost:Office.lists.info.india-misc")
          '("nnimap+localhost:nnimap+localhost:Gmail.INBOX"))))

;;}}

;;{{ from: http://www.emacswiki.org/emacs/GnusBiff
(defvar foundnewmbox "")

(defun fmbiff ()
  (interactive)
  (save-excursion
    (with-current-buffer gnus-group-buffer
      ;; (set-buffer "*Group*")
      ; (beginning-of-buffer)
      (goto-char (point-min))
      (defvar foundanymbox nil)
      (cond ((re-search-forward "INBOX.ALL" nil t)
             (setq foundanymbox t))
            (t (setq foundanymbox nil)))
      ;; (set-buffer "*Group*")
      ; (beginning-of-buffer)
      (goto-char (point-min))
      (cond ((re-search-forward "0: INBOX.ALL" nil t)
             (setq foundnewmbox ""))
            (t (if foundanymbox (setq foundnewmbox "[M]")
                   (setq foundnewmbox ""))))
      (message nil))))

(unless (member 'foundnewmbox global-mode-string)
   (setq global-mode-string (append global-mode-string
                                    (list 'foundnewmbox))))

(add-hook 'gnus-after-getting-new-news-hook 'fmbiff)


(defvar mac-biff-lighter ""
  "Lighter used by `mac-biff-mode'.")

(defvar mac-biff-mail-re "\\([[:digit:]]+\\)"
  "Regular expression to match number counts in a Gnus buffer.")

(define-minor-mode mac-biff-mode
  "Minor mode to display state of new email."
  nil mac-biff-lighter nil
  (if mac-biff-mode
      (progn (add-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
             (add-hook 'gnus-exit-group-hook 'mac-biff-update)
             (mac-biff-update))
    (remove-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
    (remove-hook 'gnus-exit-group-hook 'mac-biff-update)))

(defun mac-biff-update1 ()
  "Read the mail count from Gnus."
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward mac-biff-mail-re nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (setq mac-biff-lighter (if (= count 0)
                               ""
                             (format " [%d]" count)))))
;;}}


;;{{ from: http://stackoverflow.com/questions/1053245/new-mail-notifications-in-gnus-for-emacs
(defun mac-biff-update ()
  "Read the mail count from Gnus."
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward mac-biff-mail-re nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (if (> count 0)
        (if (= 0 (shell-command
                                        ;(format "/usr/local/bin/growlnotify -a Emacs.app -m 'You have %d new messages!'" count)))))
                  (format "zenity --question --text 'You have %d new messages!'" count)))
            (make-frame))
      )))




;;}}


;;;
;;; Get messages automaticaly
;;;

;;}}


(provide 'notification-config)
;;; notmuch-config.el --- sadsa

;;}}


(provide 'reply-config)




;;; stat.el --- Statetics function

(defun sdfsdgfdsgdfg-gnus-user-format-function-b (header)
  (let ((descr
         ;; (assq 'x-bugzilla-who (mail-header-extra header))))
         (or
          (gnus-extra-header 'x-bugzilla-who header)
          (gnus-extra-header 'X-Bugzilla-Who header))))
    ;; (if descr (cdr descr) "bugzilla")))
    (if descr descr "bugzilla")))


(defun gnus-user-format-function-atch (header)
  ;; http://osdir.com/ml/emacs.gnus.user/2006-08/msg00011.html
  "Display @ for message with attachment in summary line.

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        indicator)
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    (if indicator
        indicator
      " ")))





(rs-gnus-summary-tree-arrows-01)


(progn ;; deh-require-maybe gnus-summary-stripe
  (setq gnus-summary-stripe-regexp "^.+│.+│.+│"))


(defun my-gnus-summary-line-initialize ()
  "Setup my summary line."
  (interactive)
  ;; Alias for the content-type function:
  (defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)
  ;; Alias for the size function:
  (defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)
  ;; Alias for the score function:
  (defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
  ;;
  (defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
  ;;
  ;; Use them:
  (setq gnus-balloon-face-0 'rs-gnus-balloon-0)
  (setq gnus-balloon-face-1 'rs-gnus-balloon-1)
  ;; Unbold face for UTF arrows: (FIXME: Doesn't work on marauder.)
  (copy-face 'default 'rs-gnus-face-1)
  (setq gnus-face-1 'rs-gnus-face-1)
  ;; (set-face-italic-p 'rs-gnus-face-1 nil)
  ;; (dolist (el '(gnus-summary-low-ancient-face
  ;; 		gnus-summary-low-read-face
  ;; 		gnus-summary-low-ticked-face
  ;; 		gnus-summary-low-undownloaded-face
  ;; 		gnus-summary-low-unread-face))
  ;;   (message "%s" el)
  ;;   (set-face-italic-p el nil)
  ;;   (set-face-bold-p el nil)
  ;;   (sit-for 1))
  (if (or (not window-system)
          (string-match "marauder\\|siogo" system-name))
      (rs-gnus-summary-tree-arrows-latin)
    (rs-gnus-summary-tree-arrows))
  ;; Set line format:
  (setq gnus-summary-line-format
        "%«%U%R%u&score;%u&ct; %4u&size;%»%* %(%-20,20f%) %1«%1{%B %}%s%»\n"))


(setq rs-gnus-summary-line-content-type-alist
      '(("^text/plain"             " ")
        ("^text/html"              "h")
        ("^message/rfc822"         "f") ;; forwarded
        ("^multipart/mixed"        "m")
        ("^multipart/alternative"  "a")
        ("^multipart/related"      "r")
        ("^multipart/signed"       "s")
        ("^multipart/encrypted"    "e")
        ("^multipart/report"       "t")
        ("^application/"           "A")
        ("^image/"                 "I")))

(defvar lotus-gnus/global-summry-line-format   nil "")
(defvar lotus-gnus/bugzilla-summry-line-format nil "")
(defvar lotus-gnus/sent-summry-line-format     nil "")

(defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)
(defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)
(defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
(defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
;;
(setq gnus-balloon-face-0 'rs-gnus-balloon-0
      gnus-balloon-face-1 'rs-gnus-balloon-1
      gnus-face-1         'rs-gnus-face-1)

(copy-face 'default 'rs-gnus-face-1)

(let* (;;(marks "%0{%«%U%R%z %u&score;%u&ct; %4u&size;%»%}")
       ;; (marks "%0«%U%R%z%u&atch;%u&score;%u&ct;%4u&size;%»")
       (marks "%0«%U%R%z%u&atch;%u&score;%u&ct;%4k%»")
       ;; (marks "%0{%U%R%z%}")
       ;; (attachment "%0{%@%}")
       (pipe "%3{│%}")
       ;; (date  (concat pipe "%1{%d%}" pipe))
       (date  (concat pipe "%1{%&user-date;%}" pipe))
       (lines " %1{%-4L%}: ")
       (from "%4{%-20,20f%}")
       (thread-mark "%1{%B%}")
       (subject "%s")
       (sp " ")
       (nl "\n")
                                        ;(bugzilla-who "%4{%-20,20ub%}")
       )
  (setq
   lotus-gnus/global-summry-line-format   (concat marks date lines from sp pipe sp thread-mark subject nl)
   lotus-gnus/bugzilla-summry-line-format (concat marks date lines from sp pipe sp thread-mark subject nl)
   lotus-gnus/sent-summry-line-format     (concat marks date lines from sp pipe sp thread-mark subject nl)))

(setq gnus-parameters
      `(
        (".*"
         (gnus-summary-line-format ,lotus-gnus/global-summry-line-format)
         (gnus-summary-display-arrow t)
         (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
         (gnus-article-sort-functions '(gnus-article-sort-by-date gnus-article-sort-by-score)))
                                        ;"Gnus: %g [%A] %Z"

        ("nnimap.*\\.bugs"
         (gnus-summary-line-format ,lotus-gnus/bugzilla-summry-line-format))

        ("nnimap.*\\.sent-mail\\|.*sent"
         (gnus-summary-line-format ,lotus-gnus/sent-summry-line-format)
         (gnus-summary-display-arrow t)
         (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
                                        ;"Gnus: %g [%A] %Z"
         (gnus-extra-headers '(To Newsgroups X-Newsreader))
         (gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*"))
        ("nnshimbun.*"
         (encapsulate-images t))))


;;; winconfig.el ends here
