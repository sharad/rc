;;; packages.el --- lotus-mailnews layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-mailnews-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-mailnews/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-mailnews/pre-init-PACKAGE' and/or
;;   `lotus-mailnews/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-mailnews-packages
  '(
    (gnus :location local)
    bbdb
    lsdb
    shimbun
    notmuch
    gnus-win
    gnus-sum
    gnus-msg
    gnus-pers
    gnus-namazu
    gnus-demon
    gnus-dired
    message
    sendmail
    dbus
    mailcrypt
    )
  "The list of Lisp packages required by the lotus-mailnews layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun lotus-mailnews/pre-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (progn
      (progn
        (gnus-delay-initialize))
      (progn
        (setq gnus-select-method '(nntp "news.gmane.org"))

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
              "Gmail\\.INBOX\\|Gmail\\.sent-mail"))))
      (progn
        (setq gnus-init-file "~/.gnus.el")
        (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
        (setq
         gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
        (setq
         gnus-directory      (concat gnus-home-directory "News/"))
        (setq
         nndraft-directory (concat gnus-directory "drafts/")))))



  (progn                                ;unconditionally
    (progn
      (progn
        (setq gnus-init-file "~/.gnus.el"))
      (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
      (setq
       gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
      (setq
       gnus-directory      (concat gnus-home-directory "News/"))
      (setq
       nndraft-directory (concat gnus-directory "drafts/")))))

(defun lotus-mailnews/post-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (progn
      (progn
        (progn
          (setq gnus-init-file "~/.gnus.el"))
        (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
        (setq
         gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
        (setq
         gnus-directory      (nnheader-concat gnus-home-directory "News/"))
        (setq
         nndraft-directory  (nnheader-concat gnus-directory "drafts/")))
      (progn
        (add-hook
         'gnus-article-prepare-hook
         'gnus-treat-mail-gravatar)))))

(defun lotus-mailnews/post-init-bbdb ()
  (use-package bbdb
    :defer t
    :config
    (progn
      (progn
        (unless (fboundp 'bbdb-insinuate-gnus)
          (defun bbdb-insinuate-gnus ()
            (bbdb-initialize 'gnus)))
        (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

      (progn
        (setq bbdb-file (expand-file-name "bbdb/bbdb" "~/.emacs.d/.cache/autoconfig"))

        (defun bbdb/gnus-pop-up-bbdb-buffer-for-some-time ()
          (if (functionp 'bbdb/gnus-pop-up-bbdb-buffer)
              (progn
                (bbdb/gnus-pop-up-bbdb-buffer)
                ;; (with-selected-window (get-buffer-window gnus-article-buffer)
                ;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
                (let ((win-bbdb (get-buffer-window "*BBDB*")))
                  (when win-bbdb
                    ;; (run-at-time "4 sec" nil #'delete-window w))))
                    (run-at-time "4 sec" nil #'(lambda (w)
                                                 (if (and
                                                      (windowp w)
                                                      (window-valid-p w))
                                                     ;; (old-delete-window w)
                                                     (progn
                                                       (delete-window w)
                                                       (message "deleted %s window" w))))
                                 win-bbdb))))
            (message "#'bbdb/gnus-pop-up-bbdb-buffer is not present in bbdb3 use some other function for popup.")))

        (define-key gnus-summary-mode-map (kbd "s-c s-v")  'bbdb/gnus-pop-up-bbdb-buffer)

        (setq bbdb-use-pop-up t
              bbdb-save-db-timeout 0) ;; I want it
        (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
        (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer-for-some-time)

        (defun toggle-bbdb-use-pop-up ()
          (interactive)
          (setq
           bbdb-use-pop-up (not bbdb-use-pop-up)))))))

(defun lotus-mailnews/post-init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (setq lsdb-file (lotus-cache-file "lsdb/lsdb"))))))

(defun lotus-mailnews/init-shimbun ()
  (use-package sb-rss-blogs ;; shimbun
    :defer t
    :config
    (progn
      (progn
        (add-to-list 'shimbun-rss-blogs-group-url-regexp
                     '("OSNews" "http://www.osnews.com/files/recent.xml"))
        (add-to-list 'shimbun-rss-blogs-group-url-regexp
                     '("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux"))

        (add-to-list 'shimbun-rss-blogs-group-url-regexp
                     '("NDTV" "http://feeds2.feedburner.com/NdtvNews-TopStories")))
      (progn
        (setq shimbun-atom-hash-group-path-alist
              '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
                ("OSNews" "http://www.osnews.com/files/recent.xml" t)
                ("PlanetEmacsen" "http://planet.emacsen.org/atom.xml" t)
                ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t))

              shimbun-rss-hash-group-path-alist
              '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
                ("OSNews" "http://www.osnews.com/files/recent.xml" t)
                ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t)))))))

(defun lotus-mailnews/init-notmuch ()
  (use-package notmuch-address
    :defer t
    :config
    (progn
      (progn
        (setq notmuch-fcc-dirs nil))
      (progn
        ;; http://notmuchmail.org/emacstips/
        (setq notmuch-address-command (expand-file-name "notmuch-addrlookup" "~/bin"))
        (if (file-exists-p notmuch-address-command)
            (notmuch-address-message-insinuate))))))

(defun lotus-mailnews/init-gnus-win ()
  (use-package gnus-win
    :defer t
    :config
    (progn               ;Toggle Article Window
      ;; from http://www.emacswiki.org/emacs/GnusAndPine
      ;; from http://www.emacswiki.org/emacs/GnusAndPine#toc4
      ;;{{ http://cvlab.epfl.ch/~tola/files/code/dotgnus
      ;; ;; layout of the gnus layout display ; 3-pane format
      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (vertical 25
                               (group 1.0))
                     (vertical 1.0
                               (summary 0.25 point)
                               (article 1.0)))))

      (gnus-add-configuration
       '(summary
         (horizontal 1.0
                     (vertical 25
                               (group 1.0))
                     (vertical 1.0
                               (summary 1.0 point)))))

      ;; Add info configuration also for function `gnus-info-find-node'

      ;; gnus-buffer-configuration

      ;;}}

      (defun toggle-article-window ()
        (interactive)
        (let
            ((article-buffer (car
                              (remove-if-not '(lambda (bn)
                                                (string-match "*Article" bn 0)) (mapcar #'buffer-name (buffer-list))))))
          (if (and article-buffer
                   (get-buffer-window article-buffer nil))
              (gnus-configure-windows 'summary 'force)
            (gnus-configure-windows 'article 'force)))))))

(defun lotus-mailnews/init-gnus-sum ()
  (use-package gnus-sum
    :defer t
    :config
    (progn
      (progn
        (setq
         gnus-extra-headers          '(To Newsgroups Content-Type Date)
         gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*"))
      (progn
        ;;{{ http://eschulte.github.com/emacs-starter-kit/starter-kit-gnus.html
        (when window-system
          (setq
           gnus-sum-thread-tree-indent "  "
           gnus-sum-thread-tree-root "● "
           gnus-sum-thread-tree-false-root "◯ "
           gnus-sum-thread-tree-single-indent "◎ "
           gnus-sum-thread-tree-vertical        "│"
           gnus-sum-thread-tree-leaf-with-other "├─► "
           gnus-sum-thread-tree-single-leaf     "╰─► ")))
      (progn
        (setq gnus-user-date-format-alist
              '(((gnus-seconds-today) . " %k:%M") ;dans la journée = 14:39
                ((+ 86400 (gnus-seconds-today)) . "hier %k:%M")
                                        ;hier = hier 14:39
                ((+ 604800 (gnus-seconds-today)) . "%a %k:%M")
                                        ;dans la semaine = sam 14:39
                ((gnus-seconds-month) . "%a %d") ;ce mois = sam 28
                ((gnus-seconds-year) . "%b %d") ;durant l'année = mai 28
                (t . "%b %d '%y")))))))

(defun lotus-mailnews/init-gnus-msg ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        )
      (progn
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
                         (address ,office-email))
                     ;; ("From" ,office-email)

                     `((name ,myname)
                       (signature "Regards,\n-sharad")
                       ("Jabber-ID" ,jabber-id)
                       (address ,email-addr)))
                 ;; ("From" ,email-addr)


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
                      default-email))))


                                        ; try to get only to address, not all in CC Bcc)

                ;; (eval ;; (if (equal (system-name) ,office-host-name)
                ;;  (unless (equal (system-name) ,office-host-name)
                ;;    (progn
                ;;      (set (make-local-variable 'message-send-mail-function) 'message-send-mail-with-sendmail)
                ;;      (set (make-local-variable 'sendmail-program) "/usr/bin/msmtp") ;; we substitute sendmail with msmtp
                ;;      (set (make-local-variable 'message-sendmail-extra-arguments) nil)
                ;;      (set (make-local-variable 'message-sendmail-f-is-evil) t)
                ;;      (set (make-local-variable 'message-sendmail-envelope-from) 'header))))

                


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
                 ("Posting-style" "Gmail.*"))

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
                         (add-hook (make-local-variable 'gnus-message-setup-hook) 'jreply nil t)))
                 
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
                    (set (make-local-variable 'message-cite-reply-position) 'above))))))


        ;; (".*"
        ;;  (From
        ;;   (with-current-buffer gnus-article-buffer
        ;;     (message-fetch-field "to")))

        ))))

(defun lotus-mailnews/init-gnus-art ()
  (use-package gnus-art
    :defer t
    :config
    (progn
      (progn
        (add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation))
      (progn
        (setq gnus-signature-separator
              '("^-- $"
                "^-- *$"
                "^-------*$"
                "^ *--------*$"
                "^________*$"
                "^========*$")))
      (progn
        (gnus-start-date-timer))
      (progn
        (setq gnus-article-date-lapsed-new-header t)
        (add-hook 'gnus-part-display-hook 'gnus-article-date-lapsed)
        (add-hook 'gnus-part-display-hook 'gnus-article-date-local)

        (add-hook 'gnus-article-prepare-hook
                  '(lambda ()
                     ;; 	     (gnus-article-de-quoted-unreadable)
                     (gnus-article-emphasize)
                     (gnus-article-hide-boring-headers)
                     (gnus-article-hide-headers-if-wanted)
                     ;; 	     (gnus-article-hide-pgp)
                     (gnus-article-highlight)
                     (gnus-article-highlight-citation)
                     (gnus-article-date-lapsed)
                     (gnus-article-date-local)              ; will actually convert timestamp from other timezones to yours
                     (gnus-article-strip-trailing-space))))
      (progn
        (setq
         gnus-treat-body-boundary 'head
         gnus-treat-date-lapsed 'head
         gnus-treat-display-x-face 'head
         gnus-treat-strip-cr 2
         gnus-treat-strip-leading-blank-lines t
         gnus-treat-strip-multiple-blank-lines t
         gnus-treat-strip-trailing-blank-lines t
         gnus-treat-unsplit-urls t

         gnus-treat-date-english 'head
         gnus-treat-date-iso8601 'head
         gnus-treat-date-lapsed 'head
         gnus-treat-date-local 'head
         gnus-treat-date-original 'head
         gnus-treat-date-user-defined 'head
         gnus-treat-date-ut 'head
         gnus-treat-date-original 'head
         ;; Make sure Gnus doesn't display smiley graphics.
         gnus-treat-display-smileys t
         gnus-treat-hide-boring-headers 'head
         gnus-treat-hide-signature nil ;; (unless (equal (system-name) office-host-name) 'last)
         gnus-treat-strip-banner t))
      (progn
        (setq gnus-article-x-face-command
              ;; http://git.gnus.org/cgit/gnus.git/plain/lisp/gnus-art.el?h=V5-8&id=9e60844ade6660e25359aefaf313daf3e92ff3a9
              ;; should be 'gnus-display-x-face-in-from else it will popup image outside
              (if (featurep 'xemacs)
                  (if (or (gnus-image-type-available-p 'xface)
                          (gnus-image-type-available-p 'pbm))
                      'gnus-display-x-face-in-from
                    "{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | ee -")
                (if (gnus-image-type-available-p 'pbm)
                    'gnus-display-x-face-in-from
                  "{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | display -"))))
      (progn
        (setq gnus-visible-headers
              '(
                "^Cc:"
                "^Date:"
                "^Followup-To:"
                "^From:"
                "^Keywords:"
                "^Newsgroups:"
                "^Mailing-List:"
                "^Organization:"
                "^Posted-To:"
                "^Reply-To:"
                "^Subject:"
                "^Summary:"
                "^To:"
                "^X-Newsreader:"
                "^X-Url:"
                "^X-bugzilla"  ; Show all X-headers
                ;; for attachment
                "^Content-Type"
                "^X-Face:"
                "^X-Face")

              gnus-sorted-header-list
              '("^From:"
                "^Subject:"
                "^Summary:"
                "^Keywords:"
                "^Newsgroups:"
                "^Followup-To:"
                "^To:"
                "^Cc:"
                "^Date:"
                "^Organization:"))))))

(defun lotus-mailnews/init-nnmail ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (setq
         nnmail-extra-headers '(To Newsgroups Content-Type Date))))))

(defun lotus-mailnews/init-gnus-pers ()
  (use-package gnus-pers
    :defer t
    :config
    (progn
      (progn
        (defvar gnus-personality-activate nil "")
        (gnus-personality-init/sharad)))))

(defun lotus-mailnews/init-gnus-namazu ()
  (use-package gnus-namazu
    :defer t
    :config
    (progn
      (progn
        (gnus-namazu-insinuate)

        (setq gnus-namazu-index-update-interval nil)

        (defun xsteve-gnus-namazu-update-all-indices ()
          (interactive)
          (gnus-namazu-update-all-indices t))

        (defun xsteve-gnus-update-namazu-index ()
          (run-at-time "6:00am" nil 'xsteve-gnus-namazu-update-all-indices)))
      (progn
        (use-package midnight
          :defer t
          :config
          (progn
            (add-hook 'midnight-hook 'xsteve-gnus-update-namazu-index)))))))

(defun lotus-mailnews/init-gnus-dired ()
  (use-package gnus-dired
    :defer t
    :config
    (progn
      (progn
        (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))))



(defun lotus-mailnews/init-gnus-daemon ()
  (use-package gnus-daemon
    :defer t
    :config
    (progn
      (setq gnus-use-demon t)
      (add-hook 'gnus-group-mode-hook 'gnus-demon-init)
      (add-hook 'gnus-exit-gnus-hook 'gnus-demon-cancel)
      ;; Sort threads by the date of the root node.
      (setq gnus-thread-sort-functions `(gnus-thread-sort-by-date))
      ;; Initialize the Gnus daemon, check new mail every six minutes.
      ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news 1 nil))
      ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 2 nil)
      (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 10 10)
      (gnus-demon-add-handler '(lambda () (gnus-demon-scan-mail-and-news-now 6)) 22 20))))

(defun lotus-mailnews/init-message ()
  (use-package message
    :defer t
    :config
    (progn
      (progn
        (setq
         gnus-message-archive-method '(nnimap "localhost")
         gnus-message-archive-group        ;even I have handled it in gnus-posting-style
         #'(lambda (group)
             (append
              (list "sent")
              (if (message-news-p)
                  '("sent-news")
                `("sent-mail"
                  ,@(if (member (system-name) office-host-names)
                        '("Office.Meru.Sent Items" "Office.Fortinet.Sent Items"))))
              (list (format-time-string "sent.%Y-%m"))))))
      (progn
        (setq message-alternative-emails
              (regexp-opt
               (list
                email-addr
                office-email
                office-fortinet-email
                office-meru-email)))
        (setq message-dont-reply-to-names
              (append
               (list
                user-mail-address
                email-addr
                office-email
                office-fortinet-email
                office-meru-email)
               user-other-email-addresses)))
      (progn
        (setq gnus-gcc-externalize-attachments nil)
        (add-hook 'message-send-hook 'check-attachments-attached))
      (progn
        ;;{{ XSteve, insert Hi Hello Name

        (setq message-citation-line-function 'message-insert-formatted-citation-line
              message-cite-function 'message-cite-original-without-signature)
        (add-hook 'message-setup-hook 'xsteve-message-citation t)
        ;;

        (defun sharad-message-citation-delete ()
          "Delete Hi."
          (message-goto-body)
          (search-forward-regexp "Hi")
          (move-beginning-of-line 1)
          (if (looking-at "Hi")
              (kill-line))))
      (progn
        (define-key message-mode-map [f6] 'xsteve-message-citation-toggle))
      (progn
        (add-hook 'message-sent-hook 'gnus-score-followup-article)
        (add-hook 'message-sent-hook 'gnus-score-followup-thread))

      (progn
        (setq message-cite-reply-above nil
              message-cite-reply-above t
              ;; http://emacsworld.blogspot.in/2011/11/gnus-tip-customising-position-of-point.html
              message-cite-reply-position 'traditional
              message-cite-reply-position 'above))

      (progn
        (use-package ispell
          :defer t
          :config
          (progn
            ;; Ispell.el assumes you use ispell, if you choose aspell say
            ;; (setq ispell-program-name "timeout -k 12 10 aspell")
            (setq ispell-program-name "aspell")
            ;; in your Emacs configuration file.  If you want your outgoing
            ;; messages to be spell-checked, say
            (add-hook 'message-send-hook 'ispell-message)
            ;; In your ~/.gnus.el, if you prefer on-the-fly spell-checking say
            (add-hook 'message-mode-hook (lambda () (flyspell-mode 1))))))

      (progn
        (use-package footnote
          :defer t
          :config
          (progn
            (add-hook 'message-mode-hook (lambda () (footnote-mode 1))))))
      (progn
        (add-hook 'message-mode-hook ;          'turn-on-auto-fill)
                  '(lambda ()
                     (turn-on-auto-fill)
                     (setq fill-column 70)))
        (setq message-generate-headers-first t)
        (setq message-kill-buffer-on-exit t)
        ;;{{ http://www.gnus.org/manual/gnus_401.html
        (setq gnus-confirm-mail-reply-to-news t)
        (setq message-user-fqdn (concat "personal.machine.of." myshortname ".com")))
      (progn
        (defvar *use-msmtp-for-senmail* nil "msmtp to use")

        (setq *use-msmtp-for-senmail* (equal (system-name) "asfsdspratap"))

        (if *use-msmtp-for-senmail* ;; where I am using msmtp
            (setq  ;; for msmtp
             ;; see http://www.gnus.org/manual/message_36.html
             message-sendmail-f-is-evil t
             message-sendmail-envelope-from nil)
          (setq
           ;; see http://www.gnus.org/manual/message_36.html
           message-sendmail-f-is-evil nil
           message-sendmail-envelope-from 'header))))))

(defun lotus-mailnews/init-sendmail ()
  (use-package sendmail
    :defer t
    :config
    (progn
      (progn
        (setq mail-specify-envelope-from t
              mail-envelope-from 'header))
      (progn
        (setq send-mail-function 'sendmail-send-it)))))

(defun lotus-mailnews/init-dbus ()
  (use-package dbus
    :defer t
    :config
    (progn
      (defvar gnus-nm-dbus-registration nil "dsfdsf")
      (defvar gnus-nm-connected-hook nil
        "Functions to run when network is connected.")
      (defvar gnus-nm-disconnected-hook nil
        "Functions to run when network is disconnected.")
      (setq gnus-nm-dbus-registration nil)

      ;; Add hooks for plugging/unplugging on network state change:
      (add-hook 'gnus-nm-connected-hook    'gnus-nm-agent-plug)
      (add-hook 'gnus-nm-connected-hook    'gnus-group-send-queue)
      (add-hook 'gnus-nm-disconnected-hook 'gnus-nm-agent-unplug)
      ;; Add hooks for enabling/disabling integration on startup/shutdown:
      (add-hook 'gnus-started-hook   'gnus-nm-enable)
      (add-hook 'gnus-exit-gnus-hook 'gnus-nm-disable))))

(defun lotus-mailnews/init-mailcrypt ()
  (use-package mailcrypt
    :defer t
    :commands (mc-install-read-mode mc-install-write-mode)
    :config
    (progn
      ;; http://www.suse.de/~garloff/Writings/mutt_gpg/node18.html
      (mc-setversion "gpg")
      (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
      (add-hook 'message-mode-hook 'mc-install-write-mode)
      (add-hook 'news-reply-mode-hook 'mc-install-write-mode)

      ;; Next time you start Gnus, you get a menu called ¨Mailcrypt¨ which
      ;; gives you an easy access to GnuPG.

      ;; Some variable settings which might be helpful are:

      ;; Use the pgp2 compatibility wrapper
      (setq mc-gpg-path "/usr/bin/gpg-compat")

      ;; If you have more than one key, specify the one to use
      (setq mc-gpg-user-id "0x12345678")

      ;; Always sign encrypted messages
      (setq mc-pgp-always-sign t)

      ;; How long should mailcrypt remember your passphrase
      (setq mc-passwd-timeout 600)

      ;; To sign automatically every message you send, you need to add some
      ;; lisp code. After adding the following lines to your .emacs file,
      ;; (X)Emacs will ask you if the message is to be signed before sending
      ;; it.

      ;; (add-hook 'message-send-hook 'my-sign-message)
      (defun my-sign-message ()
        (if (yes-or-no-p "Sign message? ")
            (mc-sign-message))))))


;;; packages.el ends here
