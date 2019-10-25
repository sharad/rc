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
    lsdb
    gnus-win
    gnus-sum
    gnus-msg
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
        (progn
          (setq gnus-init-file "~/.gnus.el"))
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

(defun lotus-mailnews/post-init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (setq lsdb-file (lotus-cache-file "lsdb/lsdb"))))))

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
  (use-package lsdb
    :defer t
    :config
    (progn
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

;;; packages.el ends here
