
(defun lotus-gnus-start-config ()
  ;;{{ Good :: Excellent beautiful Great!! Thanks XSteve
  ;; Use the keybinding M-F7 to toggle between the gnus window configuration and your normal editing windows.
  (defun xsteve-gnus ()
    (interactive)
    (let ((bufname (buffer-name)))
      (if (or
           (string-equal "*Group*" bufname)
           (string-equal "*BBDB*" bufname)
           (string-match "\*Summary" bufname)
           (string-match "\*Article" bufname))
          (progn
            (xsteve-bury-gnus))
        ;unbury
        (if (get-buffer "*Group*")
            (unless (xsteve-unbury-gnus)
              (gnus-plugged))
          ; (gnus-unplugged))
          ;; (progn
          ;;   (xsteve-unbury-gnus)
          ;;   (if (functionp 'gnus-summary-rescan-group)
          ;;       (gnus-summary-rescan-group)))
          (gnus-plugged)))))
  ;;(gnus-unplugged)))))

  (defun xsteve-unbury-gnus ()
    (interactive)
    (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
      (set-window-configuration gnus-bury-window-configuration)))

  (defun xsteve-bury-gnus ()
    (interactive)
    (setq gnus-bury-window-configuration nil)
    (let ((buf nil)
          (bufname nil))
      (dolist (buf (buffer-list))
        (setq bufname (buffer-name buf))
        (when (or
               (string-equal "*Group*" bufname)
               (string-equal "*BBDB*" bufname)
               (string-match "\*Summary" bufname)
               (string-match "\*Article" bufname))
          (unless gnus-bury-window-configuration
            (setq gnus-bury-window-configuration (current-window-configuration)))
          (delete-other-windows)
          (if (eq (current-buffer) buf)
              (bury-buffer)
            (bury-buffer buf))))))

  ;;}}

  ;;{{

  (when (and (xrequire 'cl)
             (xrequire 'footnote))
    (setq footnote-body-tag-spacing 1))

  ;;}}


  ;;{{ from http://www.emacswiki.org/emacs/GnusNetworkManager

  ;; If you want to start up Gnus in offline or online state depending on
  ;; the current network status, you can add a custom Gnus startup
  ;; function in ~/.emacs, something like this:

  (if (xrequire 'dbus)
      (defun nm-is-connected ()
        (equal 3 (dbus-get-property
                  :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                  "org.freedesktop.NetworkManager" "State")))
    (defun switch-to-or-startup-gnus ()
      "Switch to Gnus group buffer if it exists, otherwise start Gnus in plugged or unplugged state,
depending on network status."
      (interactive)
      (if (or (not (fboundp 'gnus-alive-p))
              (not (gnus-alive-p)))
          (if (nm-is-connected)
              (gnus)
            (gnus-unplugged))
        (switch-to-buffer gnus-group-buffer)
        (delete-other-windows))))


  ;;}}
  )

(defun lotus-gnus-group-config ()
  ;; stolen from:
  ;; http://linil.wordpress.com/2008/01/18/gnus-gmail
  (setq
   gnus-invalid-group-regexp "[:`'\"]\\|^$"
   ;; gnus-group-sort-function gnus-thread-sort-functions
   )


  (setq gnus-permanently-visible-groups ".*INBOX")

  ;; "^nnimap+localhost:Office\\.INBOX\\|^nnimap+localhost:Office\\.sent-mail$"
  ;; "^nnimap+localhost:Gmail\\.INBOX\\|^nnimap+localhost:Gmail\\.sent-mail$")))


  ;;{{ Group setting
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  ;;}}

  ;;{{

  ;; (setq gnus-message-archive-group        ;even I have handled it in gnus-posting-style
  ;;       `((if (message-news-p)
  ;;             "sent-news"
  ;;             ,(if (equal (system-name) office-host-name)
  ;;                  "Office.Sent Items"
  ;;                  "sent"))))

  (setq gnus-message-archive-group        ;even I have handled it in gnus-posting-style
        '("sent"
          (if (message-news-p)
              '("sent-news")
            `("sent-mail"
              ,@(if (member (system-name) office-host-names)
                    '("Office.Meru.Sent Items" "Office.Fortinet.Sent Items"))))
          '(format-time-string "sent.%Y-%m")
          ))


  ;; http://www.gnus.org/manual/gnus_153.html
  (setq gnus-gcc-mark-as-read t)
  ;    If non-nil, automatically mark Gcc articles as read.

  ;; (setq gnus-gcc-externalize-attachments 'all)
  (setq gnus-gcc-externalize-attachments nil)
  ;    If nil, attach files as normal parts in Gcc copies; if a regexp
  ;    and matches the Gcc group name, attach files as external parts;
  ;    if it is all, attach local files as external parts; if it is
  ;    other non-nil, the behavior is the same as all, but it may be
  ;    changed in the future.

  ;;}}

  ;; I keep hitting "b" by mistake in the group view, and it messes things up.
  (define-key gnus-group-mode-map "b" 'gnus-group-get-new-news)


  ;;{{Face http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC20
  ;; 2.1.3 Group Highlighting

  ;; Highlighting in the group buffer is controlled by the
  ;; gnus-group-highlight variable. This is an alist with elements that
  ;; look like (form . face). If form evaluates to something non-nil,
  ;; the face will be used on the line.

  ;; Here's an example value for this variable that might look nice if the background is dark:

  (when (not (fboundp 'dotspacemacs/user-init))

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

  ;;{{ http://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
  (deh-section "GNUS Group Parameters."
    (when (require 'summary-config)
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
               (encapsulate-images t))))))

  (setq nnshimbun-group-parameters-alist
        '(
          ("^nnshimbun.*:" index-range all prefetch-articles off
           encapsulate-images on expiry-wait 6)))

  ;; ("mail\\..*"
  ;;  (gnus-show-threads nil)
  ;;  (gnus-use-scoring nil)
  ;;  (gnus-summary-line-format
  ;;   "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
  ;;  (gcc-self . t)
  ;;  (display . all))

  ;;  ("^nnimap:\\(foo.bar\\)$"
  ;;  (to-group . "\\1"))

  ;; ("mail\\.me"
  ;;  (gnus-use-scoring  t))

  ;; ("list\\..*"
  ;;  (total-expire . t)
  ;;  (broken-reply-to . t))


  ;;}}
  )

(defun lotus-gnus-summary-config ()
  ;; x-bugzilla-target-milestone: 6.1.0.0
  ;; x-bugzilla-url: https://bugzilla/
  ;; x-bugzilla-changed-fields: Status CC
  ;; x-bugzilla-assigned-to:
  ;; x-bugzilla-reason: CC
  ;; auto-submitted: auto-generated
  ;; x-bugzilla-type: changed
  ;; x-bugzilla-watch-reason: None
  ;; x-bugzilla-product:
  ;; x-bugzilla-component:
  ;; x-bugzilla-keywords:
  ;; x-bugzilla-severity: major
  ;; x-bugzilla-who:
  ;; x-bugzilla-status: VERIFIED
  ;; x-bugzilla-priority: P1

  (require 'nnheader)

  ;;{{http://eschulte.github.com/emacs-starter-kit/starter-kit-gnus.html
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

  ;;}}

  (deh-section "summary line format user functions"
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



    (deh-require-maybe rs-gnus-summary
      ;; Setup all:
      ;; (rs-gnus-summary-line-initialize)

      ;; Usage for the format functions:

      (rs-gnus-summary-tree-arrows-01)

      ;; Usage for the balloon face:

      (deh-require-maybe gnus-summary-stripe
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


      ))


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

  (deh-section "summary line formats"



    (defvar lotus-gnus/global-summry-line-format   nil "")
    (defvar lotus-gnus/bugzilla-summry-line-format nil "")
    (defvar lotus-gnus/sent-summry-line-format     nil "")


    ;; Alias for the content-type function:
    (defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)
    ;;   You need to add `Content-Type' to `nnmail-extra-headers' and
    ;; `gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."

    ;; Alias for the size function:
    (defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)
    ;; Alias for the score function:
    (defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
    ;;
    (defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
    ;;
    ;; Use them:
    (setq gnus-balloon-face-0 'rs-gnus-balloon-0
          gnus-balloon-face-1 'rs-gnus-balloon-1
          gnus-face-1 'rs-gnus-face-1)
    ;; Unbold face for UTF arrows: (FIXME: Doesn't work on marauder.)
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
       ; lotus-gnus/bugzilla-summry-line-format (concat attachment marks date lines bugzilla-who sp pipe sp thread-mark subject nl)
       lotus-gnus/bugzilla-summry-line-format (concat marks date lines from sp pipe sp thread-mark subject nl)
       lotus-gnus/sent-summry-line-format     (concat marks date lines from sp pipe sp thread-mark subject nl)))



    ;; With a custom date format :
    ;; affichage de la date en relatif

    ;; gnus-user-date-format-alist '((t . "%Y-%b-%d %H:%M"))

    (setq gnus-user-date-format-alist
          '(((gnus-seconds-today) . " %k:%M") ;dans la journée = 14:39
            ((+ 86400 (gnus-seconds-today)) . "hier %k:%M")
            ;hier = hier 14:39
            ((+ 604800 (gnus-seconds-today)) . "%a %k:%M")
            ;dans la semaine = sam 14:39
            ((gnus-seconds-month) . "%a %d") ;ce mois = sam 28
            ((gnus-seconds-year) . "%b %d") ;durant l'année = mai 28
            (t . "%b %d '%y"))) ;le reste = mai 28 '05

    )

  ;; ;; Specify the order of the header lines
  ;; (setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))





  ;; (concat
  ;;  "%0{%U%R%z%}"
  ;;  "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
  ;;                                         ;" %L: "
  ;;  " %1{%4,-4L%}: "
  ;;                                         ;"  "
  ;;  "%4{%-20,20f%}"               ;; name
  ;;  "  "
  ;;  "%3{│%}"
  ;;  " "
  ;;  "%1{%B%}"
  ;;  "%s\n")
  )

(defun lotus-gnus-article-config ()
  (defun gnus-article-mst-show-country ()
    ;; from http://dishevelled.net/elisp/gnus-mst-show-country.el
    (interactive)
    (let ((from (message-fetch-field "From" t)))
      (when from
        (let ((addr (car (ietf-drums-parse-address from))))
          (when addr
            (let* ((field (progn
                            (string-match "\\.\\(\\sw+\\)$" addr)
                            (match-string 1 addr)))
                   (country (tld-to-country field)))
              (when country
                (save-restriction
                  (article-narrow-to-head)
                  (goto-char (point-max))
                  (insert (propertize (concat "X-Country: " country "\n")
                                      'face 'gnus-header-subject-face))
                  ;; (previous-line 1)
                  (line-move-1 1)
                  (beginning-of-line)))))))))


  ;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
  ;; Show the time since the article came in.
  ;; see http://www.gnu.org/software/emacs/manual/html_node/gnus/Customizing-Articles.html#Customizing-Articles
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
   gnus-treat-strip-banner t)

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
               (gnus-article-strip-trailing-space)
               ;;              (gnus-article-fill-cited-article)
               ;; (with-selected-window (get-buffer-window gnus-article-buffer)
               ;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
               ;; (select-window (get-buffer-window gnus-summary-buffer))
               ))

  (when nil ; not working

    (defadvice gnus-summary-select-article (after goto-article-subject activate)
      (with-selected-window (get-buffer-window gnus-article-buffer)
        (gnus-summary-goto-subject (cdr gnus-article-current))))

    (ad-disable-advice 'gnus-summary-select-article 'after 'goto-article-subject)
    (ad-remove-advice 'gnus-summary-select-article 'after 'goto-article-subject)
    (ad-update 'gnus-summary-select-article)

    (defadvice gnus-summary-scroll-up (after goto-article-subject activate)
      (with-selected-window (get-buffer-window gnus-article-buffer)
        (gnus-summary-goto-subject (cdr gnus-article-current))))

    (ad-disable-advice 'gnus-summary-scroll-up 'after 'goto-article-subject)
    (ad-remove-advice 'gnus-summary-scroll-up 'after 'goto-article-subject)
    (ad-update 'gnus-summary-scroll-up))

  (unless (fboundp 'gnus-article-goto-subject)
    (defun gnus-article-goto-subject ()
      (interactive)
      (with-selected-window
          (get-buffer-window gnus-article-buffer)
        (gnus-summary-goto-subject
         (cdr gnus-article-current)))))


  ;;}}


  ;; gnus-visible-headers

  ;; "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:"


  ;;{{
  (gnus-start-date-timer)

  ;; Start a timer to update the Date headers in the article buffers.
  ;; The numerical prefix says how frequently (in seconds) the function
  ;; is to run.
  ;;}}







  ;;{{
  (defun article-show-attachment (&optional arg)
    "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
    (interactive (gnus-show-attachment-arg))
    (unless (gnus-article-check-hidden-text 'signature arg)
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t))
            (when (gnus-article-narrow-to-signature)
              (gnus-article-hide-text-type
               (point-min) (point-max) 'signature))))))
    (gnus-set-mode-line 'article))

  (defun gnus-show-attachment-arg ()
    'head)

  ;; (defun article-show-attachment (&optional arg)
  (defun article-show-attachment ()
    "Translate article using an online translation service."
    ;; (interactive (gnus-show-attachment-arg))
    (interactive)
    (gnus-with-article-buffer
     (when (article-goto-body)
       (let* ((start (point))
              (end (point-max))
              (orig (buffer-substring start end)))
         ;; (trans (babel-as-string orig)))

         (insert "\nArticle has attachment\n")
         ;; (save-restriction
         ;;   (narrow-to-region start end)
         ;;   (delete-region start end)
         ;;   (insert trans))
         ))))


  ;;}}
  )

(defun lotus-gnu-attachment-config ()


  ;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
  ;; Avoid "Here's an attachment oops I forget to attach it augh" embarrassment.
  ;; Taken from <http://ww.telent.net/diary/2003/1/>.
  (defun check-attachments-attached ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (
             ;; Nil when message came from outside (eg calling emacs as editor)
             ;; Non-nil marker of end of headers.
             (internal-messagep
              (re-search-forward
               (concat "^" (regexp-quote mail-header-separator) "$") nil t))
             (end-of-headers              ; Start of body.
              (copy-marker
               (or internal-messagep
                   (re-search-forward "^$" nil t)
                   (point-min))))
             (limit
              (or (re-search-forward "^-- $" nil t)
                  (point-max)))
             (old-case-fold-search case-fold-search))
        (unwind-protect
            (progn
              (goto-char end-of-headers)
              (when (search-forward "attach" limit t)
                (goto-char end-of-headers)
                ;; the word 'attach' has been used, can we find an
                ;; attachment?
                (unless
                    (or (re-search-forward "^<#/" limit t)
                        (re-search-forward "^<#/" nil t)
                        (y-or-n-p
                         "Found the word `attach' but no MIME attachment: send anyway? "
                         )
                        (error "Aborted send")))))
          (set-marker end-of-headers nil)))))

  (add-hook 'message-send-hook 'check-attachments-attached)

  ;;}}

  ;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine
  (require 'message)

  (defvar my-message-attachment-regexp
    "attach\\|\Wfiles?\W\\|enclose\\|\Wdraft\\|\Wversion")

  (defun check-mail ()
    "ask for confirmation before sending a mail. Scan for possible attachment"
    (require 'message)
    (save-excursion
      (message-goto-body)
      (let ((warning ""))
        (when (and (search-forward-regexp my-message-attachment-regexp nil t nil)
                   (not (search-forward "<#part" nil t nil)))
          (setq warning "No attachment.\n"))
        (goto-char (point-min))
        (unless (message-y-or-n-p (concat warning "Send the message ? ") nil nil)
          (error "Message not sent")))))
  (add-hook 'message-send-hook 'check-mail)






  (defun message-attach-all-files-from-folder(&optional disposition dir-to-attach)
    ;; from: http://www.emacswiki.org/emacs/MessageMode#toc5
    "create the mml code to attach all files found in a given directory"
    (interactive)

    (if (eq disposition nil)
        (setq disposition (completing-read "Enter default disposition to use: " '(("attachment" 1) ("inline" 2)) nil t)))

    (if (eq dir-to-attach nil)
        (setq dir-to-attach (read-directory-name "Select a folder to attach: ")))

    (if (not (string-match "/$" dir-to-attach))
        (setq dir-to-attach (concat dir-to-attach "/")))

    (dolist (file (directory-files dir-to-attach))
      (when (and (not (string= "." file)) (not (string= ".." file)))
        (let (full-file-path mime-type)
          (setq full-file-path (concat dir-to-attach file))
          (if (file-readable-p full-file-path)
              (if (file-directory-p full-file-path)
                  (message-attach-all-files-from-folder disposition full-file-path)

                (setq mime-type (substring (shell-command-to-string (concat "file --mime-type --brief " (shell-quote-argument (expand-file-name full-file-path)))) 0 -1))
                (insert (concat "<#part type=\"" mime-type "\" filename=\"" full-file-path "\" disposition=" disposition ">\n"))))))))

  ;; (setq gnus-gcc-externalize-attachments 'all)
  (setq gnus-gcc-externalize-attachments nil)
  ;    If nil, attach files as normal parts in Gcc copies; if a regexp
  ;    and matches the Gcc group name, attach files as external parts;
  ;    if it is all, attach local files as external parts; if it is
  ;    other non-nil, the behavior is the same as all, but it may be
  ;    changed in the future.
  ;;}}

  )

(defun lotus-gnus-binding-config ()
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
                (local-set-key "f"  'gnus-summary-mail-forward)))))

(defun lotus-gnus-citation-config ()
  (require 'nnheader)

  ;;{{ XSteve, insert Hi Hello Name
  ;; The message-citation-line-function is responsible to display a
  ;; message citation. The following Code allows to switch

  ;; (setq message-citation-line-function #'(lambda () ; was message-insert-citation-line
  ;;                                          (message-insert-formatted-citation-line) ; put wrote:
  ;;                                          (message-goto-body)
  ;;                                          (xsteve-message-citation))) ;put hi

  (setq message-citation-line-function 'message-insert-formatted-citation-line
        message-cite-function 'message-cite-original-without-signature)
  ;; (add-hook 'gnus-message-setup-hook 'xsteve-message-citation t)
  (add-hook 'message-setup-hook 'xsteve-message-citation t)
  ;; (setq message-cite-function 'sc-cite-original)
  ;;

  ;; message-setup-hook

  ;; (defun gettoto ()
  ;;   (interactive)
  ;;   (message message-reply-headers))

  (defun sharad-message-citation-delete ()
    "Delete Hi."
    (message-goto-body)
    (search-forward-regexp "Hi")
    (move-beginning-of-line 1)
    (if (looking-at "Hi")
        (kill-line)))

  (deh-require-maybe gnus-junk

    (defun get-proper-citation-name (email name)
      "Get proper name."
      ;; name can be null for "xxxx@xxxxxx.xxx"
      ;; but email will be there.

      ;; in other case like "Xxxxx Xxxxx <xxxx@xxxxxx.xxx>"
      ;; name and email both will be there.
      (let ((first-name-in-email
             (if (string-match "^\\(\\w\+\\)" email)
                 (match-string 0 email)))
            (first-name-in-name
             (if name (car (split-string name)))))

        (if (and
             first-name-in-name
             (string-caseless-equal first-name-in-email first-name-in-name))
            first-name-in-name
          (capitalize first-name-in-email)))))


  (defun xsteve-message-citation ()
    (interactive)
    (when message-reply-headers
      (xsteve-message-citation-delete)
      (message-goto-body)
      (let* ((from-address
              (mail-header-parse-address (mail-header-from message-reply-headers)))
             (parsed-address
              (if (member (car from-address)
                          message-dont-reply-to-names)
                  (mail-header-parse-address
                   (car
                    (remove-if (lambda (s)
                                 (string-match "^\s*$" s))
                               (split-string (message-fetch-field "to") "[,;]") )))
                from-address))
             (my-bbdb-record (bbdb-search-simple (cdr parsed-address) (car parsed-address)))
             (start-pos (point))
             following-text
             (following-newlines 2)
             (overlay)
             (anrede (when my-bbdb-record (bbdb-record-getprop my-bbdb-record 'anrede)))
             (first-name (funcall 'get-proper-citation-name (car parsed-address) (cdr parsed-address)))
             (name-to-use
              (or (if my-bbdb-record
                      (bbdb-record-name my-bbdb-record)
                    first-name)
                  "Sharad Pratap")))
        (progn
          (if anrede
              (insert (format "%s\n\n" anrede))
            (funcall xsteve-message-citation-function first-name))
          (if following-text (insert following-text))
          (when following-newlines
            (dotimes (v following-newlines)
              (insert "\n"))
            (forward-line (- following-newlines 1))))
        (unless (eq start-pos (point))
          (setq overlay (make-overlay start-pos (point)))
          (overlay-put overlay 'xsteve-message-citation nil)))))

  (defun xsteve-message-citation-hallo (name)
    (insert "Hallo " name "!"))

  (defun xsteve-message-citation-hi (name)
    (insert "Hi " name "!"))

  (defun xsteve-message-citation-herr (name)
    (insert "Hallo Herr " (or name "Fred Namenlos ") "!"))

  (defun xsteve-message-citation-default (name)
    (message-insert-citation-line))

  ;; correct it
  ;; (xsteve-define-alternatives 'xsteve-message-citation-function '(xsteve-message-citation-hallo
  ;;                                                                 xsteve-message-citation-herr
  ;;                                                                 xsteve-message-citation-hi
  ;;                                                                 xsteve-message-citation-default))

  (setq xsteve-message-citation-function
        'xsteve-message-citation-hi)

  (defun xsteve-message-citation-delete ()
    (interactive)                         ;http://www.gnu.org/s/emacs/manual/html_node/elisp/Overlays.html#Overlays
    (let ((overlay)
          (start-pos))
      (goto-char (point-min))
      (goto-char (next-overlay-change (point)))
      (setq overlay (car-safe (overlays-at (point)))) ;; do not use car...
      (when overlay
        (overlay-get overlay 'xsteve-message-citation)
        (setq start-pos (point))
        (goto-char (next-overlay-change (point)))
        (delete-region start-pos (point)))))

  (defun xsteve-message-citation-toggle ()
    (interactive)
    (save-excursion
      ;; (toggle-xsteve-message-citation-function)  ;; implement it
      (xsteve-message-citation)))

  (define-key message-mode-map [f6] 'xsteve-message-citation-toggle)
  ;;}}

  )

(defun lotus-gnus-contacts-config ()
  (deh-require-maybe bbdb

    (defun bbdb/gnus-pop-up-bbdb-buffer-for-some-time ()
      (bbdb/gnus-pop-up-bbdb-buffer)
      ;; (with-selected-window (get-buffer-window gnus-article-buffer)
      ;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
      (let ((w (get-buffer-window "*BBDB*")))
        (when w
          ;; (run-at-time "4 sec" nil #'delete-window w))))
          (run-at-time "4 sec" nil #'(lambda (w)
                                       (if (and
                                            (windowp w)
                                            (window-valid-p w))
                                           (old-delete-window w))) w))))
    (define-key gnus-summary-mode-map (kbd "s-c s-v")  'bbdb/gnus-pop-up-bbdb-buffer)

    (setq bbdb-use-pop-up t
          bbdb-save-db-timeout 0) ;; I want it
    (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
    (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer-for-some-time)))

(defun lotus-gnus-image-config ()
  (require 'gravatar)
  (require 'gnus-art)
  (require 'mail-extr) ;; Because of binding `mail-extr-disable-voodoo'.

  (defgroup gnus-image nil
    "Gnus Image."
    :group 'gnus-visual)

  (defcustom gnus-image-size nil
    "How big should images be displayed.
If nil, default to `image-size'."
    :type 'integer
    :version "24.1"
    :group 'gnus-image)

  (defcustom gnus-image-properties '(:ascent center :relief 1)
    "List of image properties applied to Image images."
    :type 'list
    :version "24.1"
    :group 'gnus-image)

  (defcustom gnus-image-too-ugly gnus-article-x-face-too-ugly
    "Regexp matching posters whose avatar shouldn't be shown automatically."
    :type '(choice regexp (const nil))
    :version "24.1"
    :group 'gnus-image)

  (defvar gnus-image-display-funcs nil "Functions to display images")


  (defun gnus-image-transform-address (header category fun &optional force)
    (gnus-with-article-headers
     (let* ((mail-extr-disable-voodoo t)
            (mail-extr-ignore-realname-equals-mailbox-name nil)
            (addresses (mail-extract-address-components
                        (or (mail-fetch-field header) "") t))
            (image-size (or gnus-image-size gravatar-size))
            name)
       (dolist (address addresses)
         (when (and (setq name (car address))
                    (string-match "\\` +" name))
           (setcar address (setq name (substring name (match-end 0)))))
         (when (or force
                   (not (and gnus-image-too-ugly
                             (or (string-match gnus-image-too-ugly
                                               (or (cadr address) ""))
                                 (and name
                                      (string-match gnus-image-too-ugly
                                                    name))))))
           ;; (ignore-errors
           (funcall fun
                    (cadr address)
                    'gnus-image-insert
                    (list header address category)))))))
  ;;)

  ;; (image-retrieve)
  ;; (gravatar-retrieve)

  (defun gnus-image-insert (image header address category)
    "Insert IMAGE for ADDRESS in HEADER in current article buffer.
Set image category to CATEGORY."
    (unless (eq image 'error)
      (gnus-with-article-buffer
       (let ((mark (point-marker))
             (inhibit-point-motion-hooks t)
             (case-fold-search t))
         (save-restriction
           (article-narrow-to-head)
           ;; The buffer can be gone at this time
           (when (buffer-live-p (current-buffer))
             (gnus-article-goto-header header)
             (mail-header-narrow-to-field)
             (let ((real-name (car address))
                   (mail-address (cadr address)))
               (when (if real-name
                         (re-search-forward
                          (concat (gnus-replace-in-string
                                   (regexp-quote real-name) "[\t ]+" "[\t\n ]+")
                                  "\\|"
                                  (regexp-quote mail-address))
                          nil t)
                       (search-forward mail-address nil t))
                 (goto-char (1- (match-beginning 0)))
                 ;; If we're on the " quoting the name, go backward
                 (when (looking-at "[\"<]")
                   (goto-char (1- (point))))
                 ;; Do not do anything if there's already a image. This can
                 ;; happens if the buffer has been regenerated in the mean time, for
                 ;; example we were fetching someaddress, and then we change to
                 ;; another mail with the same someaddress.
                 (unless (memq 'gnus-image (text-properties-at (point)))
                   (let ((point (point)))
                     (unless (featurep 'xemacs)
                       (setq image (append image gnus-image-properties)))
                     (gnus-put-image image (buffer-substring (point) (1+ point)) category)
                     (put-text-property point (point) 'gnus-image address)
                     (gnus-add-wash-type category)
                     (gnus-add-image category image)))))))
         (goto-char (marker-position mark))))))

  ;;;###autoload
  (defun gnus-treat-from-image (&optional force)
    "Display image in the From header.
If image is already displayed, remove it."
    (interactive (list t)) ;; When type `W D g'
    (gnus-with-article-buffer
     (if (memq 'from-image gnus-article-wash-types)
         (gnus-delete-images 'from-image)
       (loop for fun in gnus-image-display-funcs
             do (gnus-image-transform-address "from" 'from-image fun force)))))

  ;;;###autoload
  (defun gnus-treat-mail-image (&optional force)
    "Display images in the Cc and To headers.
If images are already displayed, remove them."
    (interactive (list t)) ;; When type `W D h'
    (gnus-with-article-buffer
     (if (memq 'mail-image gnus-article-wash-types)
         (gnus-delete-images 'mail-image)
       (loop for fun in gnus-image-display-funcs
             do (progn
                  (gnus-image-transform-address "cc" 'mail-image fun force)
                  (gnus-image-transform-address "to" 'mail-image fun force))))))



  ;; image-retrieve
  ;; or gravatar-retrieve

  (testing

   (eudc-display-jpeg-inline
    (with-temp-buffer
      (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))))

   (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))

   )




  (testing
   (defun gravatar-retrieve (mail-address cb &optional cbargs)
     "Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS."
     (let ((url (gravatar-build-url mail-address)))
       (if (gravatar-cache-expired url)
           (let ((args (list url
                             'gravatar-retrieved
                             (list cb (when cbargs cbargs)))))
             (when (> (length (if (featurep 'xemacs)
                                  (cdr (split-string (function-arglist 'url-retrieve)))
                                (help-function-arglist 'url-retrieve)))
                      4)
               (setq args (nconc args (list t))))
             (apply #'url-retrieve args))
         (apply cb
                (with-temp-buffer
                  (mm-disable-multibyte)
                  (url-cache-extract (url-cache-create-filename url))
                  (gravatar-data->image))
                cbargs)))))


  ;; (defun image-dired-create-thumb (original-file thumbnail-file)

  ;; (defmacro with-string-as-temp-file (str if &optional of &body body)
  ;;   `(let ((if ,(make-temp-file "thumb"))
  ;;          (of ,(make-temp-file "thumb")))
  ;;      (append-to-file ,str nil if)
  ;;      (
  ;;           (find-file of)
  ;;        )
  ;;      ))

  ;; (defun image-dired-create-thumb (original-file thumbnail-file)



  (defun eudc-ldap-retrieve (mail-address cb &optional cbargs)
    "Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS."
    (apply cb
           (create-image
            (cdaar (remove-if 'null (eudc-query (list (cons 'mail mail-address)) '(thumbnailPhoto) t)))
            'jpeg t)
           cbargs))

  (add-to-list 'gnus-image-display-funcs #'eudc-ldap-retrieve)
  (add-to-list 'gnus-image-display-funcs #'gravatar-retrieve)

  ;; (defun image-dired-create-thumb (original-file thumbnail-file)




  (provide 'gnus-image-config)

  ;;; gnus-image.el ends here



  (testing

   (gnus-with-article-headers
    (let* ((mail-extr-disable-voodoo t)
           (mail-extr-ignore-realname-equals-mailbox-name nil)
           (addresses (mail-extract-address-components
                       (or (mail-fetch-field "from") "") t))
           (image-size (or gnus-image-size gravatar-size))
           name)
      (dolist (address addresses)
        (when (and (setq name (car address))
                   (string-match "\\` +" name))
          (setcar address (setq name (substring name (match-end 0)))))
        (when (or t
                  (not (and gnus-image-too-ugly
                            (or (string-match gnus-image-too-ugly
                                              (or (cadr address) ""))
                                (and name
                                     (string-match gnus-image-too-ugly
                                                   name))))))
          (gravatar-retrieve
           ;; (cadr address)
           ;; nil
           email-personal
           'gnus-image-insert
           (list "to"
                 `(nil ,email-personal)
                 'from-image))))))

   (gravatar-retrieve email-personal #'identity)



   (gnus-image-insert (gravatar-retrieve email-personal #'identity)
                      "to" `(nil email-personal) 'from-image)


   (gnus-image-insert
    (list 'image :type 'jpeg :data
          (cdaar (remove-if 'null (eudc-query `((mail . ,email-office )) '(thumbnailPhoto)))))
    "from" `(nil ,email-friend) 'from-image)

   "Name Sur" <email@host.com>
   (list 'image :type 'jpeg :data
         (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))) )

  )

(defun lotus-gnus-template-config ()
  (defvar message-template-directory "~/.xemacs/gnustmpls/")

  (defun gnus-insert-temple ()
    (interactive)
    (let
        ((file (concat
                message-template-directory "/"
                (ido-completing-read "template: "
                                     (mapcar
                                      #'file-name-nondirectory
                                      (directory-files message-template-directory t "[a-zA-Z]+"))))))
      ;; (message-change-subject NEW-SUBJECT)
      (message-goto-body)
      (insert
       (with-temp-buffer
         ;; (template-expand-template file)
         (template-new-file-0 file)
         (buffer-string)))))


  (defun gnus-create-temple ()
    (interactive)
    (let (startbody endbody)

      (save-excursion
        (message-goto-body)
        (setq startbody (point))
        (message-goto-signature)
        (previous-line)
        )


      (template-simple-expand-template file)))

  )

(defun lotus-gnus-misc-config ()
  ;; Integration to bbdb and dired
  (deh-require-maybe bbdb
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (make-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory) t)
  (setq
   mail-user-agent 'gnus-user-agent
   gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory)
   gnus-startup-file   (expand-file-name "autoconfig/gnus/newsrc" user-emacs-directory))


  (global-set-key-if-unbind (kbd "H-s") 'gnus-group-save-newsrc)

  ;; all from gnus-bonus-el deb package
  (xrequire 'gnus-eyecandy) ; -enhance the group buffer by adding icons.
  (xrequire 'gnus-filterhist) ; -add a buffer which display the message filtering
  (xrequire 'gnus-junk) ; -semi-automatic replies to junk e-mails;
  (xrequire 'gnus-pers) ; -an alternative to gnus-posting-styles.
  (xrequire 'message-x) ; -customizable completion in message headers;
  (xrequire 'nnir) ; -searchable mail backend;
  (xrequire 'nnnil) ; -empty, read-only backend;
  (xrequire 'nntodo) ; -manage to-do items;
  (xrequire 'spam-stat) ; -spam-detector based on statistics.




  ;; www.student.montefiore.ulg.ac.be/~merciadri/emacs-gnus.php
  (add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation)


  ;; (defun gnus-other-frame ()
  ;;   "Like `mail' command, but display mail buffer in another frame."
  ;;   (interactive "P")
  ;;   (let ((pop-up-frames t)
  ;; 	(special-display-buffer-names nil)
  ;; 	(special-display-regexps nil)
  ;; 	(same-window-buffer-names nil)
  ;; 	(same-window-regexps nil))
  ;;     (pop-to-buffer "*mail*"))
  ;;   (mail noerase to subject in-reply-to cc replybuffer sendactions))





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


  ;; Use a second connection to grab the next article when I read one, so
  ;; I don't have to wait for it be downloaded.
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



  (setq gnus-registry-install t
        nnmail-crosspost t
        gnus-agent nil)

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
  )

(defun lotus-gnus-schedule-config ()


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
      (defun my-save-icalendar (handle)
        (let ((diary "~/.Organize/emacs/diary/outlook"))
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

  )

(defun lotus-gnus-header-config ()


  (deh-section "Header and ignored From address."
    (setq
     gnus-extra-headers          '(To Newsgroups Content-Type Date)
     nnmail-extra-headers        '(To Newsgroups Content-Type Date)
     gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*")

    ;; (string-match gnus-ignored-from-addresses "spratapfd@arubanetwork" )

    ;; from http://www.ichimusai.org/pub/dot-gnus
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
            "^X-bugzilla"	; Show all X-headers
            ;; for attachment
            "^Content-Type"
            "^X-Face:"
            "^X-Face"
            )
          gnus-sorted-header-list
          '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^Organization:")))




  (deh-section "Art"

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


  ;; gnus-extra-headers

  )

(defun lotus-gnus-crypt-config ()

  (deh-require-maybe mailcrypt
    ;; http://www.suse.de/~garloff/Writings/mutt_gpg/node18.html
    (mc-setversion "gpg")
    (autoload 'mc-install-write-mode "mailcrypt" nil t)
    (autoload 'mc-install-read-mode "mailcrypt" nil t)
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
          (mc-sign-message))))

  )

(defun lotus-gnus-mailto-config ()

  ;; from: http://journal.wjsullivan.net/185095.html

  ;; (defun johnsu01/mailto (url)
  ;;   "Follow a mailto URL as passed from Iceweasel, prompting for a posting style."
  ;;   (let ((gnus-newsgroup-name
  ;;          (completing-read "Use posting style of group: "
  ;;                           gnus-active-hashtb nil
  ;;                           (gnus-read-active-file-p))))
  ;;     (setq url (url-unhex-string url))
  ;;     (browse-url-mail url))
  ;;   ;; message-mail does not do anything with the body argument, so we have to.
  ;;   (if (string-match (regexp-quote "?") url)
  ;;       (let* ((start (match-end 0))
  ;;              (args (url-parse-query-string
  ;;                     (substring url start nil)))
  ;;              (body (cadr (assoc-string "body" args t))))
  ;;         (when body
  ;;           (switch-to-buffer (car (message-buffers)))
  ;;           (save-excursion
  ;;             (message-goto-body)
  ;;             (insert body))))))

  (setq mail-user-agent                   ;see Chvmail-user-agent
        'gnus-user-agent)




  ;; (defun gnus-summary-mail-other-window (&optional arg)
  ;;   "Start composing a mail in another window.
  ;; Use the posting of the current group by default.
  ;; If ARG, don't do that.  If ARG is 1, prompt for group name to find the
  ;; posting style."
  ;;   (interactive "P")
  ;;   ;; We can't `let' gnus-newsgroup-name here, since that leads
  ;;   ;; to local variables leaking.
  ;;   (let ((group gnus-newsgroup-name)
  ;; 	;; make sure last viewed article doesn't affect posting styles:
  ;; 	(gnus-article-copy)
  ;; 	(buffer (current-buffer)))
  ;;     (unwind-protect
  ;; 	(progn
  ;; 	  (setq gnus-newsgroup-name
  ;; 		(if arg
  ;; 		    (if (= 1 (prefix-numeric-value arg))
  ;; 			(gnus-group-completing-read "Use group: "
  ;; 						    nil nil
  ;; 						    (gnus-read-active-file-p))
  ;; 		      "")
  ;; 		  gnus-newsgroup-name))
  ;; 	  ;; #### see comment in gnus-setup-message -- drv
  ;; 	  (gnus-setup-message 'message (message-mail)))
  ;;       (with-current-buffer buffer
  ;; 	(setq gnus-newsgroup-name group)))))

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
  )

(defun lotus-gnus-message-config ()
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
                   )
               `((name ,myname)
                 (signature "Regards,\n-sharad")
                 ("Jabber-ID" ,jabber-id)
                 (address ,email-addr)))

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
            (let* ((default-email (concat myname " <" email-addr ">"))
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
           (address ,email-addr)
           ("From" ,email-addr)
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
  )

(defun lotus-gnus-nm-config ()
  ;;{{ from http://www.emacswiki.org/emacs/GnusNetworkManager
  ;(when (and nil (xrequire 'dbus))
  (when (xrequire 'dbus)
    (defvar gnus-nm-dbus-registration nil
      "dsfdsf")
    (defvar gnus-nm-connected-hook nil
      "Functions to run when network is connected.")
    (defvar gnus-nm-disconnected-hook nil
      "Functions to run when network is disconnected.")
    (setq gnus-nm-dbus-registration nil)

    (defun imap-nuke-server-processes ()
      "Brutally kill running IMAP server background processes. Useful
when Gnus hangs on network outs or changes."
      (interactive)
      (let ((sm (if gnus-select-method
                    (cons gnus-select-method gnus-secondary-select-methods)
                  gnus-secondary-select-methods)))
        (while sm
          (let ((method (car (car sm)))
                (vserv (nth 1 (car sm))))
            (when (and (eq 'nnimap method)
                       (not (string= "localhost"
                                     ;(second (find-if
                                     (second (remove-if-not
                                              (lambda (e)
                                                (if (listp e)
                                                    (eq 'nnimap-address (car e))))
                                              sm))))
                       (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))
              (gnus-message 6 "Killing IMAP process for server %s" vserv)
              (delete-process (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))))
          (setq sm (cdr sm)))))

    (defun gnus-nm-agent-unplug()
      "Kill IMAP server processes and unplug Gnus agent."
      (gnus-message 6 "Network is disconnected, unplugging Gnus agent.")
      (with-current-buffer gnus-group-buffer
        (imap-nuke-server-processes) ; optional, help prevent hangs in IMAP processes when network has gone down.
        (gnus-agent-toggle-plugged nil)))

    (defun gnus-nm-agent-plug()
      "Plug Gnus agent."
      (gnus-message 6 "Network is connected, plugging Gnus agent.")
      (with-current-buffer gnus-group-buffer
        (gnus-agent-toggle-plugged t)))

    (defun gnus-nm-state-dbus-signal-handler (nmstate)
      "Handles NetworkManager signals and runs appropriate hooks."
      (when (and (fboundp 'gnus-alive-p) (gnus-alive-p))
        (cond
         ((or (= 4 nmstate) (= 1 nmstate))
          (run-hooks 'gnus-nm-disconnected-hook))
         ((= 3 nmstate)
          (run-hooks 'gnus-nm-connected-hook)))))

    (defun gnus-nm-enable()
      "Enable integration with NetworkManager."
      (interactive)
      (when (not gnus-nm-dbus-registration)
        (progn (setq gnus-nm-dbus-registration
                     (dbus-register-signal :system
                                           "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                                           "org.freedesktop.NetworkManager" "StateChanged"
                                           'gnus-nm-state-dbus-signal-handler))
               (gnus-message 6 "Enabled integration with NetworkManager"))))

    (defun gnus-nm-disable()
      "Disable integration with NetworkManager."
      (interactive)
      (when gnus-nm-dbus-registration
        (progn (dbus-unregister-object gnus-nm-dbus-registration)
               (setq gnus-nm-dbus-registration nil)
               (gnus-message 6 "Disabled integration with NetworkManager"))))

    ;; Add hooks for plugging/unplugging on network state change:
    (add-hook 'gnus-nm-connected-hook    'gnus-nm-agent-plug)
    (add-hook 'gnus-nm-connected-hook    'gnus-group-send-queue)
    (add-hook 'gnus-nm-disconnected-hook 'gnus-nm-agent-unplug)
    ;; Add hooks for enabling/disabling integration on startup/shutdown:
    (add-hook 'gnus-started-hook   'gnus-nm-enable)
    (add-hook 'gnus-exit-gnus-hook 'gnus-nm-disable))
  ;;}} (gnus-nm-disable)
  )

(defun lotus-gnus-notif-config ()

  ;;{{ from: http://www.emacswiki.org/emacs/GnusNotify
  ;; I am using gnus-notify+

  (defvar gnus-mst-display-new-messages "New Mails" "doc")
  (defvar gnus-mst-notify-groups "*" "doc")

  (when (xrequire 'gnus-notify)
    (setq gnus-mst-display-new-messages "New mails"
          gnus-mst-notify-groups
          (if (equal (system-name) "spratap")
              '("nnimap+localhost:Office.INBOX" "nnimap+localhost:Office.lists.info.india" "nnimap+localhost:Office.lists.info.india-misc")
            '("nnimap+localhost:nnimap+localhost:Gmail.INBOX"))))

  (when (xrequire 'gnus-notify+)
    ;; adding (modeline-notify t) to group for gnus-notify+
    (set-or-nconc gnus-parameters           ;check for set-or-nconc in macros.el
                  `((,(mapconcat 'identity gnus-mst-notify-groups "\\|")
                     '(modeline-notify t)))))

  ;; (macroexpand `(set-or-nconc xgnus-parameters           ;check for set-or-nconc in macros.el
  ;;                             ( ,(mapconcat 'identity gnus-mst-notify-groups "|")
  ;;                                (modeline-notify t))))

  ;;}}

  ;;{{ from: http://www.emacswiki.org/emacs/GnusBiff
  ;; biff
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

  ;; How about:

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

  ;; test
  ;; (if (= 0 (shell-command "zenity --question --text Hi"))
  ;;     (make-frame))



  ;;}}


  ;; {{ http://exal.0x2.org/conf/gnus.html
  ;;;
  ;;; Get messages automaticaly
  ;;;

  ;; (if (xrequire 'gnus-demon)
  ;;     (setq gnus-use-demon t)
  ;;     (gnus-demon-add-handler 'gnus-group-get-new-news 3 2)
  ;;     (gnus-demon-init))

  (when (xrequire 'gnus-demon)

    ;; Group Level
    ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Levels.html
    ;; http://www.emacswiki.org/emacs/GnusNiftyTricks
    ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
    ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Scanning-New-Messages.html
    ;; http://www.emacswiki.org/emacs/GnusRss
    ;; http://www.emacswiki.org/emacs/GnusDemon



    (when nil ;; old




      (defun gnus-demon-scan-mail-or-news-and-update ()
        "Scan for new mail/news and update the *Group* buffer"
        (when (gnus-alive-p)
          (save-window-excursion
            (save-excursion
              (with-current-buffer gnus-group-buffer
                ;; (set-buffer gnus-group-buffer)
                (gnus-group-get-new-news)
                (message nil))))))

      (defun gnus-demon-scan-and-update ()
        (gnus-demon-scan-mail-or-news-and-update)))



    ;; (defvar gnus-scan-man-idle-timer
    ;;   (progn                                ; Cancel to prevent duplication.
    ;;     (when (boundp 'gnus-scan-man-idle-timer) (cancel-timer gnus-scan-man-idle-timer))
    ;;     (run-with-idle-timer gnus-scan-man-idle-interval nil 'hl-line-highlight-now))
    ;;   "Timer used to turn on `global-hl-line-mode' whenever Emacs is idle.")

    (setq gnus-use-demon t)

    ;; not being used
    ;; (defun gnus-demon-scan-mail-and-news ()
    ;;   (cancel-timer
    ;;    (run-with-idle-timer 6 nil 'gnus-demon-scan-mail-and-news-now)))

    (defun gnus-demon-scan-mail-and-news-now (&optional level)
      "Scan for new mail/news and update the *Group* buffer."
      (let ((level (or level 3))
            (time-format "%a %H:%M:%S"))
        (message "gnus-demon-scan-mail-and-news-now: current time %s, idle time %d"
                 (format-time-string time-format (current-time))
                 (float-time (current-idle-time)))
        (when (gnus-alive-p)
          (message "gnus-demon-scan-mail-and-news-now %d" level)
          (save-window-excursion
            (save-excursion
              (with-current-buffer gnus-group-buffer
                ;; (set-buffer gnus-group-buffer)
                (let ((idle-time (current-idle-time)))
                  (if (> (float-time idle-time) 7)
                      (with-timeout (3 (message "gnus demon timeout"))
                        (gnus-group-get-new-news level))
                    (message "not running gnus demon")))
                (message nil)))))))

    (add-hook 'gnus-group-mode-hook 'gnus-demon-init)
    (add-hook 'gnus-exit-gnus-hook 'gnus-demon-cancel)

    ;; Sort threads by the date of the root node.
    (setq gnus-thread-sort-functions `(gnus-thread-sort-by-date))
    ;; Initialize the Gnus daemon, check new mail every six minutes.
    ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news 1 nil))
    ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 2 nil)
    (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 10 10)

    (gnus-demon-add-handler '(lambda () (gnus-demon-scan-mail-and-news-now 6)) 22 20))

  ;;}}

  )

(defun lotus-gnus-notmuch-config ()

  (deh-require-maybe notmuch


    (deh-require-maybe notmuch-address
      ;; http://notmuchmail.org/emacstips/
      (setq notmuch-address-command (expand-file-name "notmuch-addrlookup" "~/bin"))
      (if (file-exists-p notmuch-address-command)
          (notmuch-address-message-insinuate)))

    (defun lotus-gnu-notmuch-current-message-id ()
      (interactive)
      (let ((id (with-current-buffer gnus-original-article-buffer
                  (nnheader-narrow-to-headers)
                  (message-fetch-field "message-id"))))
        (if (and (eql (aref id 0) ?<)
                 (eql (aref id (- (length id) 1)) ?>))
            (subseq id 1 (- (length id) 1))
          id)))

    (defun notmuch-select-tag-with-completion (prompt &rest search-terms)
      (let ((tag-list
             (with-output-to-string
               (with-current-buffer standard-output
                 (apply 'call-process notmuch-command nil t nil "search-tags" search-terms)))))
        (completing-read prompt (split-string tag-list "\n+" t) nil nil nil)))

    (defun notmuch-message-tags ()
      (let ((tag-list
             (with-output-to-string
               (with-current-buffer standard-output
                 (apply 'call-process notmuch-command nil t nil
                        "search --format=text --output=tags"
                        (concat "id:" search-id-string)))))
            tag-list)))

    (defun lotus-notmuch-add-jobapply-tag (tag)
      (interactive
       (list (notmuch-select-tag-with-completion "Tag to add: ")))
      (let ((search-id-string (lotus-gnu-notmuch-current-message-id)))
        (notmuch-call-notmuch-process "tag" (concat "+" "jobapply") (concat "id:" search-id-string)))))
  )

(defun lotus-gnus-pres-config ()
  (deh-require-maybe gnus-pers

    (defvar gnus-personality-activate nil "")

    (defun gnus-personality-activate-toggle ()
      "Toggle setting tab widths between 4 and 8"
      (interactive)
      (setq gnus-personality-activate
            (not gnus-personality-activate)))



    (defun gnus-personality-init/sharad ()
      "Install Personality functionality into message mode."
      (interactive)
      (add-hook 'message-setup-hook
                (lambda ()
                  (if gnus-personality-activate
                      (gnus-personality-electric-headers headers)))))


    (gnus-personality-init/sharad)))

(defun lotus-gnus-reply-config ()
  ;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine
  ;; Second, two commands for replying to mail. If there are several
  ;; senders and recipients or a CC: field, then you will be prompted
  ;; whether you want to reply to all.


  ;;
  (setq message-alternative-emails
        (regexp-opt
         (list
          email-addr
          office-email
          office-fortinet-email
          office-meru-email)))

  ;; message-forward-ignored-headers
  ;; ;; "\\(?:\\^\\(?:Content-Transfer-Encoding:\\|In-Reply-To\\|Message-ID\\|References\\|User-Agent\\|X-\\(?:Draft-From\\|Gnus\\|OfflineIMAP\\)\\)\\)"
  ;; (regexp-opt '(
  ;;               "^Content-Transfer-Encoding:" ; original
  ;;               "^X-Gnus" ; original
  ;;               "^Message-ID"
  ;;               "^X-Draft-From"
  ;;               "^In-Reply-To"
  ;;               "^References"
  ;;               "^User-Agent"
  ;;               "^X-OfflineIMAP"
  ;;               ))
  ;; )


  (defun gnus-summary-dwim-reply ()
    "reply depending on the CC: header"
    (interactive)
    (gnus-with-article-headers
     (cond
      ((not (re-search-forward "^C[Cc]: .\\|^To:.*," nil t))
       (gnus-summary-reply))
      ((y-or-n-p "Reply to all ? ")
       (gnus-summary-wide-reply)
       (goto-char (point-min))
       (flush-lines "^Cc: $"))
      (t (gnus-summary-reply)))
     (message-goto-body)))

  (defun gnus-summary-dwim-reply-with-original ()
    "reply, ask all if there is a CC: header or several recipients"
    (interactive)
    (gnus-with-article-headers
     (cond
      ((not (re-search-forward "C[Cc]: .\\|To:.*," nil t))
       (gnus-summary-reply-with-original nil))
      ((y-or-n-p "Reply to all ? ")
       ;; or gnus-summary-very-wide-reply-with-original ?
       (gnus-summary-wide-reply-with-original nil)
       (goto-char (point-min))
       (flush-lines "^Cc: $"))
      (t (gnus-summary-reply-with-original nil)))
     ;; replace next by message-goto-body to get cursor before citation
     (message-goto-signature)))



  ;; http://www.mail-archive.com/info-gnus-english@gnu.org/msg09033.html
  (setq message-dont-reply-to-names
        (append
         (list
          user-mail-address
          email-addr
          office-email
          office-fortinet-email
          office-meru-email)
         user-other-email-addresses))

  ;;}}
  )

(defun lotus-gnus-search-config ()

  (deh-section "Notmuch"

    (when (xrequire 'notmuch)
      (setq notmuch-fcc-dirs nil)

      ;; from: http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/c87cf38bc84a183b
      (defun th-notmuch-file-to-group (file)
        "Calculate the Gnus group name from the given file name.

Example:

  IN: /home/horn/Mail/Dovecot/Fastmail/.INBOX.mailinglists.notmuch/cur/1259184569.M4818P3384.localhost,W=6921:2,S
  OUT: nnimap+Fastmail:INBOX.mailinglists.notmuch

  IN: /home/horn/Mail/Dovecot/Fastmail/cur/1259176906.M17483P24679.localhost,W=2488:2,S
  OUT:nnimap+Fastmail:INBOX"
        (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
          (setq group (replace-regexp-in-string (concat (getenv "HOME") ".maildir/" "" group)))
          (setq group (replace-regexp-in-string "^\\([^/]+\\)/" "nnimap+\\1:" group t))
          (setq group (replace-regexp-in-string "/$" "" group))
          (if (string-match ":$" group)
              (concat group "INBOX")
            (replace-regexp-in-string ":\\." ":" group))))

      (defun th-notmuch-goto-message-in-gnus ()
        "Open a summary buffer containing the current notmuch
article."
        (interactive)
        (let ((group (th-notmuch-file-to-group (notmuch-show-get-filename)))
              (message-id (replace-regexp-in-string
                           "^id:" "" (notmuch-show-get-message-id))))
          (if (and group message-id)
              (org-gnus-follow-link group message-id)
            (message "Couldn't get relevant infos for switching to Gnus."))))

      (define-key notmuch-show-mode-map (kbd "C-c C-c") 'th-notmuch-goto-message-in-gnus)


      ;; http://www.emacswiki.org/emacs/NotMuch
      (defun notmuch-file-to-group (file)
        "Calculate the Gnus group name from the given file name."
        (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
          (setq group (replace-regexp-in-string ".*/.Maildir/" "nnimap+localhost:" group))
          (setq group (replace-regexp-in-string "/$" "" group))
          (if (string-match ":$" group)
              (concat group "INBOX")
            (replace-regexp-in-string ":\\." ":" group))
          ;; Seems like we don't even need this part:
          ;; (setq group (replace-regexp-in-string "nnimap\\+localhost:\\.?" "" group))
          ))

      (defun notmuch-goto-message-in-gnus ()
        "Open a summary buffer containing the current notmuch
     article."
        (interactive)
        (unless (gnus-alive-p) (with-temp-buffer (gnus)))
        (let ((group (notmuch-file-to-group (notmuch-show-get-filename)))
              (message-id
               (replace-regexp-in-string "\"" ""
                                         (replace-regexp-in-string "^id:" ""
                                                                   (notmuch-show-get-message-id)))))
          (message group)
          (if (and group message-id)
              (progn
                (gnus-summary-read-group group 1) ; have to show at least one old message
                (gnus-summary-refer-article message-id)) ; simpler than org-gnus method?
            (message "Couldn't get relevant infos for switching to Gnus."))))

      (define-key notmuch-show-mode-map (kbd "C-c C-c") 'notmuch-goto-message-in-gnus)))



  (deh-require-maybe gnus-namazu

    ;; Setup the search via gnus-namazu. First create the index via the command line.

    ;; # generate the database: look at gnus-directory, mine is "~/gnus"
    ;; # ~/gnus/nnml-mail contains the mails
    ;; mkdir ~/gnus/namazu
    ;; mknmz -a -h -O ~/gnus/namazu ~/gnus/nnml-mail


    ;; Enable gnus-namazu. You can start a search vie C-c C-n.

    (require 'gnus-namazu)
    (gnus-namazu-insinuate)
    (setq gnus-namazu-index-update-interval nil)
    ;; call explicitely M-x gnus-namazu-update-all-indices


    ;; Update the namazu index every day at 6:00am
    (defun xsteve-gnus-namazu-update-all-indices ()
      (interactive)
      (gnus-namazu-update-all-indices t))

    (defun xsteve-gnus-update-namazu-index ()
      (run-at-time "6:00am" nil 'xsteve-gnus-namazu-update-all-indices))

    (require 'midnight)
    (add-hook 'midnight-hook 'xsteve-gnus-update-namazu-index))
  )

(defun lotus-gnus-server-config ()
  (require 'nnheader)

  ;; http://www.delorie.com/gnu/docs/emacs/gnus_316.html Q1.11
  ;; (require 'tm-setup)
  ;; (require 'gnus)
  ;; (require 'mime-compose)

  ;; use M-x password-reset (from password-cache.el)
  ;; if .authinfo.gpg  became active later.

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
        "Gmail\\.INBOX\\|Gmail\\.sent-mail")))


  ;;{{ make it working only for Outlook Office
  (setq gnus-message-archive-method
        '(nnimap "localhost"
                 (nnimap-address "localhost")
                 ;; (nnimap-server-port 993)
                 ;; (nnimap-server-port 443)
                 (nnimap-server-port 143)
                 ;; (nnimap-stream ssl)
                 (nnimap-authinfo-file "~/.authinfo.gpg")))
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

  )

(defun lotus-gnus-shumbun-config ()
  (deh-require (and shimbun sb-rss-blogs)

    (add-to-list 'shimbun-rss-blogs-group-url-regexp
                 '("OSNews" "http://www.osnews.com/files/recent.xml"))

    (add-to-list 'shimbun-rss-blogs-group-url-regexp
                 '("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux"))


    (add-to-list 'shimbun-rss-blogs-group-url-regexp
                 '("NDTV" "http://feeds2.feedburner.com/NdtvNews-TopStories")))

  (setq shimbun-atom-hash-group-path-alist
        '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
          ("OSNews" "http://www.osnews.com/files/recent.xml" t)
          ("PlanetEmacsen" "http://planet.emacsen.org/atom.xml" t)
          ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t))

        shimbun-rss-hash-group-path-alist
        '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
          ("OSNews" "http://www.osnews.com/files/recent.xml" t)
          ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t)))

  )

(defun lotus-gnus-stat-config ()
  (deh-section "statistics"
    ;;{{
    ;; from: http://www.ichimusai.org/pub/dot-gnus
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; STATISTICS FUNCTION
    ;;
    ;; Select from summary buffer and run M-x stat RET
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (defun stat (beg end)
      (interactive "r")
      (let (header from-list subject-list from subject (n 0) (chars 0))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (setq header (gnus-summary-article-header))
            (incf n)
            (incf chars (mail-header-chars header))
            (setq from (gnus-extract-address-components (mail-header-from header)))
            (setq from (or (car from) (cadr from)))
            (if (assoc from from-list)
                (incf (cdr (assoc from from-list)))
              (push (cons from 1) from-list))
            (setq subject (gnus-simplify-subject (mail-header-subject header)))
            (if (assoc subject subject-list)
                (incf (cdr (assoc subject subject-list)))
              (push (cons subject 1) subject-list))
            (forward-line)))
        (setq from-list (sort from-list (lambda (a b) (> (cdr a) (cdr b)))))
        (setq subject-list (sort subject-list (lambda (a b) (> (cdr a) (cdr b)))))
        (switch-to-buffer-other-window (get-buffer-create "*stat*"))
        (insert (format "Total number of posts: %i\n" n))
        (insert (format "Average bytes/post: %f\n" (/ (float chars) n)))
        (insert (format "Total number of posters: %i\n" (length from-list)))
        (insert (format "Average posts/poster: %f\n\n" (stat-mean from-list)))
        (stat-top from-list 20)
        (insert (format "\nTotal number of subjects: %i\n" (length subject-list)))
        (insert (format "Average posts/subject: %f\n\n" (stat-mean subject-list)))
        (stat-top subject-list 20)))

    (defun stat-mean (alist)
      (let ((mean 0))
        (dolist (x alist)
          (incf mean (cdr x)))
        (/ (float mean) (length alist))))

    (defun stat-top (alist &optional n)
      (dotimes (i (if (integerp n)
                      (min n (length alist))
                    (length alist)))
        (insert (format "%4i %s\n"
                        (cdr (nth i alist))
                        (car (nth i alist))))))


    ;;}}

    ))

(defun lotus-gnus-utils-config ()


  (require 'thingatpt+)

  (defun google-lucky (string)
    (concat string " [http://www.google.com/search?hl=en&&q="
            (or (string-replace-match "\s" string "+" t t) string)
            "&btnI=1]"))


  (defun google (string)
    (concat string " [http://www.google.com/search?hl=en&&q="
            (or (string-replace-match "\s" string "+" t t) string)
            "]"))

  (defun string-apply-fn (&optional fn)
    (interactive
     (let ((fn (symbol-function
                (intern (ido-completing-read "Modifier to apply: "
                                             '("google-lucky" "google"))))))
       (list fn)))
    (let* ((region-active (and (region-active-p)
                               (not (equal (region-beginning) (region-end)))))
           (bound (if region-active
                      (cons (region-beginning) (region-end))
                    (bounds-of-thing-at-point 'word)))
           (search-str (funcall #'buffer-substring (car bound) (cdr bound))))
      (funcall #'delete-region (car bound) (cdr bound))
      (insert (funcall fn search-str))))

  )

(defun lotus-gnus-window-config ()


  (deh-section "Toggle Article Window"
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
          (gnus-configure-windows 'article 'force)))))

  )
