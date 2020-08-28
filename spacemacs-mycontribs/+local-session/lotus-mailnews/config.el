;;; config.el --- config                             -*- lexical-binding: t; -*-

(defmacro lotus-fix-later (&rest body)
  `(when nil
     ,@body))

(defun lotus-load-if-exists (file)
  (when (file-exists-p file)
    (load-file file)))

(defvar lotus-osetup-dir "~/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup")

(defun lotus-load-osetup-dir ()
  (lotus-load-if-exists (expand-file-name "info.d/common/elisp/common-info.el" lotus-osetup-dir))
  (lotus-load-if-exists (expand-file-name "info.d/common/elisp/passwds.el" lotus-osetup-dir))
  (lotus-load-if-exists (expand-file-name "info.d/hosts/default/elisp/host-info.el" lotus-osetup-dir))
  (lotus-load-if-exists (concat (expand-file-name "info.d/hosts/" lotus-osetup-dir)
                                (system-name)
                                "/elisp/host-info.el")))

(lotus-load-osetup-dir)

;; notmuch

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
    (notmuch-call-notmuch-process "tag" (concat "+" "jobapply") (concat "id:" search-id-string))))

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
        (message-id (replace-regexp-in-string "^id:" ""
                                              (notmuch-show-get-message-id))))
    (if (and group
             message-id)
        (org-gnus-follow-link group
                              message-id)
      (message "Couldn't get relevant infos for switching to Gnus."))))

;; (define-key notmuch-show-mode-map (kbd "C-c C-c") 'th-notmuch-goto-message-in-gnus)


;; http://www.emacswiki.org/emacs/NotMuch
(defun notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name."
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/.Maildir/" "nnimap+localhost:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))
    ;; Seems like we don't even need this part:
    ;; (setq group (replace-regexp-in-string "nnimap\\+localhost:\\.?" "" group))


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

;; (define-key notmuch-show-mode-map (kbd "C-c C-c") 'notmuch-goto-message-in-gnus)

;; gnus-pers

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

;;; gnus-art - article

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
;;              (gnus-article-fill-cited-article)
;; (with-selected-window (get-buffer-window gnus-article-buffer)
;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
;; (select-window (get-buffer-window gnus-summary-buffer))

(unless (fboundp 'gnus-article-goto-subject)
  (defun gnus-article-goto-subject ()
    (interactive)
    (with-selected-window
        (get-buffer-window gnus-article-buffer)
      (gnus-summary-goto-subject
       (cdr gnus-article-current)))))

(defun article-show-attachment (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-show-attachment-arg))
  (unless (gnus-article-check-hidden-text 'signature arg)
    (save-excursion
      (save-restriction)))
  (let ((inhibit-read-only t))
    (when (gnus-article-narrow-to-signature)
      (gnus-article-hide-text-type
       (point-min) (point-max) 'signature)))
  (gnus-set-mode-line 'article))

(defun gnus-show-attachment-arg ()
  'head)

(defun article-show-attachment ()
  "Translate article using an online translation service."
  ;; (interactive (gnus-show-attachment-arg))
  (interactive)
  (gnus-with-article-buffer
    (when (article-goto-body)
      (let* ((start (point)))
        (end (point-max))
        (orig (buffer-substring start end)
              ;; (trans (babel-as-string orig)))

              (insert "\nArticle has attachment\n"))))))

;; reply

(defun lotus-message-signature-present ()
  (save-excursion
    (if (message-goto-signature)
        (eobp))))

(defun jreply (&optional keys)
  (interactive)
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
      (gnus-summary-wide-reply-with-original< nil)
      (goto-char (point-min))
      (flush-lines "^Cc: $"))
     (t (gnus-summary-reply-with-original nil)))
    ;; replace next by message-goto-body to get cursor before citation
    (message-goto-signature)))

;; message

;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine
;; (require 'message)

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
        (error "Message not sent"))))
  (add-hook 'message-send-hook 'check-mail))

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
              (insert (concat "<#part type=\"" mime-type
                              "\" filename=\"" full-file-path
                              "\" disposition=" disposition ">\n"))))))))
;;}}

;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
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
                       "Found the word `attach' but no MIME attachment: send anyway? ")
                      (error "Aborted send")))))
        (set-marker end-of-headers nil)))))

;; citation
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
                             (split-string (message-fetch-field "to") "[,;]"))))
              from-address)))
      (if (functionp 'bbdb-search-simple)
          (let ((my-bbdb-record (bbdb-search-simple (cdr parsed-address) (car parsed-address)))
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
              (overlay-put overlay 'xsteve-message-citation nil)))
        (message "bbdb3 not have #'bbdb-search-simple use #'bbdb-search-mail from bbdb3 for name %s address %s"
                 (cdr parsed-address)
                 (car parsed-address))))))

(defun xsteve-message-citation-hallo (name)
  (insert "Hallo " name "!"))

(defun xsteve-message-citation-hi (name)
  (insert "Hi " name "!"))

(defun xsteve-message-citation-herr (name)
  (insert "Hallo Herr " (or name "Fred Namenlos ") "!"))

(defun xsteve-message-citation-default (name)
  (message-insert-citation-line))


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

;; gnus-junk
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
      (capitalize first-name-in-email))))

;; gnus-msg

;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus

(defun dummy-add-formalities ()
  "Thanks."
  (message-position-point))

(defun cjb-add-formalities ()
  "Add the sender's first name and my tag to e-mail."
  ;; Modified from <http://www.repose.cx/conf/.elisp/de-gnus.el>
  (save-excursion
    (message-goto-signature)
    ;; (previous-line 1)
    (forward-line)
    (when (not (looking-at "Sharad"))
         (insert "\n\n- Sharad."))
    (let* ((to (message-fetch-field "To")))
         (address-comp (mail-extract-address-components to))
         (name (car address-comp))
         (first (or (and name (concat "" (car (split-string name)))) ""))

         (when first
           ;; Go to the first line of the message body.
           (message-goto-body)
           (insert "Hi,\n\n")
           (kill-line)
           (kill-line)
           (kill-line)
           (message-goto-signature)
           (forward-line -4)
           (newline)))))

;; rs-gnus-exts - gnus-summary

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

;; message template

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
      (previous-line))
    (template-simple-expand-template file)))

;; posting style

(defun lotus-gnus-posting-style-function ()
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


    ;; try to get only to address, not all in CC Bcc)

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
               "sent-news")
             ((save-excursion )t (make-local-variable 'message-citation-line-function) 'message-insert-formatted-citation-line)
             (set (make-local-variable 'message-cite-reply-above) nil)
             (set (make-local-variable 'message-cite-reply-position) 'traditional)))))


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

    ("Office.Fortinet.*\\|nnvirtual:Inbox\\|nnvirtual:Inbox-Sent\\|nnvirtual:Incoming"
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

;; gnus-daemon

;; Group Level
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Levels.html
;; http://www.emacswiki.org/emacs/GnusNiftyTricks
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Scanning-New-Messages.html
;; http://www.emacswiki.org/emacs/GnusRss
;; http://www.emacswiki.org/emacs/GnusDemon

;; (defvar gnus-scan-man-idle-timer
;;   (progn                                ; Cancel to prevent duplication.
;;     (when (boundp 'gnus-scan-man-idle-timer) (cancel-timer gnus-scan-man-idle-timer))
;;     (run-with-idle-timer gnus-scan-man-idle-interval nil 'hl-line-highlight-now))
;;   "Timer used to turn on `global-hl-line-mode' whenever Emacs is idle.")



;; not being used
;; (defun gnus-demon-scan-mail-and-news ()
;;   (cancel-timer
;;    (run-with-idle-timer 6 nil 'gnus-demon-scan-mail-and-news-now)))
(defvar gnus-demon-scan-mail-and-news-now-min-idle-time 3) ;; 7
(defvar gnus-demon-scan-mail-and-news-now-max-run-time  10) ;; 3
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
              (if (> (float-time idle-time) gnus-demon-scan-mail-and-news-now-min-idle-time)
                  (with-timeout (gnus-demon-scan-mail-and-news-now-max-run-time
                                 (message "gnus demon timeout"))
                    (gnus-group-get-new-news level))
                (message "not running gnus demon")))
            (message nil)))))))

;; gnus-nm network-manager

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

;; stats
;; ---
(defun lotus-gnus-stat (beg end)
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
    (insert (format "Average posts/poster: %f\n\n" (lotus-gnus-stat-mean from-list)))
    (lotus-gnus-stat-top from-list 20)
    (insert (format "\nTotal number of subjects: %i\n" (length subject-list)))
    (insert (format "Average posts/subject: %f\n\n" (lotus-gnus-stat-mean subject-list)))
    (lotus-gnus-stat-top subject-list 20)))

(defun lotus-gnus-stat-mean (alist)
  (let ((mean 0))
    (dolist (x alist)
      (incf mean (cdr x)))
    (/ (float mean) (length alist))))

(defun lotus-gnus-stat-top (alist &optional n)
  (dotimes (i (if (integerp n)
                  (min n (length alist))
                (length alist)))
    (insert (format "%4i %s\n"
                    (cdr (nth i alist))
                    (car (nth i alist))))))

;; --


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

;; gnus-notification

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

;; ----

(defvar mac-biff-lighter ""
  "Lighter used by `mac-biff-mode'.")

(defvar mac-biff-mail-re "\\([[:digit:]]+\\)"
  "Regular expression to match number counts in a Gnus buffer.")

(define-minor-mode mac-biff-mode
  "Minor mode to display state of new email."
  nil mac-biff-lighter nil
  (if mac-biff-mode
      (progn
        (add-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
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
            (make-frame)))))

;; --

;;{{ from: http://www.emacswiki.org/emacs/GnusNotify

(defvar gnus-mst-display-new-messages "New Mails" "doc")
(defvar gnus-mst-notify-groups "*" "doc")

(when (require 'gnus-notify nil t)
  (setq gnus-mst-display-new-messages "New mails"
        gnus-mst-notify-groups
        (if (equal (system-name) "spratap")
            '("nnimap+localhost:Office.INBOX" "nnimap+localhost:Office.lists.info.india" "nnimap+localhost:Office.lists.info.india-misc")
          '("nnimap+localhost:nnimap+localhost:Gmail.INBOX"))))

;;}}


(add-hook 'gnus-after-getting-new-news-hook 'fmbiff)

;; mail-utils

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
  (let* ((region-active (when (region-active-p)
                          (not (equal (region-beginning) (region-end)))))
         (bound (if region-active
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'word)))
         (search-str (funcall #'buffer-substring (car bound) (cdr bound))))
    (funcall #'delete-region (car bound) (cdr bound))
    (insert (funcall fn search-str))))


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

;; --

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

;; --
;;{{ Address Book http://www.emacswiki.org/emacs/ExternalAbook
(when (require 'external-abook nil t)
  (setq external-abook-command "timeout 4 /usr/bin/lbdbq '%s*' | sed 1d | cut -d'	' -f1,2") ;;"contacts -lf '%%e\t%%n' %s")

  (eval-after-load "message"
    '(progn
       (add-to-list 'message-mode-hook
                    #'(lambda ()
                        (define-key message-mode-map "\C-c\t" 'external-abook-try-expand))))))
;;}}

;; fit in correct place

(setq gnus-registry-install t
      nnmail-crosspost t
      gnus-agent nil)

;;{{
(lotus-fix-later
 (setq gnus-local-domain (or (getenv "DOMAINNAME") office-fqdn)))
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
 read-mail-command 'gnus)
 ;;gnus-treat-display-smileys nil

(autoload 'sendmail-send-it "sendmail")

;; https://www.emacswiki.org/emacs/CategoryMailAddons
;; (require 'sigbegone)



(defun lotus-mailnews/init-outlook-init ())

(defun lotus-mailnews/init-outlook-config ()
  (setq outlook-organization-domain-regexp "fsf\\.org"))

(defun lotus-mailnews/init-org-outlook-init ())

(defun lotus-mailnews/init-org-outlook-config ()
  (setq outlook-organization-domain-regexp "fsf\\.org"))

(defun lotus-mailnews/common-init-gnus ()
  (interactive)
  ;; (debug)
  (progn
    (setq gnus-init-file "~/.gnus.el")
    (make-directory           (expand-file-name ".cache/gnus/" user-emacs-directory) t)
    (setq gnus-home-directory (expand-file-name ".cache/gnus/" user-emacs-directory))
    (setq gnus-directory      (expand-file-name "News/" gnus-home-directory))
    (use-package nndraft
      :defer t
      :config
      (progn
        (setq nndraft-directory (expand-file-name "drafts/" gnus-directory)))))
  (progn
    (setq gnus-asynchronous t)
    ;; https://lars.ingebrigtsen.no/2020/01/15/news-gmane-org-is-now-news-gmane-io/
    ;; (setq gnus-select-method '(nntp "news.gmane.org"))
    (setq gnus-select-method '(nntp "news.gmane.io"))))

(defun lotus-mailnews/common-config-gnus ()
  ;; (debug)
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

(defun lotus-mailnews/post-init-gnus-init ()
  (lotus-mailnews/common-init-gnus))

(defun lotus-mailnews/post-init-gnus-config ()
  (interactive)
  (lotus-mailnews/common-init-gnus)
  (lotus-mailnews/common-config-gnus)
  (progn
    (setq gnus-interactive-exit t)
    (progn
      (gnus-delay-initialize))
    (progn
      ;; If non-nil, the startup message won't be displayed. That way, your
      ;; boss might not notice that you are reading news instead of doing
      ;; your job.
      (setq gnus-inhibit-startup-message t))
    (progn
      (use-package gnus-start
        :defer t
        :config
        (progn
          (setq gnus-startup-file         (expand-file-name ".cache/gnus/newsrc" user-emacs-directory)
                gnus-read-active-file     nil
                gnus-check-new-newsgroups nil ; 'ask-server
                gnus-save-newsrc-file     t))))
    (progn
      (use-package simple
        :defer t
        :config
        (progn
          (progn
            (setq mail-user-agent 'gnus-user-agent)))))

    (progn
      (setq gnus-init-file "~/.gnus.el")
      (make-directory      (expand-file-name ".cache/gnus/" user-emacs-directory) t)
      (setq
       gnus-home-directory (expand-file-name ".cache/gnus/" user-emacs-directory))
      (use-package nnheader
        :defer t
        :config
        (progn
          (progn
            (setq
             gnus-directory      (nnheader-concat gnus-home-directory "News/")))
          (progn
            (use-package nndraft
              :defer t
              :config
              (progn
                (setq nndraft-directory (nnheader-concat gnus-directory      "drafts/"))))))))

    (use-package gnus-gravatar
      :defer t
      :config
      (progn
        (add-hook
         'gnus-article-prepare-hook
         'gnus-treat-mail-gravatar)))

    (progn
      ;; gnus-sum not called
      (lotus-mailnews/init-gnus-sum-config))))

(defun lotus-mailnews/init-nndraft-config ()
  (use-package nnheader
    :defer t
    :config
    (progn
      (setq nndraft-directory (nnheader-concat gnus-directory "drafts/")))))

(defun lotus-mailnews/init-gnus-gravatar-config ()
  (use-package gnus-gravatar
    :defer t
    :config
    (progn
      (add-hook
       'gnus-article-prepare-hook
       'gnus-treat-mail-gravatar))))

(defun lotus-mailnews/post-init-bbdb-config ()
  (progn
    (unless (fboundp 'bbdb-insinuate-gnus)
      (defun bbdb-insinuate-gnus ()
        (bbdb-initialize 'gnus)))
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

  (progn
    (setq bbdb-file (expand-file-name "bbdb/bbdb" "~/.emacs.d/.cache/"))

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

    (setq bbdb-use-pop-up t             ;TODO:
          bbdb-save-db-timeout 0) ;; I want it
    (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
    (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer-for-some-time)

    (defun toggle-bbdb-use-pop-up ()
      (interactive)
      (setq
       bbdb-use-pop-up (not bbdb-use-pop-up)))))

(defun lotus-mailnews/post-init-lsdb-config ()
  (progn
    (setq lsdb-file (lotus-cache-file "lsdb/lsdb"))))

(defun lotus-mailnews/init-shimbun-config ()
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
            ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t)))))

(defun lotus-mailnews/init-notmuch-config ()
  (progn
    (setq notmuch-fcc-dirs nil))
  (progn
    ;; http://notmuchmail.org/emacstips/
    (setq notmuch-address-command (expand-file-name "notmuch-addrlookup" "~/bin"))
    (if (file-exists-p notmuch-address-command)
        (notmuch-address-message-insinuate))))

(defun lotus-mailnews/init-gnus-win-config ()
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
                             (summary 1.0 point)))))))



(defun lotus-mailnews/init-gnus-sum-config ()
  (progn
    (add-hook 'gnus-summary-mode-hook
               #'(lambda ()
                   (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
                   (local-set-key "="           'lotus-toggle-article-window)
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
            (t . "%b %d '%y"))))
  (progn
    (setq gnus-user-date-format-alist
          '(((gnus-seconds-today) . " %k:%M") ;dans la journée = 14:39
            ((+ 86400 (gnus-seconds-today)) . "yesterday %k:%M")
                                        ;hier = hier 14:39
            ((+ 604800 (gnus-seconds-today)) . "%a %k:%M")
                                        ;dans la semaine = sam 14:39
            ((gnus-seconds-month) . "%a %d") ;ce mois = sam 28
            ((gnus-seconds-year) . "%b %d") ;durant l'année = mai 28
            (t . "%b %d '%y"))))
  (progn
    (defvar lotus-gnus-summary-order 'down)

    (defun lotus-gnus-summary-applied-order ()
      (if (member 'gnus-thread-sort-by-most-recent-date
                  gnus-thread-sort-functions)
          'top
        (if (some #'(lambda (e)
                      (when (consp e)
                        (eq 'not (car e))))
                  gnus-thread-sort-functions)
            'top
          'down)))

    (add-hook 'gnus-summary-prepare-hook
              #'(lambda ()
                  (unless (gnus-summary-first-subject t)
                    (if (eq (lotus-gnus-summary-applied-order) 'top)
                        (beginning-of-buffer)
                      (progn (end-of-buffer)
                             (forward-line -1))))))

    (add-hook 'gnus-summary-prepared-hook
              #'(lambda ()
                  (unless (gnus-summary-first-subject t)
                    (if (eq (lotus-gnus-summary-applied-order) 'top)
                        (beginning-of-buffer)
                      (progn (end-of-buffer)
                             (forward-line -1))))))

    (setq gnus-summary-thread-gathering-function
          'gnus-gather-threads-by-references)

    ;; (setq gnus-user-date-format-alist
    ;;       '(((gnus-seconds-today) . "    %k:%M")
    ;;         (604800 . "%a %k:%M")
    ;;         ((gnus-seconds-month)
    ;;          . "%a %d")
    ;;         ((gnus-seconds-year)
    ;;          . "%b %d")
    ;;         (t . "%b %d '%y")))

    ;; Sort threads by the date of the root node.
    ;; (setq gnus-thread-sort-functions `(gnus-thread-sort-by-date))
    ;; (setq gnus-thread-sort-functions
    ;;       '(gnus-thread-sort-by-date
    ;;         gnus-thread-sort-by-number))
    ;;         ;; gnus-thread-sort-by-subject
    ;;         ;; gnus-thread-sort-by-total-score
    ;; (setq gnus-thread-sort-functions
    ;;       '(gnus-thread-sort-by-date
    ;;         gnus-thread-sort-by-number
    ;;         gnus-thread-sort-by-subject
    ;;         gnus-thread-sort-by-total-score))
    ;; (setq gnus-thread-sort-functions
    ;;       '((not gnus-thread-sort-by-date)
    ;;         gnus-thread-sort-by-number
    ;;         gnus-thread-sort-by-subject
    ;;         (not gnus-thread-sort-by-total-score)))
    (if (eq 'down lotus-gnus-summary-order)
        (setq gnus-thread-sort-functions
              '(gnus-thread-sort-by-date
                gnus-thread-sort-by-number))
      (setq gnus-thread-sort-functions
            '(gnus-thread-sort-by-number
              gnus-thread-sort-by-most-recent-date))))

  (progn
    (use-package gnus-win
      :defer t
      :config
      (progn
        ;; Add info configuration also for function `gnus-info-find-node'
        ;; gnus-buffer-configuration
        (defun lotus-toggle-article-window ()
          (interactive)
          (let ((article-buffer (car
                                 (remove-if-not #'(lambda (bn)
                                                    (string-match "*Article" bn 0))
                                                (mapcar #'buffer-name (buffer-list))))))
            (if (and article-buffer
                     (get-buffer-window article-buffer nil))
                (gnus-configure-windows 'summary 'force)
              (gnus-configure-windows 'article 'force))))))))

(defun lotus-mailnews/init-gnus-msg-config ()
  (progn
    (setq gnus-gcc-mark-as-read t
          gnus-gcc-externalize-attachments nil))
  (progn
    ;; http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus

    (defadvice gnus-summary-reply (after formalities () activate)
      ;; (cjb-add-formalities)
      "Thanks."
      (dummy-add-formalities)))
  (progn
    (setq gnus-posting-styles (lotus-gnus-posting-style-function))))

(defun lotus-mailnews/init-gnus-art-config ()
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
     gnus-treat-body-boundary              'head
     gnus-treat-date-lapsed                'head
     gnus-treat-display-x-face             'head
     gnus-treat-strip-cr                   2
     gnus-treat-strip-leading-blank-lines  t
     gnus-treat-strip-multiple-blank-lines t
     gnus-treat-strip-trailing-blank-lines t
     gnus-treat-unsplit-urls               t

     gnus-treat-date-english               'head
     gnus-treat-date-iso8601               'head
     gnus-treat-date-lapsed                'head
     gnus-treat-date-local                 'head
     gnus-treat-date-original              'head
     gnus-treat-date-user-defined          'head
     gnus-treat-date-ut                    'head
     gnus-treat-date-original              'head
     ;; Make sure Gnus doesn't display smiley graphics.
     gnus-treat-display-smileys            t
     gnus-treat-hide-boring-headers        'head
     gnus-treat-hide-signature             nil ;; (unless (equal (system-name) office-host-name) 'last)
     gnus-treat-strip-banner               t))
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
          '("^Cc:"
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
            "^Organization:"))))

(defun lotus-mailnews/init-nnmail-config ()
  (setq nnmail-extra-headers '(To Newsgroups Content-Type Date)))

(defun lotus-mailnews/init-gnus-pers-config ()
  (defvar gnus-personality-activate nil "")
  (gnus-personality-init/sharad))

(defun lotus-mailnews/init-gnus-namazu-config ()
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
        (add-hook 'midnight-hook 'xsteve-gnus-update-namazu-index)))))

(defun lotus-mailnews/init-gnus-dired-config ()
  (progn
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(defun lotus-mailnews/init-gnus-demon-init ()
  (setq gnus-use-demon t)
  (setq gnus-demon-timestep 10))

(defun lotus-mailnews/init-gnus-demon-config ()
  (progn
    (setq gnus-use-demon      t)
    (setq gnus-demon-timestep 8)
    ;; Initialize the Gnus daemon, check new mail every six minutes.
    ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news 1 nil))
    ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 2 nil)
    (gnus-demon-add-handler #'gnus-demon-scan-mail-and-news-now 10 10)
    (gnus-demon-add-handler #'(lambda ()
                                (message "running gnus demon")
                                (gnus-demon-scan-mail-and-news-now 6))
                            22 20)))

(defun lotus-mailnews/init-message-config ()
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
      (when (looking-at "Hi")
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
      :init
      (progn
        (add-hook 'message-send-hook
                  'ispell-message)
        ;; In your ~/.gnus.el, if you prefer on-the-fly spell-checking say
        (add-hook 'message-mode-hook
                  #'(lambda ()
                      (flyspell-mode 1))))
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
        (add-hook 'message-mode-hook
                  #'(lambda ()
                      (footnote-mode 1))))))
  (progn
    (add-hook 'message-mode-hook ;          'turn-on-auto-fill)
              #'(lambda ()
                  (turn-on-auto-fill)
                  (setq fill-column 70)))
    (setq message-generate-headers-first t)
    (setq message-kill-buffer-on-exit    t)
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
       message-sendmail-envelope-from 'header)))
  (progn
    (setq message-from-style nil))
  (progn
    (use-package gnus
      :defer t
      :config
      (progn ;; https://www.emacswiki.org/emacs/MessageMode
        (setq message-auto-save-directory (expand-file-name "drafts/" gnus-directory))))))

(defun lotus-mailnews/init-sendmail-config ()
  (progn
    (setq mail-specify-envelope-from t
          mail-envelope-from 'header))
  (progn
    (setq send-mail-function 'sendmail-send-it)))

(defun lotus-mailnews/init-dbus-config ()
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
  (add-hook 'gnus-exit-gnus-hook 'gnus-nm-disable))

(defun lotus-mailnews/init-mailcrypt-config ()
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
        (mc-sign-message))))

(defun lotus-mailnews/init-nnheader-config ()
  (setq gnus-nov-is-evil nil)
  (use-package nnheader
    :defer t
    :config
    (progn
      (setq gnus-directory    (nnheader-concat gnus-home-directory "News/")
            nndraft-directory (nnheader-concat gnus-directory      "drafts/")))))

(defun lotus-mailnews/init-gnus-group-config ()
  (progn
    (global-set-key-if-unbind (kbd "H-s") 'gnus-group-save-newsrc))
  (progn
    (setq  gnus-invalid-group-regexp "[:`'\"]\\|^$"))
  (progn
    ;;Face http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC20

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
              (t . my-group-face-5)))))
  (progn
    (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
    (setq gnus-group-line-format
                                        ;"%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")
          "%M\%S\%p\%P\%5y: %(%-100,100g%) %6,6~(cut 2)d\n"))
  (progn
    (setq gnus-permanently-visible-groups ".*INBOX")
    (define-key gnus-group-mode-map "b" 'gnus-group-get-new-news)
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))
  (progn
    (use-package shimbun
      :defer t
      :config
      (progn
        (setq nnshimbun-group-parameters-alist
              '(
                ("^nnshimbun.*:" index-range all prefetch-articles off
                 encapsulate-images on expiry-wait 6))))))
  (progn
    ;;{{Exiting http://www.stanford.edu/~rgm/comp/dotgnus.html

    (defvar my-before-kill-emacs-hook nil)

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
              '("bbdb" "*BBDB*" "*Compile-Log*" "posts")))
    ;;   (let ((gnus-startup-jingle
    ;;          (expand-file-name
    ;;           "Library/WindowMaker/Sounds/Windows/chimes.wav"
    ;;           (or (getenv "GNUSTEP_USER_ROOT") "~/GNUstep"))))
    ;;     (gnus-play-jingle))

    (add-hook 'gnus-after-exiting-gnus-hook 'my-gnus-after-exiting-gnus-hook-fn))
  (progn
    (add-hook 'gnus-group-mode-hook 'gnus-demon-init)
    (add-hook 'gnus-exit-gnus-hook 'gnus-demon-cancel)))

(defun lotus-mailnews/init-mm-decode-config ()
  (progn
    (use-package diary-lib
      :defer t
      :config
      (progn
        ;; diary-from-outlook-gnus is an interactive compiled Lisp function in
        ;; `diary-lib.el'.

        ;; (diary-from-outlook-gnus &optional NOCONFIRM)

        ;; Maybe snarf diary entry from Outlook-generated message in Gnus.
        ;; Unless the optional argument NOCONFIRM is non-nil (which is the case when
        ;; this function is called interactively), then if an entry is found the
        ;; user is asked to confirm its addition.
        ;; Add this function to `gnus-article-prepare-hook' to notice appointments
        ;; automatically.

        ;; (require 'mm-decode)
        ;; (require 'mm-util)

        (defun diary-from-outlook-gnus-safe ()
          (ignore-errors
            (diary-from-outlook-gnus)))
        (remove-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus)
        ;; this function `diary-from-outlook-gnus'
        ;; when failed with error "no buffer name with multipart/related"
        ;; it left article in the end, so I have to remove it.
        (add-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus-safe)

        ;; using icalendar.el wotrking

        (progn
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
           '("text/calendar" . my-save-icalendar)))))))

(defun lotus-mailnews/init-todo-gnus-config ()
  (progn
    ;; Also it could be usefull to see always all todo items,
    ;; regardless if they are marked as unread or read:
    (pushnew '(("^nntodo+"
                (display . all)))
             gnus-parameters))
  (progn
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

    (setq gnus-permanently-visible-groups "^nntodo+")))

(defun lotus-mailnews/init-rs-gnus-exts-config ()
  (progn
    (rs-gnus-summary-tree-arrows-01))
  (progn ;; deh-require-maybe gnus-summary-stripe
    (setq gnus-summary-stripe-regexp "^.+│.+│.+│"))
  (progn
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

    (defalias 'gnus-user-format-function-ct    'rs-gnus-summary-line-content-type)
    (defalias 'gnus-user-format-function-size  'rs-gnus-summary-line-message-size)
    (defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
    (defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
    ;;
    (setq gnus-balloon-face-0                  'rs-gnus-balloon-0
          gnus-balloon-face-1                  'rs-gnus-balloon-1
          gnus-face-1                          'rs-gnus-face-1)

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
           (nl "\n"))
      ;;(bugzilla-who "%4{%-20,20ub%}")

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
             (encapsulate-images t))))))

(defun lotus-mailnews/init-gnus-start-config ()
  (progn
    (setq
     ;; see http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC13

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

     gnus-dribble-directory (expand-file-name ".cache/gnus/gnus-data" user-emacs-directory)
     gnus-always-read-dribble-file t
                                        ;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC12
     ;; You can turn off writing the `.newsrc' file by setting
     ;; gnus-save-newsrc-file to nil, which means you can delete the file
     ;; and save some space, as well as exiting from Gnus faster. However,
     ;; this will make (insert )t impossible to use other newsreaders than
     ;; Gnus. But hey, who would want to, right?
     gnus-save-newsrc-file nil)))

(defun lotus-mailnews/init-host-info-config ()
  ())

(defun lotus-mailnews/init-common-info-config ()
  ())

(defun lotus-mailnews/init-passwds-config ()
  ())

(defun lotus-mailnews/init-mu4e-message-config ()
  ())

(defun lotus-mailnews/init-mu4e-view-init ()
  (progn
    (defvar mu4e~view-message)))

(defun lotus-mailnews/init-mu4e-view-config ()
  ())

;;; config.el ends here
