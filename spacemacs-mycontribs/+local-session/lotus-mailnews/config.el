;;; config.el --- config                             -*- lexical-binding: t; -*-


(defmacro lotus-fix-later (&rest body)
  `(when nil
     ,@body))

(let ((osetup
       (expand-file-name
        ".repos/git/main/resource/userorg/main/readwrite/public/user/osetup" "~")))
  (push (expand-file-name "info.d/common/elisp" osetup) load-path)
  (let ((default-local-lib
          (expand-file-name "info.d/hosts/default/elisp" osetup))
        (local-lib
         (expand-file-name (concat "info.d/hosts/" (system-name) "/elisp") osetup)))
    (push
     (if (file-directory-p local-lib)
         local-lib
       default-local-lib)
     load-path)))

(require 'host-info)
(require 'common-info)
(require 'passwds)



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
        (message-id (replace-regexp-in-string
                     "^id:" "" (notmuch-show-get-message-id))))
    (if (and group message-id)
        (org-gnus-follow-link group message-id)
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


;;}}

;;{{

;;}}

;;{{
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
              (insert (concat "<#part type=\"" mime-type "\" filename=\"" full-file-path "\" disposition=" disposition ">\n"))))))))
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
  (let* ((region-active (and (region-active-p)
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
                    '(lambda ()
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

;; (require 'sigbegone)

;;; config.el ends here
