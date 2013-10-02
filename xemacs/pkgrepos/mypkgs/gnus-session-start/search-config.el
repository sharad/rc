
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

(provide 'search-config)
