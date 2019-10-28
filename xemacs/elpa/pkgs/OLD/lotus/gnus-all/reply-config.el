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


(provide 'reply-config)
