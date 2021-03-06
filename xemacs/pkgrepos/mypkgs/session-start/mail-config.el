

(xrequire 'nnir)



;;{{ from remember-experimental

(deh-section "Compose mail."
  (defvar remember-mail-recipient nil
    "People to mail remember entries to.
If this is a string, mail will be sent to that recipient.
If this is a function, the function should return a list of e-mail addresses.")

  (setq remember-mail-recipient "sacha@free.net.ph")
  (defvar remember-blog-title "Sacha's journal" "Text to prepend to subjects.")
  (defun remember-mail-subject (title)
    "Return a reasonable subject based on TITLE."
    (concat remember-blog-title (format-time-string " (%Y.%m.%d %R): ") title))

  (defun remember-compose-mail (title entry)
    "Compose a message for TITLE containing ENTRY."
    (compose-mail "Undisclosed recipients" (remember-mail-subject title)
                  '(("Bcc" . "harvey@adphoto.com.ph")))
    (rfc822-goto-eoh)
    (forward-line 1)
    (insert title "\n"
            "----------------------------------------------------------------\n"
            entry "\n"
            "\n"
            "This message was automatically generated by remember.el.\n"
            "Blog is at http://sacha.free.net.ph/notebook/wiki/today.php\n"
            "and this entry is at http://sacha.free.net.ph/notebook/wiki/"
            (planner-today) ".php .\n"))

  ;; (remember-compose-mail "Title" "Entry goes here.")

  ;; mail-mode-hook
  )



;; remember-config.el:10:1:Warning: cl package required at runtime
;; Warning (mail): The default mail mode is now Message mode.
;; You have the following Mail mode variable customized:

;;   mail-mode-hook

;; To use Mail mode, set `mail-user-agent' to sendmail-user-agent.
;; To disable this warning, set `compose-mail-user-agent-warnings' to nil.
;; Parsing /home/s/hell/.mailrc... done
;; remember-config.el:22:1:Warning: `save-excursion' defeated by `set-buffer'

;; In end of data:
;; remember-config.el:219:1:Warning: the following functions are not known to be defined:
;;     feature-symbols, lotus-remember-fun-set-orgnizer,
;;     lotus-remember-set-orgnizer, reader-mode


;;}}


(provide 'mail-config)
