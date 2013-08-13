(in-package :stumpwm)

(defvar *newmail-timer* nil
  "Runs the mail checker.")

(defvar my-emails nil
  "The previous formatted contents of the mail spool.")

(defun my-mail-popup (emails)
  "Displays a summary of all new email."
  (let ((summary nil))
    ;; Create the text for the summary
    (if emails
        (setq summary (concatenate 'string (format nil "^6*System mailbox (~a)^n~% ~%"
                                                   (length (split-string emails))) emails))
        (setq summary "^6*System mailbox (0)"))
    ;; Display the summary
    (message "~a" summary)))

(defun my-get-mail ()
  "Returns the formatted contents of the mail spool."
  (let ((mail nil)
        (lines (run-prog-collect-output
                "/bin/grep" "-e" "^Subject:"
                (concat "/var/spool/mail/" *login-user*))))
    (when (not (string= lines ""))
      ;; Split the subjects by newline
      (setq lines (split-string lines))
      ;; Add each of the subject lines
      (dolist (line lines)
        (setq mail (concatenate 'string mail (format nil "~a~%" (subseq line 9))))))
    mail))

(defun my-check-mail ()
  "Displays the mail popup if there's new email in the spool."
  (let ((newmail (my-get-mail)))
    (when (and newmail (not (string= my-emails newmail)))
      (my-mail-popup newmail))
    (setq my-emails newmail)))

(defcommand mail () ()
  "Displays the mail popup."
  (setq my-emails (my-get-mail))
  (my-mail-popup my-emails))

(defun my-stop-newmail-timer ()
  "Stops the newmail timer."
  (ignore-errors
    (cancel-timer *newmail-timer*)))

(defun my-start-newmail-timer ()
  "Starts the newmail timer."
  (my-stop-newmail-timer)
  (setf *newmail-timer* (run-with-timer 10 10 'my-check-mail)))

(defcommand mailstart () ()
  "Starts the newmail timer."
  (my-start-newmail-timer))

(defcommand mailstop () ()
  "Stops the newmail timer."
  (my-stop-newmail-timer))

