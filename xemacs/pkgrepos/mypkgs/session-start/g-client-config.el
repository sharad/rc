;; http://bc.tech.coop/blog/070306.html
(deh-require-maybe g
  (setq g-user-email "sh4r4d _at_ _G-mail_")
  (setq g-html-handler 'w3m-buffer)

  ;; Write to Diary any entry added to Google Calendar
  (eval-after-load "gcal"
    '(progn
      (defun gcal-read-event (title content
                              where
                              start end
                              who
                              transparency status)
        "Prompt user for event params and return an event structure."
        (interactive
         (list
          (read-from-minibuffer "Title: ")
          (read-from-minibuffer "Content: ")
          (read-from-minibuffer "Where: ")
          (gcal-read-calendar-time "Start Time: ")
          (gcal-read-calendar-time "End Time: ")
          (gcal-read-who "Participant: ")
          (gcal-read-transparency)
          (gcal-read-status)))
        (save-excursion
          (let ((pop-up-frames (window-dedicated-p (selected-window))))
            (find-file-other-window (substitute-in-file-name diary-file)))
          (when (eq major-mode default-major-mode) (diary-mode))
          (widen)
          (diary-unhide-everything)
          (goto-char (point-max))
          (when (let ((case-fold-search t))
                  (search-backward "Local  Variables:"
                                   (max (- (point-max) 3000) (point-min))
                                   t))
            (beginning-of-line)
            (insert "n")
            (forward-line -1))
          (let* ((dayname)
                 (day (substring start 8 10))
                 (month (substring start 5 7))
                 (year (substring start 0 4))
                 (starttime (substring start 11 16))
                 (endtime (substring end 11 16))
                 (monthname (calendar-month-name (parse-integer month) t))
                 (date-time-string (concat (mapconcat 'eval calendar-date-display-form "")
                                           " " starttime " - " endtime)))
            (insert
             (if (bolp) "" "n")
             ""
             date-time-string " " title))
          (bury-buffer)
          (declare (special gcal-auth-handle))
          (let ((event (make-gcal-event
                        :author-email (g-auth-email gcal-auth-handle))))
            (setf (gcal-event-title  event) title
                  (gcal-event-content event) content
                  (gcal-event-where event) where
                  (gcal-event-when-start event) start
                  (gcal-event-when-end event) end
                  (gcal-event-who event) who
                  (gcal-event-transparency  event) transparency
                  (gcal-event-status event) status)
            event))))))


(provide 'g-client-config)

