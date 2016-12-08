;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://tsdh.wordpress.com/2008/07/03/some-calendar-and-org-mode-integration-stuff/
;; Some calendar and org-mode integration stuff
;; 03Jul08

;; The following code binds RET in calendar mode to a function that opens
;; an org agenda buffer for that day (or better that week).









(defvar configuration|common|cal-org-config|package-list nil)


;;;###autoload
(defun configuration|common|cal-org-config|calendar|config ()
  (use-package org-agenda
      :defer t
      :config
      (defun th-calendar-open-agenda ()
        (interactive)
        (let* ((calendar-date (or
                               ;; the date at point in the calendar buffer
                               (calendar-cursor-to-date)
                               ;; if there's none, use the curren date
                               (calendar-current-date)))
               (day (time-to-days (encode-time 1 1 1
                                               (second calendar-date)
                                               (first calendar-date)
                                               (third calendar-date))))
               (calendar-buffer (current-buffer)))
          (org-agenda-list nil day)
          (select-window (get-buffer-window calendar-buffer))))

      (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)

      ;; And heres a small minor mode which uses the function above to refresh
      ;; the agenda buffer when you move point in the calendar buffer, so
      ;; calendar and agenda stay in sync.
      (define-minor-mode th-org-agenda-follow-calendar-mode
          "If enabled, each calendar movement will refresh the org agenda
buffer."
        :lighter " OrgAgendaFollow"
        (if (not (eq major-mode 'calendar-mode))
            (message "Cannot activate th-org-agenda-follow-calendar-mode in %s." major-mode)
            (if th-org-agenda-follow-calendar-mode
                (add-hook 'calendar-move-hook 'th-calendar-open-agenda)
                (remove-hook 'calendar-move-hook 'th-calendar-open-agenda))))

      (add-hook 'calendar-mode-hook 'th-org-agenda-follow-calendar-mode)
      )

  ;; Another thing I added to calendar is the display of the week-of-year
  ;; in the mode-line.
  (add-to-list 'calendar-mode-line-format
               '(let ((day (nth 1 date))
                      (month (nth 0 date))
                      (year (nth 2 date)))
                 (format-time-string "Week of year: %V"
                  (encode-time 1 1 1 day month year))))
  ;;Possibly related posts: (automatically generated)
  )

;;;###autoload
(defun configuration|common|cal-org-config|calendar|init ()
    (use-package calendar
      :defer t
      :config
      (configuration|common|cal-org-config|calendar|config)))
(push 'calendar configuration|common|cal-org-config|package-list)



;;;###autoload
(defun configuration|common|cal-org-config|config ()
  configuration|common|cal-org-config|package-list)
;;;###autoload
(defun configuration|common|cal-org-config|init ()
  (configuration|common|cal-org-config|config))

;;;###autoload
(defun configuration|common|cal-org-config|packages ()
  configuration|common|cal-org-config|package-list)



(provide 'cal-org-config)
