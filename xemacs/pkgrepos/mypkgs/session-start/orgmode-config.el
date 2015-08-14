

(deh-require-maybe (and org-compat org)
  ;; publishing

  ;;
  ;; (deh-require-maybe (progn org-publish org-html))
  ;; (defconst org-export-html-special-string-regexps
  ;;   '(("\\\\-" . "&shy;")
  ;;     ("---\\([^-]\\)" . "&mdash;\\1")
  ;;     ("--\\([^-]\\)" . "&ndash;\\1")
  ;;     ("\\.\\.\\." . "&hellip;"))
  ;;   "Regular expressions for special string conversion.")


  (deh-section "time management"

    (require 'org-timer)

    ;; http://orgmode.org/worg/org-gtd-etc.html
    (add-to-list 'org-modules 'org-timer)
    (add-to-list 'org-modules 'org-clock)
    (deh-section "miscellaneous"
     (setq
      org-timer-default-timer 25
      org-clock-persist-file  (auto-config-file "org/clock/org-clock-save.el")
      org-log-note-clock-out t           ;excellent, great
      org-clock-clocked-in-display 'both ;; ('mode-line 'frame-title 'both)
      org-clock-idle-time 5 ;; minutes
      org-clock-resolve-expert nil ;; good
      org-clock-sound t ;; could be file name
      ;; org-clock-current-task
      ;; org-clock-heading
      org-clock-history-length 100
      ;; org-clock-marker
      ;; org-clock-history
      org-clock-persist t
      ;; org-clock-out-switch-to-state ;; good
      ;; org-clock-in-switch-to-state
      org-clock-out-remove-zero-time-clocks t)

     ;; (find
     ;;   org-clock-leftover-time
     ;;   org-clock-default-task ;; M-x org-clock-mark-default-task
     ;;   M-x org-clock-select-task
     ;; (org-clocking-buffer)
     ;; (org-clock-sum-today)
     ;; (org-clock-sum-custom nil 'today)
     ;; (org-clock-is-active)
     ;; )

     (add-hook 'org-clock-in-hook
               (lambda ()
                 ;; if effort is not present tahnadd it.
                 (unless (org-entry-get nil "Effort")
                   (org-set-effort))
                 ;; set timer
                 (when (not
                        (and
                         (boundp' org-timer-countdown-timer)
                         org-timer-countdown-timer))
                   (if (org-entry-get nil "Effort")
                       (save-excursion
                         (forward-line -2)
                         (org-timer-set-timer nil))
                       (call-interactively 'org-timer-set-timer)))))

     (add-hook 'org-clock-out-hook
               (lambda ()
                 (if (and
                      (boundp' org-timer-countdown-timer)
                      org-timer-countdown-timer)
                     (org-timer-stop))))

     (defun org-clock-in-if-not ()
       (interactive)
       (unless (org-clock-is-active)
         ;; (org-clock-goto t)
         (let (buffer-read-only)
           (org-clock-in '(4)))))

     (add-hook 'sharad/enable-startup-interrupting-feature-hook
               '(lambda ()

                 (add-hook 'after-make-frame-functions
                  '(lambda (nframe)
                    (org-clock-in-if-not)) t)

                 (add-hook 'delete-frame-functions
                  '(lambda (nframe)
                    (if (and
                         (org-clock-is-active)
                         (y-or-n-p-with-timeout
                          (format "Do you want to clock out current task %s: " org-clock-heading)
                          7
                          nil))
                        (let (org-log-note-clock-out
                              buffer-read-only)
                          (org-clock-out)))))

                 (if (fboundp 'org-clock-persistence-insinuate)
                     (org-clock-persistence-insinuate)
                     (message "Error: Org Clock function org-clock-persistence-insinuate not available."))
                 (if (fboundp 'org-clock-start-check-timer)
                     (org-clock-start-check-timer)))

               t)

     (defmacro org-with-clock-position (clock &rest forms)
       "Evaluate FORMS with CLOCK as the current active clock."
       `(with-current-buffer (marker-buffer (car ,clock))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (car ,clock))
              (beginning-of-line)
              (let (buffer-read-only)
                ,@forms)))))
     ) ;; deh-section "miscellaneous"

    (deh-section "today time"
      (defvar org-clock-work-day-hours 8 "work day hours")

      (defvar org-clock-monitor-files nil
        "org clock monitor files")

      (defvar org-clock-monitor-files-mins-aggregate-internal nil
        "org clock monitor files")

      (defvar org-clock-work-day-start 10)
      (defvar org-clock-work-day-lunch-break-hour 1)
      (defvar org-clock-work-day-end (+ org-clock-work-day-start org-clock-work-day-lunch-break-hour org-clock-work-day-hours))

      (defvar org-clock-work-day-msg nil)

      (defvar org-clock-work-day-mode-line-map (make-sparse-keymap))
      (defvar org-mode-work-day-mode-line-string "" "Hello")
      (put 'org-mode-work-day-line-string 'risky-local-variable t)
      ;; (get 'org-mode-work-day-line-string 'risky-local-variable)
      ;; (get 'org-mode-line-string 'risky-local-variable)

      (defvar org-clock-get-work-day-clock-string-separator nil)
      (defvar org-mode-work-mode-line-timer nil)
      (defvar org-mode-work-day-pause nil)
      (defvar org-mode-work-day-display 'mode-line)

      (defface org-mode-line-wday
          '((t (:inherit mode-line)))
        "Face used for clock display in mode line."
        :group 'org-faces)

      (defface org-mode-line-wday-underrun
          '((t (:inherit mode-line :foreground "Green")))
        "Face used for clock display for overrun tasks in mode line."
        :group 'org-faces)

      (defface org-mode-line-wday-overrun
          '((t (:inherit mode-line :background "red")))
        "Face used for clock display for overrun tasks in mode line."
        :group 'org-faces)

      (defvar org-work-day-face 'org-mode-line-clock)
      (defvar org-work-day-face-overrun 'org-mode-line-clock-overrun)


      (setq
       org-work-day-face          'org-mode-line-wday
       org-work-day-face-underrun 'org-mode-line-wday-underrun
       org-work-day-face-overrun  'org-mode-line-wday-overrun)

      ;; (setq
      ;;  org-work-day-face          'org-mode-line-clock
      ;;  org-work-day-face-underrun 'org-mode-line-wday-overrun
      ;;  org-work-day-face-overrun  'org-mode-line-clock-overrun)

      (setq org-clock-monitor-files
            (list
             "/home/s/paradise/doc/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org"
             "/home/s/paradise/doc/CreatedContent/contents/org/tasks/meru/works/emacs/todo.org"))

      (defun org-clock-unclocked-files-mins-today (files)
        (let* ((totalmins 0))
          (org-agenda-prepare-buffers files)
          (while (setq file (pop files))
            (with-current-buffer (find-buffer-visiting file)
              (save-excursion
                (save-restriction
                  (incf totalmins (org-clock-sum-today))))))
          totalmins))

      (defun org-clock-clocked-secs-today ()
        "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
        (if (org-clock-is-active)
            (let ((currently-clocked-time
                   (floor (- (org-float-time)
                             (org-float-time org-clock-start-time)))))
              currently-clocked-time)
            0))

      (defun org-clock-files-secs (files &optional all)
        (+
         (org-clock-clocked-secs-today)
         (*
          60
          (if (or
               all
               (null org-clock-monitor-files-mins-aggregate-internal))
              (setq
               org-clock-monitor-files-mins-aggregate-internal
               (org-clock-unclocked-files-mins-today files))
              org-clock-monitor-files-mins-aggregate-internal))))

      (defun org-clock-files-min-today (&optional force)
        (interactive "P")
        (let* ((today-clock-secs (org-clock-files-secs org-clock-monitor-files force))
               (secs (- (* org-clock-work-day-hours 60 60) today-clock-secs))
               (remiain-today-clock-hms (org-timer-secs-to-hms secs)))
          (message "%s left for today." (format "%s" remiain-today-clock-hms))))


      (defun org-clock-work-day-start-secs ()
        (floor
         (org-float-time
          (apply 'encode-time
                 (append '(0 0 0) (cdddr (decode-time)))))))
      ;; (org-float-time org-clock-work-day-end)

      (define-key org-clock-work-day-mode-line-map [mode-line mouse-2] 'org-clock-files-min-today)
      (define-key org-clock-work-day-mode-line-map [mode-line mouse-1] 'org-clock-work-day-menu)

      (defun org-clock-work-day-menu ()
        (interactive)
        (popup-menu
         '("Clock"
           ["Clock out" org-clock-out t]
           ["Change effort estimate" org-clock-modify-effort-estimate t]
           ["Go to clock entry" org-clock-goto t]
           ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

      ;; TODO: optimize it.
      (defun org-clock-get-work-day-clock-string ()
        "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
        (let* ((now-sec             (floor (org-float-time)))
               (day-start-secs      (org-clock-work-day-start-secs))
               (work-day-start-secs (+ day-start-secs (* org-clock-work-day-start 60 60)))
               (work-day-end-secs   (+ day-start-secs (* org-clock-work-day-end 60 60)))
               (work-day-over-secs  (- now-sec work-day-start-secs))
               (work-day-left-secs  (- work-day-end-secs now-sec))
               (work-day-over-str   (org-timer-secs-to-hms work-day-over-secs))
               (work-day-left-str   (org-timer-secs-to-hms work-day-left-secs))
               (today-clocked-secs  (org-clock-files-secs org-clock-monitor-files))
               (today-dur-left-sec  (- (* org-clock-work-day-hours 60 60) today-clocked-secs))
               (today-dur-left-str  (org-timer-secs-to-hms today-dur-left-sec))
               (work-done-str
                (org-propertize
                 today-dur-left-str
                 'face (if (< today-dur-left-sec work-day-left-secs) ;; t ;; org-clock-task-overrun
                           org-work-day-face-underrun
                           org-work-day-face-overrun)))
               (work-day-time-str
                (org-minutes-to-clocksum-string (* org-clock-work-day-hours 60)))
               (clockstr (org-propertize
                          (concat
                           (if org-clock-get-work-day-clock-string-separator " " "")
                           "["
                           "%s %s/%s %s"
                           "]"
                           ;; (if org-clock-work-day-msg
                           ;;     (concat " (" (replace-regexp-in-string "%" "%%" org-clock-work-day-msg) ")"))
                           )
                          'face org-work-day-face)))
          (format clockstr
                  work-day-over-str
                  work-done-str
                  work-day-time-str
                  work-day-left-str)))

      (defun org-clock-work-day-update-mode-line-internal ()
      ;; (defun org-clock-work-day-update-mode-line ()
        (setq org-mode-work-day-mode-line-string
              (org-propertize

               (let ((clock-string (org-clock-get-work-day-clock-string))
                     (help-text
                      ;; "Org-mode clock is running.\nmouse-1 shows a menu\nmouse-2 will jump to task"
                      "Today's work clocks."
                       ))
                 (if (and (> org-clock-string-limit 0)
                          (> (length clock-string) org-clock-string-limit))
                     (org-propertize
                      (substring clock-string 0 org-clock-string-limit)
                      'help-echo (concat help-text ": " org-clock-heading))
                     (org-propertize clock-string 'help-echo help-text)))

               'local-map org-clock-work-day-mode-line-map
               'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
        (force-mode-line-update))
    ;;

    (defun org-clock-work-day-update-mode-line ()
      "Update the timer time in the mode line."
      (if org-mode-work-day-pause
          nil
          (org-clock-work-day-update-mode-line-internal)
          (force-mode-line-update)))

    (defun org-clock-work-day-mode-line-add ()
      (interactive)
      ;; (or global-mode-string (setq global-mode-string '("")))
      ;; (or (memq 'org-mode-work-day-mode-line-string global-mode-string)
      ;;     (setq global-mode-string
      ;;           (append global-mode-string
      ;;                   '(org-mode-work-day-mode-line-string))))

      (or global-mode-line-list (setq global-mode-string '("")))
      (or (memq 'org-mode-work-day-mode-line-string global-mode-line-list)
          (setq global-mode-line-list
                (append global-mode-line-list
                        '(org-mode-work-day-mode-line-string))))
      (when (eq org-mode-work-day-display 'mode-line)
        (org-clock-work-day-update-mode-line)
        (when org-mode-work-mode-line-timer
          (cancel-timer org-mode-work-mode-line-timer)
          (setq org-mode-work-mode-line-timer nil))
        (setq org-mode-work-mode-line-timer
              (run-with-timer 20 20 'org-clock-work-day-update-mode-line))))

      (defun org-clock-work-day-mode-line-remove ()
        (interactive)
        ;; (setq global-mode-string
        ;;       (delq 'org-mode-work-day-mode-line-string global-mode-string))
        (setq global-mode-line-list
              (delq 'org-mode-work-day-mode-line-string global-mode-line-list))
        (when org-mode-work-mode-line-timer
          (cancel-timer org-mode-work-mode-line-timer)
          (setq org-mode-work-mode-line-timer nil)))


      (org-clock-work-day-mode-line-add))


    (defun org-timer-update-mode-line ()
      "Update the timer time in the mode line."
      (if org-timer-pause-time
          nil
          (setq org-timer-mode-line-string
                (org-propertize
                 (concat " <" (substring (org-timer-value-string) 0 -1) ">")
                 'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
          (force-mode-line-update)))

    ;; (let ((range (org-clock-special-range 'today))
    ;;       (headline-filter))
    ;;   (org-clock-sum (car range) (cadr range)
    ;;                  headline-filter :org-clock-minutes-today))
    ;;
    ;;
    ;; (let ((range (org-clock-special-range 'today)))
    ;;   (org-clock-get-table-data
    ;;    "today.org"
    ;;    '(
    ;;      :tstart (car range)
    ;;      :tend   (cadr range)
    ;;      )))

    (deh-section "org clock timer checker"

     (defvar org-clock-check-timer-long-period 20
       "Period of Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defvar org-clock-check-timer-long nil
       "Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defvar org-clock-check-timer-short-period 7
       "Period Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defvar org-clock-check-timer-short nil
       "Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")


     (defun org-clock-start-check-timer ()
       (interactive)
       (setq
        org-clock-check-timer-long
        (run-with-idle-timer
         org-clock-check-timer-long-period
         org-clock-check-timer-long-period
         '(lambda (period)
           (unless (org-clock-is-active)
             (message "scheduleing after 2 sec.")
             (run-at-time 2 nil
                          '(lambda (period)
                            (when org-clock-check-timer-short
                              (cancel-timer org-clock-check-timer-short)
                              (setq org-clock-check-timer-short nil))
                            (setq
                             org-clock-check-timer-short
                             (run-with-idle-timer
                              period
                              nil
                              '(lambda ()
                                (message "after 7 sec.")
                                (let ((idle-time (ceiling (float-time (or (current-idle-time) (seconds-to-time 0))))))
                                  (message "idle time %s logn time %s" idle-time org-clock-check-timer-long-period)
                                  (message "(< idle-time org-clock-check-timer-long-period) %s (org-clock-is-active) %s"
                                           (< idle-time org-clock-check-timer-long-period)
                                           (org-clock-is-active))
                                  (unless (org-clock-is-active)
                                    (if (< idle-time org-clock-check-timer-long-period)
                                        (org-clock-in-if-not)
                                        (message "Not reminding to clock-in as it is idle for long %s secs time." idle-time))))))))
                          period)))
         org-clock-check-timer-short-period)))

     (defun org-clock-stop-check-timer ()
       (interactive)
       (progn
         (when org-clock-check-timer-short
           (cancel-timer org-clock-check-timer-short)
           (setq org-clock-check-timer-short nil))
         (when org-clock-check-timer-long
           (cancel-timer org-clock-check-timer-long)
           (setq org-clock-check-timer-long nil)))))

    (deh-section "correction org-timer.el"
      (defun org-timer-set-timer (&optional opt)
        "Prompt for a duration in minutes or hh:mm:ss and set a timer.

If `org-timer-default-timer' is not \"0\", suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument, use this numeric value as
the duration of the timer in minutes.

Called with a `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration.

With two `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration and automatically
replace any running timer.

By default, the timer duration will be set to the number of
minutes in the Effort property, if any.  You can ignore this by
using three `C-u' prefix arguments."
        (interactive "P")
        (when (and org-timer-start-time
                   (not org-timer-countdown-timer))
          (user-error "Relative timer is running.  Stop first"))
        (let* ((default-timer
                ;; `org-timer-default-timer' used to be a number, don't choke:
                (if (numberp org-timer-default-timer)
                    (number-to-string org-timer-default-timer)
                    org-timer-default-timer))
               (clocked-time   (org-clock-get-clocked-time))
               (effort-minutes
                (or
                 (ignore-errors (org-get-at-eol 'effort-minutes 1))
                 (if (org-entry-get nil "Effort")
                     (org-timer-hms-to-secs (org-entry-get nil "Effort")))))
               (remianing-effort-minutes (if (and
                                              effort-minutes
                                              clocked-time
                                              (>= effort-minutes clocked-time))
                                             (- effort-minutes clocked-time)
                                             effort-minutes))
               (minutes (or (and (not (equal opt '(64)))
                                 effort-minutes
                                 (number-to-string remianing-effort-minutes))
                            (and (numberp opt) (number-to-string opt))
                            (and (consp opt) default-timer)
                            (and (stringp opt) opt)
                            (read-from-minibuffer
                             "How much time left? (minutes or h:mm:ss) "
                             (and (not (string-equal default-timer "0")) default-timer)))))
          (message "effort-minutes %s clocked-time %s remianing-effort-minutes %s" effort-minutes clocked-time remianing-effort-minutes)
          (when (string-match "\\`[0-9]+\\'" minutes)
            (let* ((mins (string-to-number minutes))
                   (h (/ mins 60))
                   (m (% mins 60)))
              (setq minutes (format "%02d:%02d" h m)))
            (setq minutes (concat minutes ":00")))
          (if (not (string-match "[0-9]+" minutes))
              (org-timer-show-remaining-time)
              (let ((secs (org-timer-hms-to-secs (org-timer-fix-incomplete minutes)))
                    (hl (org-timer--get-timer-title)))
                (if (or (not org-timer-countdown-timer)
                        (equal opt '(16))
                        (y-or-n-p "Replace current timer? "))
                    (progn
                      (when (timerp org-timer-countdown-timer)
                        (cancel-timer org-timer-countdown-timer))
                      (setq org-timer-countdown-timer-title
                            (org-timer--get-timer-title))
                      (setq org-timer-countdown-timer
                            (org-timer--run-countdown-timer
                             secs org-timer-countdown-timer-title))
                      (run-hooks 'org-timer-set-hook)
                      (setq org-timer-start-time
                            (time-add (current-time) (seconds-to-time secs)))
                      (setq org-timer-pause-time nil)
                      (org-timer-set-mode-line 'on))
                    (message "No timer set")))))))

    ) ;; time management


  (deh-section "org-agenda"
    (deh-require-maybe files-config

      (setq org-agenda-custom-commands
            ;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
            '(("P" "Project List"
               ((tags "PROJECT")))
              ("O" "Office"
               ((agenda)
                (tags-todo "OFFICE")))
              ("W" "Weekly Plan"
               ((agenda)
                (todo "TODO")
                (tags "PROJECT")))
              ("H" "Home NA Lists"
               ((agenda)
                (tags-todo "HOME")
                (tags-todo "COMPUTER")))
              ("Z" "Meru Today" ;; tags-todo "computer" ;; (1) (2) (3) (4)
               ((agenda ""
                        (;; (org-agenda-ndays 1)
                         (org-agenda-span 'day)
                         (org-agenda-prefix-format  "%e")))
                (org-agenda-files
                 (directory-files-recursive
                  (expand-file-name "~/Documents/CreatedContent/contents/org/tasks/meru")
                  "\\.org$"
                  2
                  "\\(rip\\|stage\\)"
                  t))

                ;; (org-agenda-sorting-strategy '(priority-up effort-down))
                )
               ;; ("~/computer.html")
               )))

      (setq
       org-columns-default-format-org "%25ITEM %TODO %3PRIORITY %TAGS"
       org-columns-default-format "%TODO %70ITEM(Task) %8Effort(Effort){:} %8CLOCKSUM{:} %8CLOCKSUM_T(Today){:} %CLOSED")

      (setq
       org-agenda-files (directory-files-recursive
                         (expand-file-name "~/Documents/CreatedContent/contents/org")
                         "\\.org$"
                         2
                         "\\(rip\\|stage\\)"
                         t))))

  (deh-section "orgextra"
    ;; http://notmuchmail.org/emacstips/
    (add-to-list 'load-path "/usr/share/org-mode/lisp")
    (deh-require-maybe org-notmuch ))

  (deh-require-maybe org2rem
    ;; (add-hook 'org-mode-hook
    ;;           (lambda()
    ;;             (add-hook 'after-save-hook 'org2rem-all-agenda-files t t)))
    ;; The following lines are always needed.  Choose your own keys.
    (defvar org2rem-create-remind-file nil "Create remind file on when saving org file.")
    (setq org-remind-escape-percentage nil
          org-remind-include-todo t
          org-log-done '(stat)        ;use for prgress logging.
          org-remind-suppress-last-newline nil)

    ;; #+SEQ_TODO: TODO ORDERED INVOICE PAYED RECEIVED SENT
    ;; #+STARTUP: lognotestate
    ;; from http://www.djcbsoftware.nl/dot-emacs.html
    ;; (add-hook 'org-mode-hook
    ;;           (lambda() (add-hook 'before-save-hook 'org-agenda-to-appt t t)))
    ;; from http://www.djcbsoftware.nl/dot-emacs.html
    (add-hook 'org-mode-hook
              (lambda()
                (if org2rem-create-remind-file
                    (add-hook 'after-save-hook 'org2rem-this-file t t)))))

  (deh-section "org misc"
   (add-hook 'org-mode-hook
             (lambda()
               (add-hook 'after-save-hook 'org-agenda-to-appt t t)))

   (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
   ;; see key binding in binding.el

   (defun gtd ()
     (interactive)
     (find-file "~/.Organize/emacs/org/office/plan.org" )
     ;;(org-show-todo-tree 4)
     )



   (xrequire 'org-export-freemind-install)
   (defadvice org-agenda-to-appt (before wickedcool activate)
     "Clear the appt-time-msg-list."
     (setq appt-time-msg-list nil)))


  (deh-require-maybe (and
                      ob-exp
                      ox-html5presentation)))



(deh-require-maybe (and remember
                        org
                        ;; org-html
                        ob-exp
                        ob-ditaa
                        org-element
                        org-list
                        planner
                        remember-planner
                        read-file-name
                        ;; remember-blosxom
                        ;; remember-experimental ;; will start mail at daemon startup time.
                        remember-autoloads
                        remember-diary
                        remember-planner
                        remember-bbdb
                        remember
                        ;; remember-bibl
                        ;; macs-wiki-journal
                        ))


(deh-require-maybe (and remember org)

  ;;If you are, like me, missing the function org-remember-insinuate, try
  ;;the following
  ;; start
  ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html


  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (defun sharad/org-remember-sys ()
    (cond
      ((string-match "spratap" (system-name)) 'office)
      (t 'myself)))

  (defvar org-template-files-revert nil "")

  (add-hook 'ad-remember-mode-after-hook
            (lambda ()
              ;;(dolist (f org-template-files-revert)
              (while org-template-files-revert
                (let ((f (pop org-template-files-revert)))
                  (if (find-buffer-visiting f)
                      (with-current-buffer (find-buffer-visiting f)
                        (setq buffer-read-only t
                              view-read-only t
                              view-mode t)))))))

  (defun org-template-set-file-writable (xfile)
    (if (consp xfile)
        (error "xfile %s not file" xfile))
    (let* ((buf (or (find-buffer-visiting xfile)
                    (find-file-noselect xfile))))

      (with-current-buffer buf
            (when buffer-read-only
              (setq buffer-read-only nil
                    view-read-only nil
                    view-mode nil)
              (add-to-list 'org-template-files-revert xfile)))
          xfile))

  (defvar org-remember-template-alist nil "org-remember-template-alist")


  (defun org-template-push (template &rest keys)
    (pushnew (cons keys template)
             org-remember-template-alist
             :key 'car))

  ;; (get-tree '((a (b (c . d)))) 'a 'b 'c)

  (defun make-orgremember-tmpl-with-sys (key s )
    )


  (defun org-template-gen (s &optional org-parent-dir)
    (let ((org-parent-dir (or org-parent-dir "~/.Organize/emacs/org/")))
      `(("Current Task"
         ?k
         "* TODO %? %^g\n %i\n [%a]\n"
         (lambda ()
           (org-template-set-file-writable (expand-file-name "notes.org" (find-task-dir)))))
        ("Emacs"
         ?m
         "* TODO %? %^g\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "emacs.org"))
        ("Todo" ;; todos
         ?t
         "* TODO %? %^g\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "todo.org")
         "G T D")
        ("Journal" ;; any kind of note
         ?j
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "j o u r n a l")
        ("Plan" ;; any kind of note
         ?n
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "plan.org")
         "p l a n")
        ("Learn" ;; any kind of note
         ?l
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "learn.org")
         "Learn")
        ("Idea" ;; any kind of note
         ?i
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "idea.org")
         "Ideas")
        ("Book" ;; book descp
         ?b
         "\n* %^{Book Title} %t :READING: \n%[~/.Organize/emacs/remember/templates/book]\n [%a]\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "Books")
        ("Private" ;; private note
         ?p
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "privnotes.org"))
        ("Remember" ;; private note
         ?r
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "remember.org"))
        ("SomeDay" ;; private note
         ?s
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "someday.org"))
        ("Waiting-For" ;; private note
         ?w
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "waiting4.org"))
        ("Contact" ;; contact
         ?c
         "\n* %^{Name} :CONTACT:\n%[~/.Organize/emacs/remember/templates/contact]\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "contacts.org"))
        ("Receipt" ;; receipt
         ?e
         "** %^{BriefDesc} %U %^g\n%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "finances.org")))))

  ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html
  ;; (defvar org-remember-templates nil "templates for org.")

  (setq org-remember-templates (org-template-gen (symbol-name (sharad/org-remember-sys))))


  (functionp
   (nth 3 (car org-remember-templates)))

  (defun th-org-remember-conkeror (url)
    (interactive "s")
    (org-remember nil ?t)
    (save-excursion
      (insert "\n\n  [[" url "]]"))
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (org-ctrl-c-ctrl-c)
                     (delete-frame nil t))))



  ;; End
  ;; from http://www.emacswiki.org/emacs/RememberMode#toc7
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )




(provide 'orgmode-config)
