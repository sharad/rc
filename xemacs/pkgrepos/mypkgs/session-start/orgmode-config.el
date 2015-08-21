
(require 'macros-config)
(require 'auto-load-config)
(require 'publishing-config)
(require 'files-config)
(require 'timer-config)


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

      (setq org-todo-keywords ;; http://doc.norang.ca/org-mode.html#sec-5
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

      (setq org-todo-keyword-faces ;; http://doc.norang.ca/org-mode.html#sec-5
            (quote (("TODO" :foreground "red" :weight bold)
                    ("NEXT" :foreground "blue" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold)
                    ("WAITING" :foreground "orange" :weight bold)
                    ("HOLD" :foreground "magenta" :weight bold)
                    ("CANCELLED" :foreground "forest green" :weight bold)
                    ("MEETING" :foreground "forest green" :weight bold)
                    ("PHONE" :foreground "forest green" :weight bold))))


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
               '(lambda ()
                 ;; if effort is not present tahnadd it.
                 (unless (org-entry-get nil "Effort")
                   (save-excursion
                    (org-set-effort)))
                 ;; set timer
                 (when (not
                        (and
                         (boundp' org-timer-countdown-timer)
                         org-timer-countdown-timer))
                   (if (org-entry-get nil "Effort")
                       (save-excursion
                         (forward-line -2)
                         (org-timer-set-timer nil))
                       (call-interactively 'org-timer-set-timer)))
                 (save-buffer)))

     (add-hook 'org-clock-out-hook
               '(lambda ()
                 (if (and
                      (boundp' org-timer-countdown-timer)
                      org-timer-countdown-timer)
                     (org-timer-stop))
                 (org-clock-get-work-day-clock-string t)
                 (save-buffer)))

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

      (require 'mode-line-config)

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

     (defvar org-clock-get-work-day-start-timer nil)
     (unless org-clock-get-work-day-start-timer
      (setq org-clock-get-work-day-start-timer
            (run-at-time "00:01am" (* 24 60 60) 'org-clock-get-work-day-clock-string t)))


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

      (setq
       org-clock-monitor-files
       (directory-files-recursive
        (expand-file-name
         "meru"
         (org-publish-get-attribute "tasks" "org" :base-directory))
        "\\.org$" 2 "\\(rip\\|stage\\)" t))

      (defun org-clock-unclocked-files-mins-today (files)
        (let* ((totalmins 0)
               file)
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
      (defun org-clock-get-work-day-clock-string (&optional force)
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
               (today-clocked-secs  (org-clock-files-secs org-clock-monitor-files force))
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

      (defun org-clock-work-day-update-mode-line-internal (&optional force)
      ;; (defun org-clock-work-day-update-mode-line ()
        (setq org-mode-work-day-mode-line-string
              (org-propertize

               (let ((clock-string (org-clock-get-work-day-clock-string force))
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

      (defun org-clock-work-day-update-mode-line (&optional force)
        "Update the timer time in the mode line."
        (if org-mode-work-day-pause
            nil
            (org-clock-work-day-update-mode-line-internal force)
            (force-mode-line-update)))

      (defun org-clock-work-day-mode-line-add (force)
        (interactive "P")
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
          (org-clock-work-day-update-mode-line force)
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

     (defvar org-clock-check-long-timer-period 7
       "Period of Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defvar org-clock-check-long-timer nil
       "Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defvar org-clock-check-short-timer-period 2
       "Period Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     ;; (defvar org-clock-check-short-timer nil
     ;;   "Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

     (defun org-clock-start-check-timer ()
       (interactive)
       (org-clock-stop-check-timer)
       (setq
        org-clock-check-long-timer
        (run-with-nonobtrusive-aware-idle-timers
         org-clock-check-long-timer-period
         org-clock-check-long-timer-period
         org-clock-check-short-timer-period
         (lambda ()
           (message "after 7 sec.")
           (unless (org-clock-is-active)
             (org-clock-in-if-not)))
         nil
         nil)))

     (defun org-clock-stop-check-timer ()
       (interactive)
       (progn
         ;; (when org-clock-check-short-timer
         ;;   (cancel-timer org-clock-check-short-timer)
         ;;   (setq org-clock-check-short-timer nil))
         (when org-clock-check-long-timer
           (cancel-timer org-clock-check-long-timer)
           (setq org-clock-check-long-timer nil)))))

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

      (setq org-agenda-custom-commands
            ;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
            `(("P" "Project List"
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
              ("Z" ;; "Meru Today" ;; tags-todo "computer" ;; (1) (2) (3) (4)
                   "Meru Today" ;;  search ""
               ((agenda ""
                        ((org-agenda-span 'day)
                         (org-agenda-prefix-format  "%e")))
                (org-agenda-files
                 ',(directory-files-recursive
                    (expand-file-name "meru" (org-publish-get-attribute "tasks" "org" :base-directory))
                   "\\.org$" 2 "\\(rip\\|stage\\)" t))

                ;; (org-agenda-sorting-strategy '(priority-up effort-down))
                )
               ;; ("~/computer.html")
               )))

      (setq
       org-columns-default-format-org "%25ITEM %TODO %3PRIORITY %TAGS"
       org-columns-default-format     "%TODO %70ITEM(Task) %8Effort(Effort){:} %8CLOCKSUM{:} %8CLOCKSUM_T(Today){:} %CLOSED")

      (setq
       org-agenda-files (directory-files-recursive
                         (expand-file-name
                          "~/Documents/CreatedContent/contents/org")
                         "\\.org$"
                         2
                         "\\(rip\\|stage\\)"
                         t))


    (setq
     ;; http://orgmode.org/worg/agenda-optimization.html
     ;; org-agenda-inhibit-startup t
     )) ;; (deh-section "org-agenda"

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
     (find-file
      (expand-file-name
       "office"
       (org-publish-get-attribute "notes" "org" :base-directory)))
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

(deh-require-maybe (and
                    remember
                    org
                    org-protocol
                    org-capture)

  ;; If you are, like me, missing the function org-remember-insinuate, try
  ;; the following
  ;; start
  ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html

  (setq
   org-directory (org-publish-get-attribute "notes" "org" :base-directory)
   org-default-notes-file (expand-file-name "notes.org" org-directory))

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
    (let* ((buf
            (or (find-buffer-visiting xfile)
                (find-file-noselect xfile))))
      (with-current-buffer buf
        (when buffer-read-only
          (setq
           buffer-read-only nil
           view-read-only   nil
           view-mode        nil)
          (add-to-list 'org-template-files-revert xfile)))
      xfile))

  (defvar org-remember-template-alist nil "org-remember-template-alist")

  (defun org-template-push (template &rest keys)
    (pushnew (cons keys template)
             org-remember-template-alist
             :key 'car))

  ;; (get-tree '((a (b (c . d)))) 'a 'b 'c)

  (deh-section "org-remember"
   (defun* add-org-remember-templates (&rest project-spec)
     "Add org project."
     (add-to-list 'org-remember-templates project-spec t))

   (setq org-remember-templates nil)

   (defun org-remember-template-gen (&optional org-parent-dir)
     (let ((org-parent-dir (or org-parent-dir
                               (expand-file-name "remember" (org-publish-get-attribute "notes" "org" :base-directory)))))
       (progn
         (add-org-remember-templates
          ?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" nil "Notes")

         (add-org-remember-templates
          "Current Task" ?k "* TODO %? %^g\n %i\n [%a]\n"
          (lambda ()
            (org-template-set-file-writable (expand-file-name "notes.org" (find-task-dir)))))

         (add-org-remember-templates
          "Emacs" ?m "* TODO %? %^g\n %i\n [%a]\n" (expand-file-name "emacs.org" org-parent-dir))

         (add-org-remember-templates "Todo" ?t "* TODO %? %^g\n %i\n [%a]\n" (expand-file-name "todo.org" org-parent-dir) "G T D")

         (add-org-remember-templates
          "Journal" ;; any kind of note
          ?j "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "journal.org" org-parent-dir) "j o u r n a l")

         (add-org-remember-templates "Plan" ;; any kind of note
                                     ?n "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "plan.org" org-parent-dir) "p l a n")

         (add-org-remember-templates "Learn" ;; any kind of note
                                     ?l "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "learn.org" org-parent-dir) "Learn")

         (add-org-remember-templates "Idea" ;; any kind of note
                                     ?i "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "idea.org" org-parent-dir) "Ideas")

         (add-org-remember-templates "Book" ;; book descp
                                     ?b "\n* %^{Book Title} %t :READING: \n%[~/Documents/CreatedContent/contents/org/remember/templates/book]\n [%a]\n" (expand-file-name "journal.org" org-parent-dir) "Books")

         (add-org-remember-templates "Private" ;; private note
                                     ?p "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "privnotes.org" org-parent-dir))

         (add-org-remember-templates "Remember" ;; private note
                                     ?r "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "remember.org" org-parent-dir))

         (add-org-remember-templates "SomeDay" ;; private note
                                     ?s "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "someday.org" org-parent-dir))

         (add-org-remember-templates "Waiting-For" ;; private note
                                     ?w "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "waiting4.org" org-parent-dir))

         (add-org-remember-templates "Contact" ;; contact
                                     ?c "\n* %^{Name} :CONTACT:\n%[~/Documents/CreatedContent/contents/org/remember/templates/contact]\n %i\n [%a]\n" (expand-file-name "contacts.org" org-parent-dir))

         (add-org-remember-templates "Receipt" ;; receipt
                                     ?e "** %^{BriefDesc} %U %^g\n%?\n [%a]\n" (expand-file-name "finances.org" org-parent-dir)))))

   ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html
   ;; (defvar org-remember-templates nil "templates for org.")

   (org-remember-template-gen)
   )

  (deh-section "org-capture"

    (defun* add-org-capture-templates (&rest project-spec)
      "Add org project."
      (add-to-list 'org-capture-templates project-spec t))

    ;; (setq org-protocol-default-template-key "l")
    (setq org-capture-templates nil)

   (defun org-remember-template-gen (&optional org-parent-dir)
     (let ((org-parent-dir (or org-parent-dir
                               (expand-file-name "capture" (org-publish-get-attribute "notes" "org" :base-directory)))))
       (progn

         (add-org-capture-templates
          "w" "Default template"
          'entry
          `(file+headline ,(expand-file-name "capture.org" org-parent-dir) "Notes")
          "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
          :empty-lines 1)

         )))

   (org-remember-template-gen))

  (deh-section "org-protocol-open-source"
    ;; [[http://orgmode.org/worg/org-contrib/org-protocol.html#sec-7][Edit published content: org-protocol-open-source]]
    (setq org-protocol-project-alist
      '(("Worg"
         :base-url "http://orgmode.org/worg/"
         :working-directory "/home/user/worg/"
         :online-suffix ".html"
         :working-suffix ".org")
        ("My local Org-notes"
         :base-url "http://localhost/org/"
         :working-directory "/home/user/org/"
         :online-suffix ".php"
         :working-suffix ".org"))))



  ;; End
  ;; from http://www.emacswiki.org/emacs/RememberMode#toc7
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; (deh-require-maybe (and

(when nil ;; deh-section "babel"
  ;; http://draketo.de/book/export/html/41
  ; And add babel inline code execution
  ; babel, for executing code in org-mode.
  (org-babel-do-load-languages
   'org-babel-load-languages
   ; load all language marked with (lang . t).
   '((C . t)
     ;; (R . t)
     (asymptote)
     (awk)
     (calc)
     (clojure)
     (comint)
     (css)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (fortran)
     (gnuplot . t)
     (haskell)
     (io)
     (java)
     (js)
     (latex)
     (ledger)
     (lilypond)
     (lisp)
     (matlab)
     (maxima)
     (mscgen)
     (ocaml)
     (octave)
     (org . t)
     (perl)
     (picolisp)
     (plantuml)
     (python . t)
     (ref)
     (ruby)
     (sass)
     (scala)
     (scheme)
     (screen)
     (sh . t)
     (shen)
     (sql)
     (sqlite))))


(deh-section "org rss"
  ;;
  (setq org-feed-alist
        `(("mybugs"
           "https://bugzilla.merunetworks.com/buglist.cgi?bug_status=NEEDINFO&bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED&email1=spratap%40merunetworks.com&emailassigned_to1=1&emailreporter1=1&emailtype1=exact&list_id=169890&query_format=advanced&title=Bug%20List&ctype=atom"
           ,(expand-file-name "meru/mybugs.org" (org-publish-get-attribute "tasks" "org" :base-directory))
           "My Bugs")
          ("OSNews"
           "http://www.osnews.com/feeds"
           ,(expand-file-name "../rss/osnews.org" (org-publish-get-attribute "tasks" "org" :base-directory))
           "OSNews Entries"))))




(provide 'orgmode-config)
