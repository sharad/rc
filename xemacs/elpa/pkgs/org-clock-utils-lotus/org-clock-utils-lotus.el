;;; org-clock-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; (use-package startup-hooks
;;     :defer t
;;     :config
;;     (progn
;;       (progn
;;         (add-to-enable-startup-interrupting-feature-hook
;;          '(lambda ()
;;            (when nil
;;              (add-hook 'after-make-frame-functions
;;                        '(lambda (nframe)
;;                          (run-at-time-or-now 100
;;                           '(lambda ()
;;                             (if (any-frame-opened-p)
;;                                 (org-clock-in-if-not)))))
;;                        t))
;;            (add-hook 'delete-frame-functions
;;             '(lambda (nframe)
;;               (if (and
;;                    (org-clock-is-active)
;;                    (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil))
;;                   (org-with-clock-writeable
;;                    (let (org-log-note-clock-out)
;;                      (if (org-clock-is-active)
;;                          (org-clock-out))))))))
;;          t))

;;       (progn
;;         (add-to-enable-desktop-restore-interrupting-feature-hook
;;          '(lambda ()
;;            (if (fboundp 'org-clock-persistence-insinuate)
;;                (org-clock-persistence-insinuate)
;;                (message "Error: Org Clock function org-clock-persistence-insinuate not available."))
;;            (if (fboundp 'org-clock-start-check-timer)
;;                (org-clock-start-check-timer)))
;;          t))))


;; (add-hook
;;  'kill-emacs-hook
;;  (lambda ()
;;    (if (and
;;         (org-clock-is-active)
;;         ;; (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil)
;;         )
;;        (org-with-clock-writeable
;;         (let (org-log-note-clock-out)
;;           (if (org-clock-is-active)
;;               (org-clock-out)))))))

;;; Code:

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'timer-utils-lotus)
(require 'startup-hooks)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'org-misc-utils-lotus)


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

(defvar org-clock-check-long-timer-period 7
  "Period of Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

(defvar org-clock-check-long-timer nil
  "Long Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

(defvar org-clock-check-short-timer-period 2
  "Period Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

;; (defvar org-clock-check-short-timer nil
;;   "Short Timer to remind to clock-in after some time of clockout or if no clock found at start of emacs.")

;;;###autoload
(defun org-clock-start-check-timer ()
  "Attempt to clock-in when already not clock found."
  (interactive)
  (org-clock-stop-check-timer)
  (setq
   org-clock-check-long-timer
   (run-with-nonobtrusive-aware-idle-timers
    org-clock-check-long-timer-period
    org-clock-check-long-timer-period
    org-clock-check-short-timer-period
    nil
    #'(lambda (arg)
        (unless (org-clock-is-active)
          (org-clock-in-if-not)))
    nil
    nil)))

;;;###autoload
(defun org-clock-stop-check-timer ()
  "Stop attemptting to clock-in when already not clock found."
  (interactive)
  (progn
    ;; (when org-clock-check-short-timer
    ;;   (cancel-timer org-clock-check-short-timer)
    ;;   (setq org-clock-check-short-timer nil))
    (when org-clock-check-long-timer
      (cancel-timer org-clock-check-long-timer)
      (setq org-clock-check-long-timer nil))))

;;;###autoload
(defun org-clock-start-check-timer-insiuate ()
  (org-clock-start-check-timer))

;;;###autoload
(defun org-clock-start-check-timer-uninsiuate ()
  (org-clock-stop-check-timer))

;; "correction org-timer.el"
(defun replace-org-timer-set-timer (&optional opt)
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
                 (org-duration-string-to-minutes (org-entry-get nil "Effort")))))
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
                (message "No timer set"))))))

;; (advice-add 'org-timer-set-timer :around #'replace-org-timer-set-timer)

(add-function :override (symbol-function 'org-timer-set-timer) #'replace-org-timer-set-timer)



(defvar org-clock-default-effort "1:00")

(defun lotus-org-mode-add-default-effort ()
  "Add a default effort estimation."
  (unless (org-entry-get (point) "Effort")
    (org-set-property "Effort" org-clock-default-effort)))
(add-hook 'org-clock-in-prepare-hook
          'lotus-org-mode-ask-effort)

(defun lotus-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;; org-refile-targets is set in org-misc-utils-lotus package
;;;###autoload
(defun org-clock-in-refile (refile-targets)
  (org-with-refile file loc (or refile-targets org-refile-targets)
    (let ((buffer-read-only nil))
      (org-clock-in))))
(defvar org-clock-in-if-not-delay 100 "org-clock-in-if-not-delay")
(defvar org-donot-try-to-clock-in nil
  "Not try to clock-in, require for properly creating frame especially for frame-launcher function.")
;;;###autoload
(defun org-clock-in-if-not ()
  (interactive)
  (unless (or
           org-donot-try-to-clock-in
           (org-clock-is-active))
    ;; (org-clock-goto t)
    (if org-clock-history
        (let (buffer-read-only)
          (org-clock-in '(4)))
        ;; with-current-buffer should be some real file
        (org-clock-in-refile nil))))

(defvar org-clock-in-if-not-at-time-timer nil)
(defun org-clock-in-if-not-at-time (delay)
  (setq org-clock-in-if-not-at-time-timer
        (run-at-time-or-now delay
                            #'(lambda ()
                                (if (any-frame-opened-p)
                                    (org-clock-in-if-not))))))

(defun org-clock-in-if-not-at-time-delay ()
  (org-clock-in-if-not-at-time org-clock-in-if-not-delay))

(defun org-clock-in-if-not-at-time-delay-frame-fn ()
  (org-clock-in-if-not-at-time-delay))

;;;###autoload
(defun lotus-org-clock-in/out-insinuate-hooks ()
  (add-hook 'org-clock-in-hook
            '(lambda ()
              ;; ;; if effort is not present than add it.
              ;; (unless (org-entry-get nil "Effort")
              ;;   (save-excursion
              ;;    (org-set-effort)))
              ;; set timer
              (when (not
                     (and
                      (boundp' org-timer-countdown-timer)
                      org-timer-countdown-timer))
                (if (org-entry-get nil "Effort")
                    (save-excursion
                      (forward-line -2)
                      (org-timer-set-timer))
                    (call-interactively 'org-timer-set-timer)))
              (save-buffer)
              (org-save-all-org-buffers)))
  (add-hook 'org-clock-out-hook
            '(lambda ()
              (if (and
                   (boundp' org-timer-countdown-timer)
                   org-timer-countdown-timer)
                  (org-timer-stop))
              (org-clock-get-work-day-clock-string t)
              (save-buffer)
              (org-save-all-org-buffers))))



(progn
  (eval-when-compile
    (require 'org-misc-utils-lotus))

  (progn
    (setq
     ;; org-timer-default-timer 25
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
     org-clock-out-remove-zero-time-clocks t))

  (progn

    (defun org-idle-tracing-function (orig-fun &rest args)
      (message "org-resolve-clocks-if-idle called with args %S" args)
      (let ((res (apply orig-fun args)))
        (message "org-resolve-clocks-if-idle returned %S" res)
        res))

    (advice-add 'org-resolve-clocks-if-idle :around #'org-idle-tracing-function)

    ;; (advice-remove 'display-buffer #'org-idle-tracing-function)
    )

  (progn
    (when nil
      (defvar org-clock-display-timer-delay 2 "Org clock display timer delay")

      (defun org-clock-display-with-timer (start end old-len)
        (when (buffer-modified-p)
          ;; (when org-clock-display-timer
          ;;   (cancel-timer org-clock-display-timer)
          ;;   (setq org-clock-display-timer nil))
          ;; (setq
          ;;  org-clock-display-timer
          ;;  (run-with-timer org-clock-display-timer-delay nil 'org-clock-display))
          (org-clock-display)))

      (defun org-mode-setup-clock-display ()
        (make-variable-buffer-local 'org-clock-display-timer)
        (add-hook 'after-change-functions
                  'org-clock-display-with-timer))

      (add-hook 'org-mode-hook 'org-mode-setup-clock-display)))

  (progn

    ))


;; https://emacs.stackexchange.com/questions/3970/org-mode-warning-when-scheduling-task-on-top-of-another-task
(defun check-availability (&optional date)
  "Doc-string."
  (interactive)
  (let* (
         not-available
         (date (if date date (read-string "Date:  ")))
         (date-parsed (parse-time-string date))
         (date-time
          (cond
            ((and
              (not (null (nth 0 date-parsed)))
              (not (null (nth 1 date-parsed)))
              (not (null (nth 2 date-parsed))))
             (date-to-time date))
            ((and
              (null (nth 0 date-parsed))
              (null (nth 1 date-parsed))
              (null (nth 2 date-parsed)))
             (date-to-time (concat date " 00:00")))))
         (date-seconds (time-to-seconds date-time)) )
    (with-current-buffer (get-buffer ".todo")
      (save-excursion
        (goto-char (point-max))
        (catch 'found
          (while (re-search-backward "\\* \\(TODO\\|WORK\\|DONE\\)" nil t)
            (unless (org-at-heading-p)
              (org-back-to-heading t))
            (let* ((element (org-element-at-point))
                   (deadline (org-element-property :deadline element))
                   (deadline-seconds
                    (when deadline
                      (time-to-seconds
                       (org-time-string-to-time
                        (org-element-property :raw-value deadline)))))
                   (scheduled (org-element-property :scheduled element))
                   (scheduled-seconds
                    (when scheduled
                      (time-to-seconds
                       (org-time-string-to-time
                        (org-element-property :raw-value scheduled))))) )
              (when
                  (or
                   (and
                    deadline-seconds
                    (= date-seconds deadline-seconds))
                   (and
                    scheduled-seconds
                    (= date-seconds scheduled-seconds)))
                (setq not-available t)
                (throw 'found (message "Not Available!")))))))
      (unless not-available
        (message "Congratulations -- you are available!")))))

;;{{{ https://stackoverflow.com/questions/4872088/is-there-any-way-for-subtasks-to-inherit-deadlines-in-org-mode

;;How about a function for adding subtasks? This one adds a deadline to the
;;subtask if its parent has one:

(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))

;; Don't forget to bind it to a key:

;; (define-key org-mode-map (kbd "C-c s") 'my-org-insert-sub-task)

;; Also you might find these settings useful:

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)
;;}}}


;;;{{{ https://orgmode.org/worg/org-hacks.html#org86e75a5
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
            (if (called-interactively-p 'interactive)
                (progn
                  (select-window (display-buffer buf t t))
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )))
        (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(run-with-idle-timer 300 t 'jump-to-org-agenda)
;;;}}}

;;;{{{ Emacs tasks https://emacs.stackexchange.com/questions/29128/programmatically-setting-an-org-mode-heading
(progn
(defvar *lotus-org-unnamed-task-file*        "~/Unnamed.org")
(defvar *lotus-org-unnamed-parent-task-name* "Unnamed tasks")
(defvar *lotus-org-unnamed-task-name-fmt*    "Unnamed task %d")
(defvar *lotus-org-unnamed-task-clock-marker* nil)

(defun lotus-org-get-incr-tasknum (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((tasknumstr (or (org-global-get-property "TASKNUM") "0"))
           (tasknum (string-to-number tasknumstr)))
      (org-global-put-property "TASKNUM" (number-to-string (1+ tasknum)))
      tasknum)))

(defun lotus-org-create-unnamed-task (&optional file task)
  (interactive
   (let ((file *lotus-org-unnamed-task-file*)
         (task *lotus-org-unnamed-parent-task-name*))
     (list file task)))

  (let ((file (or file *lotus-org-unnamed-task-file*))
        (task (or task *lotus-org-unnamed-parent-task-name*))
        (subtask (format *lotus-org-unnamed-task-name-fmt*
                         ;; (lotus-org-get-incr-tasknum (find-file-noselect file))
                         (1+ (org-with-file-headline file task (org-number-of-subheadings))))))
    (org-find-heading-marker file task t)
    (org-insert-subheading-to-file-headline
     subtask
     file
     task
     t)
    subtask))

(defun lotus-org-create-unnamed-task-task-clock-in (&optional file parent-task task)
  (interactive
   (let ((file *lotus-org-unnamed-task-file*)
         (parent-task *lotus-org-unnamed-parent-task-name*)
         (task (format *lotus-org-unnamed-task-name-fmt* 1)))
     (list file parent-task task)))
  (let ((file (or file *lotus-org-unnamed-task-file*))
        (parent-task (or parent-task *lotus-org-unnamed-parent-task-name*)))
    (org-with-file-headline
        file
        (lotus-org-create-unnamed-task file parent-task)
      (org-entry-put nil "Effort" "10")
      (org-clock-in)
      (setq
       *lotus-org-unnamed-task-clock-marker*
       (mark-marker)))))

(lotus-org-create-unnamed-task "~/Unnamed.org" "Unnamed tasks")

(defun org-clock-make-child-task-and-clock-in ()
  ;; TODO
  "Implement"
  )

(defun lotus-org-clockin-last-time (min)
  ))


(defun lotus-org-clock-detect-first-clockin-of-day ()
  ;; do necessary stuff
  ;; like context presentation etc.
  )

(defun lotus-org-clock-declare-last-clockout-of-day ()
  )

(defun org-clock-add-schedule-on-clockin-if-not ()
  )

(defun org-clock-add-deadline-on-clockin-if-not ()
  )

(defun org-define-a-task ()
  "This function is used to create a org tree to complete a task.
for e.g. implementing lvm support for guixsd what all steps a person have to take
he has to read scheme, guixsd details, than see similar module and try to implement it."
  (interactive)
  )

(defun org-log-not-on-event (start end event)
  )
;;;}}}


;;;{{{ https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly


;; The following command inserts the typical org logbook entry, a time range
;; starting from N minutes ago:

(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (let* ((ctime (cdr (decode-time (current-time))))
         (min (car ctime))
         (start (apply 'encode-time 0 (- min n) (cdr ctime))))
    (org-insert-time-stamp start t t "CLOCK: ")
    (insert "--")
    (org-insert-time-stamp (current-time) t t)))




;; mutbuerger's answer helped me a lot. It's my first time programming in elisp
;; other than configuration. His answer was close to the mark, but I still
;; wanted the normal functionality of clock-in and clock-out for finding or
;; creating the logbook under the closest heading, and adding a note.

;; My amended solution was this:

(defun offset-current-time (n)
  (let* ((ctime (cdr (decode-time (current-time))))
         (minutes (car ctime)))
    (apply 'encode-time 0 (- minutes n) (cdr ctime))))

(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (org-clock-in nil (offset-current-time n))
  (org-clock-out))

(defadvice org-clock-out (after org-clock-out-after activate) (org-update-all-dblocks))

(add-hook 'org-mode-hook
          (lambda ()
                                        ; Keys for org mode
                                        ;snip
            (define-key evil-normal-state-map (kbd "gl") 'org-insert-clock-range)
                                        ;snip

            ))


;;;}}}



;;; FOR WORKING FAST START CREATING TEMPLATE OR EMPTY FUNCTION BODY.


(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
