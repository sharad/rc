;;; org-clock-config.el --- Org Clock Config

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

;;; Code:

(require 'init-config "~/.xemacs/init-config.el")
(require 'org-clock)
(require 'org-compat)

(deh-section "redefined org clock in and out"
  (defun org-clock-update-mode-line ()
    (if org-clock-effort
        (ignore-errors
          (org-clock-notify-once-if-expired))
        (setq org-clock-task-overrun nil))
    (setq org-mode-line-string
          (org-propertize
           (let ((clock-string (org-clock-get-clock-string))
                 (help-text "Org-mode clock is running.\nmouse-1 shows a menu\nmouse-2 will jump to task"))
             (if (and (> org-clock-string-limit 0)
                      (> (length clock-string) org-clock-string-limit))
                 (org-propertize
                  (substring clock-string 0 org-clock-string-limit)
                  'help-echo (concat help-text ": " org-clock-heading))
                 (org-propertize clock-string 'help-echo help-text)))
           'local-map org-clock-mode-line-map
           'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
    (if (and org-clock-task-overrun org-clock-task-overrun-text)
        (setq org-mode-line-string
              (concat (org-propertize
                       org-clock-task-overrun-text
                       'face 'org-mode-line-clock-overrun) org-mode-line-string)))
    (force-mode-line-update))
  (defun org-clock-in (&optional select start-time)
    "Start the clock on the current item.
If necessary, clock-out of the currently active clock.
With a prefix argument SELECT (\\[universal-argument]), offer a list of recently clocked
tasks to clock into.  When SELECT is \\[universal-argument] \\[universal-argument], clock into the current task
and mark it as the default task, a special task that will always be offered
in the clocking selection, associated with the letter `d'.
When SELECT is \\[universal-argument] \\[universal-argument] \\[universal-argument], \
clock in by using the last clock-out
time as the start time \(see `org-clock-continuously' to
make this the default behavior.)"
    (interactive "P")
    (setq org-clock-notification-was-shown nil)
    (org-refresh-properties
     org-effort-property '((effort . identity)
                           (effort-minutes . org-duration-string-to-minutes)))
    (catch 'abort
      (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
                               (org-clocking-p)))
            ts selected-task target-pos (org--msg-extra "")
            (leftover (and (not org-clock-resolving-clocks)
                           org-clock-leftover-time)))

        (when (and org-clock-auto-clock-resolution
                   (or (not interrupting)
                       (eq t org-clock-auto-clock-resolution))
                   (not org-clock-clocking-in)
                   (not org-clock-resolving-clocks))
          (setq org-clock-leftover-time nil)
          (let ((org-clock-clocking-in t))
            (org-resolve-clocks)))	; check if any clocks are dangling

        (when (equal select '(64))
          ;; Set start-time to `org-clock-out-time'
          (let ((org-clock-continuously t))
            (org-clock-in nil org-clock-out-time)))

        (when (equal select '(4))
          (setq selected-task (org-clock-select-task "Clock-in on task: "))
          (if selected-task
              (setq selected-task (copy-marker selected-task))
              (error "Abort")))

        (when (equal select '(16))
          ;; Mark as default clocking task
          (org-clock-mark-default-task))

        (when interrupting
          ;; We are interrupting the clocking of a different task.
          ;; Save a marker to this task, so that we can go back.
          ;; First check if we are trying to clock into the same task!
          (when (save-excursion
                  (unless selected-task
                    (org-back-to-heading t))
                  (and (equal (marker-buffer org-clock-hd-marker)
                              (if selected-task
                                  (marker-buffer selected-task)
                                  (current-buffer)))
                       (= (marker-position org-clock-hd-marker)
                          (if selected-task
                              (marker-position selected-task)
                              (point)))
                       (equal org-clock-current-task (nth 4 (org-heading-components)))))
            (message "Clock continues in \"%s\"" org-clock-heading)
            (throw 'abort nil))
          (move-marker org-clock-interrupted-task
                       (marker-position org-clock-marker)
                       (marker-buffer org-clock-marker))
          (let ((org-clock-clocking-in t))
            (org-clock-out nil t)))

        ;; Clock in at which position?
        (setq target-pos
              (if (and (eobp) (not (org-at-heading-p)))
                  (point-at-bol 0)
                  (point)))
        (save-excursion
          (when (and selected-task (marker-buffer selected-task))
            ;; There is a selected task, move to the correct buffer
            ;; and set the new target position.
            (set-buffer (org-base-buffer (marker-buffer selected-task)))
            (setq target-pos (marker-position selected-task))
            (move-marker selected-task nil))
          (save-excursion
            (save-restriction
              (let ((buffer-read-only nil)) ;add my be -sharad
                (widen)
                (goto-char target-pos)
                (org-back-to-heading t)
                (or interrupting (move-marker org-clock-interrupted-task nil))
                (run-hooks 'org-clock-in-prepare-hook)
                (org-clock-history-push)
                (setq org-clock-current-task (nth 4 (org-heading-components)))
                (cond ((functionp org-clock-in-switch-to-state)
                       (looking-at org-complex-heading-regexp)
                       (let ((newstate (funcall org-clock-in-switch-to-state
                                                (match-string 2))))
                         (if newstate (org-todo newstate))))
                      ((and org-clock-in-switch-to-state
                            (not (looking-at (concat org-outline-regexp "[ \t]*"
                                                     org-clock-in-switch-to-state
                                                     "\\>"))))
                       (org-todo org-clock-in-switch-to-state)))
                (setq org-clock-heading
                      (cond ((and org-clock-heading-function
                                  (functionp org-clock-heading-function))
                             (funcall org-clock-heading-function))
                            ((nth 4 (org-heading-components))
                             (replace-regexp-in-string
                              "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                              (match-string-no-properties 4)))
                            (t "???")))
                (org-clock-find-position org-clock-in-resume)
                (cond
                  ((and org-clock-in-resume
                        (looking-at
                         (concat "^[ \t]*" org-clock-string
                                 " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                                 " *\\sw+.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
                   (message "Matched %s" (match-string 1))
                   (setq ts (concat "[" (match-string 1) "]"))
                   (goto-char (match-end 1))
                   (setq org-clock-start-time
                         (apply 'encode-time
                                (org-parse-time-string (match-string 1))))
                   (setq org-clock-effort (org-entry-get (point) org-effort-property))
                   (setq org-clock-total-time (org-clock-sum-current-item
                                               (org-clock-get-sum-start))))
                  ((eq org-clock-in-resume 'auto-restart)
                   ;; called from org-clock-load during startup,
                   ;; do not interrupt, but warn!
                   (message "Cannot restart clock because task does not contain unfinished clock")
                   (ding)
                   (sit-for 2)
                   (throw 'abort nil))
                  (t
                   (insert-before-markers "\n")
                   (backward-char 1)
                   (org-indent-line)
                   (when (and (save-excursion
                                (end-of-line 0)
                                (org-in-item-p)))
                     (beginning-of-line 1)
                     (org-indent-line-to (- (org-get-indentation) 2)))
                   (insert org-clock-string " ")
                   (setq org-clock-effort (org-entry-get (point) org-effort-property))
                   (setq org-clock-total-time (org-clock-sum-current-item
                                               (org-clock-get-sum-start)))
                   (setq org-clock-start-time
                         (or (and org-clock-continuously org-clock-out-time)
                             (and leftover
                                  (y-or-n-p
                                   (format
                                    "You stopped another clock %d mins ago; start this one from then? "
                                    (/ (- (org-float-time
                                           (org-current-time org-clock-rounding-minutes t))
                                          (org-float-time leftover)) 60)))
                                  leftover)
                             start-time
                             (org-current-time org-clock-rounding-minutes t)))
                   (setq ts (org-insert-time-stamp org-clock-start-time
                                                   'with-hm 'inactive))))
                (move-marker org-clock-marker (point) (buffer-base-buffer))
                (move-marker org-clock-hd-marker
                             (save-excursion (org-back-to-heading t) (point))
                             (buffer-base-buffer))
                (setq org-clock-has-been-used t)
                ;; add to mode line
                (when (or (eq org-clock-clocked-in-display 'mode-line)
                          (eq org-clock-clocked-in-display 'both))
                  (or global-mode-string (setq global-mode-string '("")))
                  (or (memq 'org-mode-line-string global-mode-string)
                      (setq global-mode-string
                            (append global-mode-string '(org-mode-line-string)))))
                ;; add to frame title
                (when (or (eq org-clock-clocked-in-display 'frame-title)
                          (eq org-clock-clocked-in-display 'both))
                  (setq frame-title-format org-clock-frame-title-format))
                (org-clock-update-mode-line)
                (when org-clock-mode-line-timer
                  (cancel-timer org-clock-mode-line-timer)
                  (setq org-clock-mode-line-timer nil))
                (when org-clock-clocked-in-display
                  (setq org-clock-mode-line-timer
                        (run-with-timer org-clock-update-period
                                        org-clock-update-period
                                        'org-clock-update-mode-line)))
                (when org-clock-idle-timer
                  (cancel-timer org-clock-idle-timer)
                  (setq org-clock-idle-timer nil))
                (setq org-clock-idle-timer
                      (run-with-timer 60 60 'org-resolve-clocks-if-idle))
                (message "Clock starts at %s - %s" ts org--msg-extra)
                (run-hooks 'org-clock-in-hook))))))))
  (defun org-clock-out (&optional switch-to-state fail-quietly at-time)
    "Stop the currently running clock.
Throw an error if there is no running clock and FAIL-QUIETLY is nil.
With a universal prefix, prompt for a state to switch the clocked out task
to, overriding the existing value of `org-clock-out-switch-to-state'."
    (interactive "P")
    (catch 'exit
      (when (not (org-clocking-p))
        (setq global-mode-string
              (delq 'org-mode-line-string global-mode-string))
        (setq frame-title-format org-frame-title-format-backup)
        (force-mode-line-update)
        (if fail-quietly (throw 'exit t) (user-error "No active clock")))
      (let ((org-clock-out-switch-to-state
             (if switch-to-state
                 (completing-read "Switch to state: "
                                  (with-current-buffer
                                      (marker-buffer org-clock-marker)
                                    org-todo-keywords-1)
                                  nil t "DONE")
                 org-clock-out-switch-to-state))
            (now (org-current-time org-clock-rounding-minutes))
            ts te s h m remove)
        (setq org-clock-out-time now)
        (save-excursion ; Do not replace this with `with-current-buffer'.
          (org-no-warnings (set-buffer (org-clocking-buffer)))
          (save-restriction
            (let ((buffer-read-only nil))
              (widen)
              (goto-char org-clock-marker)
              (beginning-of-line 1)
              (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
                       (equal (match-string 1) org-clock-string))
                  (setq ts (match-string 2))
                  (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
              (goto-char (match-end 0))
              (delete-region (point) (point-at-eol))
              (insert "--")
              (setq te (org-insert-time-stamp (or at-time now) 'with-hm 'inactive))
              (setq s (- (org-float-time (apply 'encode-time (org-parse-time-string te)))
                         (org-float-time (apply 'encode-time (org-parse-time-string ts))))
                    h (floor (/ s 3600))
                    s (- s (* 3600 h))
                    m (floor (/ s 60))
                    s (- s (* 60 s)))
              (insert " => " (format "%2d:%02d" h m))
              (when (setq remove (and org-clock-out-remove-zero-time-clocks
                                      (= (+ h m) 0)))
                (beginning-of-line 1)
                (delete-region (point) (point-at-eol))
                (and (looking-at "\n") (> (point-max) (1+ (point)))
                     (delete-char 1)))
              (move-marker org-clock-marker nil)
              (move-marker org-clock-hd-marker nil)
              (when org-log-note-clock-out
                (org-add-log-setup 'clock-out nil nil nil nil
                                   (concat "# Task: " (org-get-heading t) "\n\n")))
              (when org-clock-mode-line-timer
                (cancel-timer org-clock-mode-line-timer)
                (setq org-clock-mode-line-timer nil))
              (when org-clock-idle-timer
                (cancel-timer org-clock-idle-timer)
                (setq org-clock-idle-timer nil))
              (setq global-mode-string
                    (delq 'org-mode-line-string global-mode-string))
              (setq frame-title-format org-frame-title-format-backup)
              (when org-clock-out-switch-to-state
                (save-excursion
                  (org-back-to-heading t)
                  (let ((org-inhibit-logging t)
                        (org-clock-out-when-done nil))
                    (cond
                      ((functionp org-clock-out-switch-to-state)
                       (looking-at org-complex-heading-regexp)
                       (let ((newstate (funcall org-clock-out-switch-to-state
                                                (match-string 2))))
                         (if newstate (org-todo newstate))))
                      ((and org-clock-out-switch-to-state
                            (not (looking-at (concat org-outline-regexp "[ \t]*"
                                                     org-clock-out-switch-to-state
                                                     "\\>"))))
                       (org-todo org-clock-out-switch-to-state))))))
              (force-mode-line-update)
              (message (concat "Clock stopped at %s after "
                               (org-minutes-to-clocksum-string (+ (* 60 h) m)) "%s")
                       te (if remove " => LINE REMOVED" ""))
              (let ((h org-clock-out-hook)
                    (clock-drawer (org-clock-into-drawer)))
                ;; If a closing note needs to be stored in the drawer
                ;; where clocks are stored, let's temporarily disable
                ;; `org-clock-remove-empty-clock-drawer'.
                (if (and clock-drawer
                         (not (stringp clock-drawer))
                         (org-log-into-drawer)
                         (eq org-log-done 'note)
                         org-clock-out-when-done)
                    (setq h (delq 'org-clock-remove-empty-clock-drawer h)))
                (mapc (lambda (f) (funcall f)) h))
              (unless (org-clocking-p)
                (setq org-clock-current-task nil))))))))

  (when (fboundp 'advice-add)
    (advice-add 'org-clock-in :override #'my/org-clock-in)
    (advice-add 'org-clock-out :override #'my/org-clock-out)))


(deh-section "redefined clock resolve"

  (require 'orgmode-config)

  ;; (setq org-clock-idle-time 1)

  ;; org-refile-targets
  (defvar org-idle-other-clock-refile-targets
    '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3) (org-files-list :maxlevel . 4) (my-org-files-list :maxlevel . 4)))

  (defun org-clock-resolve-select-other-clock-marker (&optional refile-targets)
    (org-with-refile (or refile-targets org-refile-targets)
      (let ((buffer-read-only nil))
        (make-marker))))

  (defun org-clock-resolve-clock (clock resolve-to clock-out-time
                                  &optional close-p restart-p otherclock fail-quietly)
    "Resolve `CLOCK' given the time `RESOLVE-TO', and the present.
`CLOCK' is a cons cell of the form (MARKER START-TIME)."
    (let ((org-clock-resolving-clocks t))
      (cond
        ((null resolve-to)
         (org-clock-clock-cancel clock)
         (if (and restart-p (not org-clock-clocking-in))
             (org-clock-clock-in clock)))

        ((eq resolve-to 'now)
         (if restart-p
             (error "RESTART-P is not valid here"))
         (if (or close-p org-clock-clocking-in)
             (org-clock-clock-out clock fail-quietly)
             (unless (org-is-active-clock clock)
               (org-clock-clock-in clock t))))

        ((not (time-less-p resolve-to (current-time)))
         (error "RESOLVE-TO must refer to a time in the past"))

        (t
         (if restart-p
             (error "RESTART-P is not valid here"))
         (let ((org-log-note-clock-out
                (if otherclock
                    t
                    org-log-note-clock-out)))
           (org-clock-clock-out clock fail-quietly (or clock-out-time
                                                       resolve-to)))
         (unless org-clock-clocking-in
           (if close-p
               (setq org-clock-leftover-time (and (null clock-out-time)
                                                  resolve-to))
               (org-clock-clock-in (or otherclock clock) nil (and clock-out-time
                                                  resolve-to))))))))

  (defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
    "Resolve an open org-mode clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
    (assert clock)
    (let* ((ch
            (save-window-excursion
              (save-excursion
                (unless org-clock-resolving-clocks-due-to-idleness
                  (org-clock-jump-to-current-clock clock))
                (unless org-clock-resolve-expert
                  (with-output-to-temp-buffer "*Org Clock*"
                    (princ (format-message "Select a Clock Resolution Command:

i/q      Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.

g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from `k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.

s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.

C        Cancel the open timer altogether.  It will be as though you
         never clocked in.

j/J      Jump to the current clock, to make manual adjustments.

o/O      Busy with other clock entry.

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
                (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
                (let (char-pressed)
                  (when (featurep 'xemacs)
                    (message (concat (funcall prompt-fn clock)
                                     " [jkKgGsScCiq]? "))
                    (setq char-pressed (read-char-exclusive)))
                  (while (or (null char-pressed)
                             (and (not (memq char-pressed
                                             '(?k ?K ?g ?G ?s ?S ?C ?o ?O
                                               ?j ?J ?i ?q)))
                                  (or (ding) t)))
                    (setq char-pressed
                          (read-char (concat (funcall prompt-fn clock)
                                             " [jkKgGSscCiq]? ")
                                     nil 45)))
                  (and (not (memq char-pressed '(?i ?q))) char-pressed)))))

           (default
            (floor (/ (org-float-time
                       (time-subtract (current-time) last-valid)) 60)))
           (keep
            (and (memq ch '(?k ?K))
                 (read-number "Keep how many minutes? " default)))
           (gotback
            (and (memq ch '(?g ?G))
                 (read-number "Got back how many minutes ago? " default)))
           (otherclock (memq ch '(?o ?O)))
           (subtractp (memq ch '(?s ?S)))
           (barely-started-p (< (- (org-float-time last-valid)
                                   (org-float-time (cdr clock))) 45))
           (start-over (and subtractp barely-started-p)))
      (cond
        ((memq ch '(?j ?J))
         (if (eq ch ?J)
             (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
         (org-clock-jump-to-current-clock clock))
        ((or (null ch)
             (not (memq ch '(?k ?K ?g ?G ?s ?S ?C ?o ?O))))
         (message ""))
        (t
         (org-clock-resolve-clock
          clock (cond
                  ((or (eq ch ?C)
                       ;; If the time on the clock was less than a minute before
                       ;; the user went away, and they've ask to subtract all the
                       ;; time...
                       start-over)
                   nil)
                  ((or subtractp
                       (and gotback (= gotback 0)))
                   last-valid)
                  ((or (and keep (= keep default))
                       (and gotback (= gotback default)))
                   'now)
                  (otherclock 'otherclock)
                  (keep
                   (time-add last-valid (seconds-to-time (* 60 keep))))
                  (gotback
                   (time-subtract (current-time)
                                  (seconds-to-time (* 60 gotback))))
                  (t
                   (error "Unexpected, please report this as a bug")))
          (and gotback last-valid)
          (memq ch '(?K ?G ?S ?o ?O))
          (and start-over
               (not (memq ch '(?K ?G ?S ?C))))
          (if otherclock
              (org-clock-resolve-select-other-clock-marker org-idle-other-clock-refile-targets))
          fail-quietly)))))

  (when (fboundp 'advice-add)
    (advice-add 'org-clock-resolve-clock :override #'my/org-clock-resolve-clock)
    (advice-add 'org-clock-resolve :override #'my/org-clock-resolve)))


(deh-section "utils"

  (require 'org)
  (require 'org-clock)

  (setq org-log-post-message "Stored the note.")

  (defun org-store-log-note ()
    "Finish taking a log note, and insert it to where it belongs."
    (let ((txt (buffer-string)))
      (kill-buffer (current-buffer))
      (let ((note (cdr (assq org-log-note-purpose org-log-note-headings))) lines)
        (while (string-match "\\`# .*\n[ \t\n]*" txt)
          (setq txt (replace-match "" t t txt)))
        (if (string-match "\\s-+\\'" txt)
            (setq txt (replace-match "" t t txt)))
        (setq lines (org-split-string txt "\n"))
        (when (and note (string-match "\\S-" note))
          (setq note
                (org-replace-escapes
                 note
                 (list (cons "%u" (user-login-name))
                       (cons "%U" user-full-name)
                       (cons "%t" (format-time-string
                                   (org-time-stamp-format 'long 'inactive)
                                   org-log-note-effective-time))
                       (cons "%T" (format-time-string
                                   (org-time-stamp-format 'long nil)
                                   org-log-note-effective-time))
                       (cons "%d" (format-time-string
                                   (org-time-stamp-format nil 'inactive)
                                   org-log-note-effective-time))
                       (cons "%D" (format-time-string
                                   (org-time-stamp-format nil nil)
                                   org-log-note-effective-time))
                       (cons "%s" (cond
                                    ((not org-log-note-state) "")
                                    ((org-string-match-p org-ts-regexp
                                                         org-log-note-state)
                                     (format "\"[%s]\""
                                             (substring org-log-note-state 1 -1)))
                                    (t (format "\"%s\"" org-log-note-state))))
                       (cons "%S"
                             (cond
                               ((not org-log-note-previous-state) "")
                               ((org-string-match-p org-ts-regexp
                                                    org-log-note-previous-state)
                                (format "\"[%s]\""
                                        (substring
                                         org-log-note-previous-state 1 -1)))
                               (t (format "\"%s\""
                                          org-log-note-previous-state)))))))
          (when lines (setq note (concat note " \\\\")))
          (push note lines))
        (when (or current-prefix-arg org-note-abort)
          (when (org-log-into-drawer)
            (org-remove-empty-drawer-at org-log-note-marker))
          (setq lines nil))
        (when lines
          (with-current-buffer (marker-buffer org-log-note-marker)
            (org-with-wide-buffer
             (with-writable-buffer
                 (goto-char org-log-note-marker)
               (move-marker org-log-note-marker nil)
               ;; Make sure point is at the beginning of an empty line.
               (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
                     ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
               ;; In an existing list, add a new item at the top level.
               ;; Otherwise, indent line like a regular one.
               (let ((itemp (org-in-item-p)))
                 (if itemp
                     (org-indent-line-to
                      (let ((struct (save-excursion
                                      (goto-char itemp) (org-list-struct))))
                        (org-list-get-ind (org-list-get-top-point struct) struct)))
                     (org-indent-line)))
               (insert (org-list-bullet-string "-") (pop lines))
               (let ((ind (org-list-item-body-column (line-beginning-position))))
                 (dolist (line lines)
                   (insert "\n")
                   (org-indent-line-to ind)
                   (insert line)))
               (message "Note stored")
               (org-back-to-heading t)
               (org-cycle-hide-drawers 'children)))
            ;; Fix `buffer-undo-list' when `org-store-log-note' is called
            ;; from within `org-add-log-note' because `buffer-undo-list'
            ;; is then modified outside of `org-with-remote-undo'.
            (when (eq this-command 'org-agenda-todo)
              (setcdr buffer-undo-list (cddr buffer-undo-list)))))))
    ;; Don't add undo information when called from `org-agenda-todo'
    (let ((buffer-undo-list (eq this-command 'org-agenda-todo)))
      (set-window-configuration org-log-note-window-configuration)
      (with-current-buffer (marker-buffer org-log-note-return-to)
        (goto-char org-log-note-return-to))
      (move-marker org-log-note-return-to nil)
      (and org-log-post-message (message "%s" org-log-post-message))))


  (defun org-add-note-to-current-clock ()
    (interactive)
    (if (org-clock-is-active)
        (org-with-clock (list org-clock-marker)
          (with-writable-buffer
              (org-add-note)))
        (message "No current clock."))))


(provide 'org-clock-config)
;;; org-clock-config.el ends here
