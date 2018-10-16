;;; org-clock-config.el --- Org Clock Config

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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
  )


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

  (defun my/org-clock-resolve-clock (clock resolve-to clock-out-time
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

  (defun my/org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
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
