;;; org-clock-resolve-advanced.el ---

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


;;; Code:

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'timer-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))


(defun org-clock-steel-time ()
  )


(defvar org-clock-clocking-in nil)
(defvar org-clock-resolving-clocks nil)
(defvar org-clock-resolving-clocks-due-to-idleness nil)

(defun org-clock-resolve-clock (clock resolve-to clock-out-time
				      &optional close-p restart-p fail-quietly)
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
      (org-clock-clock-out clock fail-quietly (or clock-out-time
						  resolve-to))
      (unless org-clock-clocking-in
	(if close-p
	    (setq org-clock-leftover-time (and (null clock-out-time)
					       resolve-to))
	  (org-clock-clock-in clock nil (and clock-out-time
					     resolve-to))))))))

(defun org-clock-jump-to-current-clock (&optional effective-clock)
  (interactive)
  (let ((drawer (org-clock-into-drawer))
	(clock (or effective-clock (cons org-clock-marker
					 org-clock-start-time))))
    (unless (marker-buffer (car clock))
      (error "No clock is currently running"))
    (org-with-clock clock (org-clock-goto))
    (with-current-buffer (marker-buffer (car clock))
      (goto-char (car clock))
      (when drawer
	(org-with-wide-buffer
	 (let ((drawer-re (format "^[ \t]*:%s:[ \t]*$"
				  (regexp-quote (if (stringp drawer) drawer "LOGBOOK"))))
	       (beg (save-excursion (org-back-to-heading t) (point))))
	   (catch 'exit
	     (while (re-search-backward drawer-re beg t)
	       (let ((element (org-element-at-point)))
		 (when (eq (org-element-type element) 'drawer)
		   (when (> (org-element-property :end element) (car clock))
		     (org-flag-drawer nil element))
		   (throw 'exit nil)))))))))))

(defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
  "Resolve an open Org clock.
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
  (cl-assert clock)
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

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
              (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
              (let (char-pressed)
                (while (or (null char-pressed)
                           (and (not (memq char-pressed
                                           '(?k ?K ?g ?G ?s ?S ?C
                                             ?j ?J ?i ?q)))
                                (or (ding) t)))
                  (setq char-pressed
                        (read-char (concat (funcall prompt-fn clock)
                                           " [jkKgGSscCiq]? ")
                                   nil 45)))
                (and (not (memq char-pressed '(?i ?q))) char-pressed)))))
         (default
          (floor (/ (float-time
                     (time-subtract (current-time) last-valid)) 60)))
         (keep
          (and (memq ch '(?k ?K))
               (read-number "Keep how many minutes? " default)))
         (gotback
          (and (memq ch '(?g ?G))
               (read-number "Got back how many minutes ago? " default)))
         (subtractp (memq ch '(?s ?S)))
         (barely-started-p (< (- (float-time last-valid)
                                 (float-time (cdr clock))) 45))
         (start-over (and subtractp barely-started-p)))
    (cond
      ((memq ch '(?j ?J))
       (if (eq ch ?J)
           (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
       (org-clock-jump-to-current-clock clock))
      ((or (null ch)
           (not (memq ch '(?k ?K ?g ?G ?s ?S ?C))))
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
                (keep
                 (time-add last-valid (seconds-to-time (* 60 keep))))
                (gotback
                 (time-subtract (current-time)
                                (seconds-to-time (* 60 gotback))))
                (t
                 (error "Unexpected, please report this as a bug")))
        (and gotback last-valid)
        (memq ch '(?K ?G ?S))
        (and start-over
             (not (memq ch '(?K ?G ?S ?C))))
        fail-quietly)))))

;;;###autoload
(defun org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
	(let ((clocks (org-find-open-clocks file)))
	  (dolist (clock clocks)
	    (let ((dangling (or (not (org-clock-is-active))
				(/= (car clock) org-clock-marker))))
	      (if (or (not only-dangling-p) dangling)
		  (org-clock-resolve
		   clock
		   (or prompt-fn
		       (function
            (lambda (clock)
             (format
              "Dangling clock started %d mins ago"
              (floor (- (float-time)
                        (float-time (cdr clock)))
                     60)))))
		   (or last-valid
		       (cdr clock)))))))))))

(defun org-emacs-idle-seconds ()
  "Return the current Emacs idle time in seconds, or nil if not idle."
  (let ((idle-time (current-idle-time)))
    (if idle-time
	(float-time idle-time)
      0)))

(defun org-mac-idle-seconds ()
  "Return the current Mac idle time in seconds."
  (string-to-number (shell-command-to-string "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

(defvar org-x11idle-exists-p
  ;; Check that x11idle exists
  (and (eq window-system 'x)
       (eq 0 (call-process-shell-command
              (format "command -v %s" org-clock-x11idle-program-name)))
       ;; Check that x11idle can retrieve the idle time
       ;; FIXME: Why "..-shell-command" rather than just `call-process'?
       (eq 0 (call-process-shell-command org-clock-x11idle-program-name))))

(defun org-x11-idle-seconds ()
  "Return the current X11 idle time in seconds."
  (/ (string-to-number (shell-command-to-string org-clock-x11idle-program-name)) 1000))

(defun org-user-idle-seconds ()
  "Return the number of seconds the user has been idle for.
This routine returns a floating point number."
  (cond
   ((eq system-type 'darwin)
    (org-mac-idle-seconds))
   ((and (eq window-system 'x) org-x11idle-exists-p)
    (org-x11-idle-seconds))
   (t
    (org-emacs-idle-seconds))))

(defvar org-clock-user-idle-seconds)

(defun org-resolve-clocks-if-idle ()
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (when (and org-clock-idle-time (not org-clock-resolving-clocks)
             org-clock-marker (marker-buffer org-clock-marker))
    (let* ((org-clock-user-idle-seconds (org-user-idle-seconds))
           (org-clock-user-idle-start
            (time-subtract (current-time)
                           (seconds-to-time org-clock-user-idle-seconds)))
           (org-clock-resolving-clocks-due-to-idleness t))
      (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
          (org-clock-resolve
           (cons org-clock-marker
                 org-clock-start-time)
           (lambda (_)
             (format "Clocked in & idle for %.1f mins"
                     (/ (float-time
                         (time-subtract (current-time)
                                        org-clock-user-idle-start))
                        60.0)))
           org-clock-user-idle-start)))))

(defvar org-clock-current-task nil "Task currently clocked in.")
(defvar org-clock-out-time nil) ; store the time of the last clock-out
(defvar org--msg-extra)

(provide 'org-clock-resolve-advanced)
;;; org-clock-utils-lotus.el ends here
