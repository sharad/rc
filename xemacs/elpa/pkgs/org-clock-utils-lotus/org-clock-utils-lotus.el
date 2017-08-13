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

;;; Code:


(require 'timer-utils-lotus)


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
    (lambda ()
      ;; (message "after 7 sec.")
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

(progn ;; "correction org-timer.el"

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
                (message "No timer set")))))))




(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
