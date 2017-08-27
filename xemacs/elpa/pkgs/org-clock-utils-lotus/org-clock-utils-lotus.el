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

(require 'org)
(require 'org-clock)
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










;; (find
;;   org-clock-leftover-time
;;   org-clock-default-task ;; M-x org-clock-mark-default-task
;;   M-x org-clock-select-task
;; (org-clocking-buffer)
;; (org-clock-sum-today)
;; (org-clock-sum-custom nil 'today)
;; (org-clock-is-active)
;; )



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

;;;###autoload
(defun lotus-org-clock-in/out-insinuate-hooks ()
  (add-hook 'org-clock-out-hook
            '(lambda ()
              (if (and
                   (boundp' org-timer-countdown-timer)
                   org-timer-countdown-timer)
                  (org-timer-stop))
              (org-clock-get-work-day-clock-string t)
              (save-buffer)
              (org-save-all-org-buffers)))

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
                      (org-timer-set-timer nil))
                    (call-interactively 'org-timer-set-timer)))
              (save-buffer)
              (org-save-all-org-buffers))))

(defvar org-refile-base-file nil)

;;;###autoload
(defun lotus-setup-org-refile-base-file (file)
  (setq org-refile-base-file file))

;; org-refile-targets is set in org-misc-utils-lotus package
(defun lotus-org-clock-in-refile (refile-targets)
  (when org-refile-base-file
    (with-current-buffer (find-file-noselect org-refile-base-file)
      (org-with-refile (or refile-targets org-refile-targets)
        (let ((buffer-read-only nil))
          (org-clock-in))))))

(defvar org-donot-try-to-clock-in nil
  "Not try to clock-in, require for properly creating frame especially for frame-launcher function.")

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
        (lotus-org-clock-in-refile nil))))

(defun org-clock-out-with-note (note &optional switch-to-state fail-quietly at-time)
  (interactive
   (let ((note (read-from-minibuffer "Closing notes: "))
         (switch-to-state current-prefix-arg))
     (list note switch-to-state)))
  (let ((org-log-note-clock-out t))
    (move-marker org-log-note-return-to nil)
    (move-marker org-log-note-marker nil)
    (org-clock-out switch-to-state fail-quietly at-time)
    (remove-hook 'post-command-hook 'org-add-log-note)
    (org-insert-log-note note)))

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
;;                   (org-with-clock-writeable-buffer
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
;;        (org-with-clock-writeable-buffer
;;         (let (org-log-note-clock-out)
;;           (if (org-clock-is-active)
;;               (org-clock-out)))))))





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


(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
