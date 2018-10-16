;;; org-clock-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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

;; (require 'org-clock-check)
;; (require 'org-clock-hooks)
;; (require 'org-clock-experimental)
;; (require 'org-clock-unnamed-task)
;; (require 'org-clock-wrapper)

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

(defun lotus-org-clock-detect-first-clockin-of-day ()
  ;; do necessary stuff
  ;; like context presentation etc.
  )

(defun lotus-org-clock-declare-last-clockout-of-day ()
  )

(defun lotus-org-clock-offline-time ()
;; clock the time from last known clock to now
  )

(defun lll ()
  ;; schedule deadline to honour end of day time also.
 )

(defun lll ()
  ;; calculate schedule deadline whether available in given time
 )

(defun org-define-a-task ()
  "This function is used to create a org tree to complete a task.
for e.g. implementing lvm support for guixsd what all steps a person have to take
he has to read scheme, guixsd details, than see similar module and try to implement it."
  (interactive)
  )

(defun org-log-not-on-event (start end event)
  )

(defun org-goto-refile (&optional refile-targets)
  "Refile goto."
  ;; mark paragraph if no region is set
  (let* ((org-refile-targets (or refile-targets org-refile-targets))
         (target (save-excursion (safe-org-refile-get-location)))
         (file (nth 1 target))
         (pos (nth 3 target)))
    (when (set-buffer (find-file-noselect file)) ;; (switch-to-buffer (find-file-noselect file) 'norecord)
      (goto-char pos))))

(defun org-clock-new-task ()
  (interactive)
  (org-capture nil "x"))

;;;}}}


(defun get-last-cloc (tstart)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart))))

;; (org-back-to-heading t)

;; macro org-with-silent-modifications

;; (org-element-type (org-element-context))
;; (eq (org-element-type (save-match-data (org-element-at-point))) 'clock)

;;; FOR WORKING FAST START CREATING TEMPLATE OR EMPTY FUNCTION BODY.


(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
