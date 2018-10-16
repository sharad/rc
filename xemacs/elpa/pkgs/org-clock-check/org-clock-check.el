;;; org-clock-check.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

(provide 'org-clock-check)
;;; org-clock-check.el ends here
