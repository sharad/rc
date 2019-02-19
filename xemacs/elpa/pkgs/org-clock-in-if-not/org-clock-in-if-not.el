;;; org-clock-in-if-not.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

(provide 'org-clock-in-if-not)


(require 'lotus-misc-utils)


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
  (message "%s: org-clock-in-if-not: begin" (time-stamp-string))
  (lwarn 'org-ci-if-not :debug "%s: org-clock-in-if-not: begin" (time-stamp-string))
  (run-unobtrusively                    ;heavy task
    (lotus-with-no-active-minibuffer-if
        (lotus-with-other-frame-event-debug "org-clock-in-if-not" :restart
          (progn
            (lwarn 'org-ci-if-not :debug "%s: org-clock-in-if-not: [minibuff body] lotus-with-no-active-minibuffer-if" (time-stamp-string))
            (lwarn 'org-ci-if-not :debug
             "%s: org-clock-in-if-not: not running as minibuffer is already active."
             (time-stamp-string))
            (message
             "%s: org-clock-in-if-not: not running as minibuffer is already active."
             (time-stamp-string)))

        (lwarn 'org-ci-if-not :debug "%S: org-clock-in-if-not: [body] lotus-with-no-active-minibuffer-if" (time-stamp-string))
        (unless (or
                 org-donot-try-to-clock-in
                 (org-clock-is-active))
          ;; (org-clock-goto t)
          (message "%s: org-clock-in-if-not: really calling" (time-stamp-string))
          (if org-clock-history
              (let (buffer-read-only)
                (org-clock-in '(4)))
            ;; with-current-buffer should be some real file
            (org-clock-in-refile nil))))))
  (message "%s: org-clock-in-if-not: finished" (time-stamp-string))
  (lwarn 'org-ci-if-not :debug "%s: org-clock-in-if-not: finished" (time-stamp-string)))


(defvar org-clock-in-if-not-at-time-timer nil)

;;;###autoload
(defun org-clock-in-if-not-at-time (delay)
  (prog1
      (setq org-clock-in-if-not-at-time-timer
            (run-at-time-or-now delay
                                #'(lambda ()
                                    (if (any-frame-opened-p)
                                        (org-clock-in-if-not)))))
    (message
     "%s: org-clock-in-if-not-at-time: begin timer=%s after %d secs"
     (time-stamp-string)
     org-clock-in-if-not-at-time-timer
     delay)))

;;;###autoload
(defun org-clock-in-if-not-at-time-delay ()
  (message "%s: org-clock-in-if-not-at-time-delay: begin after %d secs"
           (time-stamp-string)
           org-clock-in-if-not-delay)
  (org-clock-in-if-not-at-time org-clock-in-if-not-delay))

;;;###autoload
(defun org-clock-in-if-not-at-time-delay-fn ()
  (message
   "%s: org-clock-in-if-not-at-time-delay-fn begin"
   (time-stamp-string))
  (org-clock-in-if-not-at-time-delay))

(defun org-clock-out-if-active ()
  (if (and
       (org-clock-is-active)
       (y-or-n-p-with-timeout
        (format "Do you want to clock out current task %s: " org-clock-heading)
        7 nil))
      (org-with-clock-writeable
       (let (org-log-note-clock-out)
         (if (org-clock-is-active)
             (org-clock-out))))))
;;; org-clock-in-if-not.el ends here
