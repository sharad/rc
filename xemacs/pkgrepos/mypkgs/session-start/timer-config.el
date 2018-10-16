;;; timer-config.el --- Timer Configuration

;; Copyright (C) 2015  sharad

;; Author: sharad;;; timer-config.el --- <sh4r4d _at_ _G-mail_>
;; Keywords:convenience

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

;; Timer configurations

;;; Code:

(add-to-list 'debug-tags-level-list
             '(timer 4))

(defun run-with-nonobtrusive-aware-idle-timers (longdelay repeat shortdelay fn arg &optional cancel)
  "Run a function after idle time of N, but will try to run when user"
  (lexical-let* ((longdelay longdelay)
                 (repeat repeat)
                 (shortdelay shortdelay)
                 (timer nil)
                 (subtimer nil))
    (setq timer
          (run-with-idle-timer longdelay repeat
                               (lambda (func)
                                 (unless subtimer
                                   (dmessage 'timer 7 "shortdelay %s running timer" shortdelay)
                                   (setq subtimer
                                         (run-with-nonobtrusive-timers shortdelay shortdelay 4
                                                                       (lambda (func1)
                                                                         (progn
                                                                           (dmessage 'timer 7 "shortdelay %s running fun" shortdelay)
                                                                           (if (funcall func1)

                                                                               ;; (when subtimer
                                                                               ;;   (cancel-timer subtimer)
                                                                               ;;   (setq subtimer nil))

                                                                               (when (and cancel timer)
                                                                                 (cancel-timer timer)
                                                                                 (setq timer nil)))

                                                                           (when subtimer
                                                                             (cancel-timer subtimer)
                                                                             (setq subtimer nil))

                                                                           ;; (when subtimer
                                                                           ;;   (cancel-timer subtimer)
                                                                           ;;   (setq subtimer nil))

                                                                           ))
                                                                       func))))
                               fn))))

(defun run-with-nonobtrusive-timers (idledelay repeat useridlesec fn arg)
  ""
  (lexical-let* ((idledelay idledelay)
                 (repeat repeat)
                 (useridlesec useridlesec)
                 (useridlesec-moving useridlesec)
                 (timer nil))
    (setq timer
          (run-with-idle-timer
           idledelay
           repeat
           (lambda (func)
             (let ((current-idle-sec (float-time (or (current-idle-time) '(0 0 0)))))
               (if (>= current-idle-sec useridlesec-moving) ;NOTE
                   (progn
                     (dmessage 'timer 7 "shortob running fun useridlesec-moving %s" useridlesec-moving)
                     (funcall (car func) (cdr func))
                     (setq useridlesec-moving useridlesec)
                     (when timer
                       (cancel-timer timer)
                       (setq timer nil))
                     t)
                   (progn
                     (dmessage 'timer 7 "shortob not running fun useridlesec-moving %s" useridlesec-moving)
                     (unless (zerop useridlesec-moving)
                       (decf useridlesec-moving))
                     nil))))
           (cons fn arg)))))

(when nil
  (progn
    (defvar nonobtrusive-test-timer nil)
    (if nonobtrusive-test-timer (cancel-timer nonobtrusive-test-timer))
    (setq nonobtrusive-test-timer
          (run-with-nonobtrusive-aware-idle-timers 4 4 2
                                                   (lambda ()
                                                     (dmessage 'timer 7 "Hello Hi World")
                                                     (if nonobtrusive-test-timer (cancel-timer nonobtrusive-test-timer))
                                                     (setq nonobtrusive-test-timer nil))
                                                   nil
                                                   t))))

(setq
 display-time-world-time-format "%A %d %B %R %Z"
 display-time-world-time-format "    %10A %e %B, %Y  %r"
 display-time-world-list                ;; used by M-x display-time-world
 '(("America/Vancouver"   "Vancouver")
   ("America/Los_Angeles" "California")
   ("Asia/Calcutta"       "Bangalore")
   ("Europe/London"       "London")
   ("Europe/Paris"        "Paris")
   ("Asia/Tokyo"          "Tokyo")
   ("America/Los_Angeles" "Seattle")
   ("America/New_York"    "New York")))

(display-time-mode 1)

(provide 'timer-config)
;;; timer-config.el ends here
