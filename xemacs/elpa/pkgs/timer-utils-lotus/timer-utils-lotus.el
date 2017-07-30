;;; timer-utils-lotus.el --- copy config

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

(add-to-list 'debug-tags-level-list
             '(timer 4))

;;;###autoload
(defun run-with-nonobtrusive-aware-idle-timers (longdelay repeat shortdelay repeat-after-idle fn arg &optional cancel)
  "Run a function after idle time of REPEAT + SHORTDELAY, and repeat running on every SHORTDELAY till emacs is idle if REPEAT-AFTER-IDLE is non nil.
Benefit with this timer is that it will very much ensure before running that user is not typing in emacs."
  (lexical-let* ((longdelay longdelay)
                 (repeat repeat)
                 (repeat-after-idle repeat-after-idle)
                 (shortdelay shortdelay)
                 (timer nil)
                 (subtimer nil))
    (setq timer
          (run-with-idle-timer longdelay repeat
                               (lambda (func)
                                 (unless subtimer
                                   (dmessage 'timer 7 "shortdelay %s running timer" shortdelay)
                                   (setq subtimer
                                         (run-with-nonobtrusive-timers shortdelay (if repeat-after-idle shortdelay nil) 4
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

;;;###autoload
(defun run-with-nonobtrusive-timers (idledelay repeat useridlesec fn arg)
  "Run FN with ARG only when user is not typing."
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

(provide 'timer-utils-lotus)
;;; timer-utils-lotus.el ends here
