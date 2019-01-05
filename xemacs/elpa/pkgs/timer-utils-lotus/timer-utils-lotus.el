;;; timer-utils-lotus.el --- copy config  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; (defvar debug-tags-level-list)          ;prepare debug it is in xlotus-debug-config

;; (add-to-list 'debug-tags-level-list
;;             '(timer 4))

(defun debug-message (fmt &rest args)
  (when nil
    (apply #'message fmt args)))

;;;###autoload
(defun run-with-idle-plus-timer (secs repeat function &rest args)
  (let* ((idle-time (current-idle-time))
         (secs (+ (if idle-time (float-time idle-time) 0) secs)))
    ;; (message "will run fun %s after %s + %d" function idle-time secs)
    (apply #'run-with-idle-timer secs repeat function args)))

;;;###autoload
(defun run-with-nonobtrusive-aware-idle-timers (longdelay repeat shortdelay repeat-after-idle fn arg &optional cancel)
  "Run a function after idle time of REPEAT + SHORTDELAY, and repeat running on every SHORTDELAY till emacs is idle if REPEAT-AFTER-IDLE is non nil.
Benefit with this timer is that it will very much ensure before running that user is not typing in emacs."
  (let* ((longdelay longdelay)
         (repeat repeat)
         (repeat-after-idle repeat-after-idle)
         (shortdelay shortdelay)
         (timer nil)
         (subtimer nil)
         (cancel cancel))
    (setq timer
          (run-with-idle-timer longdelay repeat
                               #'(lambda (func-arg)
                                   (unless subtimer
                                     (debug-message "shortdelay %s running timer" shortdelay)
                                     (setq subtimer
                                           (run-with-nonobtrusive-timers shortdelay
                                                                         (if repeat-after-idle shortdelay nil)
                                                                         4
                                                                         #'(lambda (func-arg1)
                                                                             (progn
                                                                               (debug-message "shortdelay %s running fun" shortdelay)
                                                                               (if (funcall (car func-arg1) (cdr func-arg1))
                                                                                   (when (and cancel timer)
                                                                                     (cancel-timer timer)
                                                                                     (setq timer nil)))
                                                                               (when subtimer
                                                                                 (cancel-timer subtimer)
                                                                                 (setq subtimer nil))))
                                                                         func-arg))))
                               (cons fn arg)))))

;;;###autoload
(defun run-with-nonobtrusive-timers (idledelay repeat useridlesec fn1 arg1)
  "Run FN with ARG only when user is not typing."
  (let* ((idledelay idledelay)
         (repeat repeat)
         (useridlesec useridlesec)
         (useridlesec-moving useridlesec)
         (timer nil))
    (setq timer
          (run-with-idle-timer
           idledelay
           repeat
           (lambda (func-arg2)
             (let ((current-idle-sec (float-time (or (current-idle-time) '(0 0 0)))))
               (if (>= current-idle-sec useridlesec-moving) ;NOTE
                   (progn
                     (debug-message "shortob running fun useridlesec-moving %s" useridlesec-moving)
                     (funcall (car func-arg2) (cdr func-arg2))
                     (setq useridlesec-moving useridlesec)
                     (when timer
                       (cancel-timer timer)
                       (setq timer nil))
                     t)
                   (progn
                     (debug-message "shortob not running fun useridlesec-moving %s" useridlesec-moving)
                     (unless (zerop useridlesec-moving)
                       (decf useridlesec-moving))
                     nil))))
           (cons fn1 arg1)))))



(defun run-with-idle-timer-nonobtrusive (sec repeat sitfor stepsec fn &optional arg)
  (let* ((sitfor sitfor)
         (sec sec)
         idle-timer)
    (setq idle-timer
          (run-with-idle-timer sec nil
                               #'(lambda (fnarg)
                                   (let ((current-idle-sec (float-time (or (current-idle-time) '(0 0 0)))))
                                     (if (>= current-idle-sec (+ sec sitfor))
                                         (progn
                                           (unless repeat
                                             (cancel-timer idle-timer))
                                           (funcall (car fnarg) (cdr fnarg)))
                                       (setq idle-timer nil))))
                               (cons fn arg)))
    (message "started time idle-timer=%s, utiliz stepsec=%s later"
             idle-timer
             stepsec)))


(defun run-with-idle-timer-nonobtrusive-simple (sec repeat fn &optional arg)
  (run-with-idle-timer sec repeat fn arg))

(when nil
  (progn
    (defvar nonobtrusive-test-timer nil)
    (if nonobtrusive-test-timer (cancel-timer nonobtrusive-test-timer))
    (setq nonobtrusive-test-timer
          (run-with-nonobtrusive-aware-idle-timers 4 4 2
                                                   (lambda ()
                                                     (debug-message "Hello Hi World")
                                                     (if nonobtrusive-test-timer (cancel-timer nonobtrusive-test-timer))
                                                     (setq nonobtrusive-test-timer nil))
                                                   nil
                                                   t))))

(provide 'timer-utils-lotus)
;;; timer-utils-lotus.el ends here
