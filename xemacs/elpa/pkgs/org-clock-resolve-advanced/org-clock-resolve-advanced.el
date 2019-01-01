;;; org-clock-resolve-advanced.el ---   -*- lexical-binding: t; -*-

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

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(provide 'org-clock-resolve-advanced)


(defun org-clock-idle-time-set (mins)
  (interactive
   (list (read-number "org-clock-idle-time: "
                      (if (numberp org-clock-idle-time)
                          org-clock-idle-time
                        5))))
  (setq org-clock-idle-time mins))

(defun org-clock-steel-time ()
  )


;; (defvar org-clock-clocking-in nil)
;; (defvar org-clock-resolving-clocks nil)
;; (defvar org-clock-resolving-clocks-due-to-idleness nil)

(defun make-rl-clock (marker start stop)
  (list marker start stop))

(defun org-get-heading-from-clock (clock)
  (if (markerp (car clock))
      (lotus-with-marker (car clock)
        (org-get-heading t))
      "imaginary"))

(setq org-resolve-opts-common
      '(("Done" . done)))

(defun org-resolve-opts-common (clock)
  (list (cons "Done" 'done)))

(defun org-resolve-opts-common-with-time (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons "Include in other" 'include-in-next)
     (cons
      (format "subtract from prev %s" heading)
      'subtract))))

(defun org-resolve-opts-prev (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons
      (format "Cancel prev %s" heading)
      'cancel-prev-p)
     (cons
      (format "Jump to prev %s" heading)
      'jump-prev-p))))

(defun org-resolve-opts-prev-with-time (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons
      (format "Include in prev %s" heading)
      'include-in-prev))))

(defun org-resolve-opts-next (clock)
  (let ((heading (org-get-heading-from-clock clock))
        (marker (car clock)))
    (list
     (cons
      (if (eq marker 'imaginary)
          "Ignore all idle time"
          (format "Cancel next %s" heading))
      'cancel-next-p)
     (cons
      (format "Jump to next %s" heading)
      'jump-next-p))))

(defun org-resolve-opts-next-with-time (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons
      (format "Include in next %s" heading)
      'include-in-next))))

(defun time-get-rl-time (time)
  (cond
    ((eq time 'now)
     (current-time))
    ((eq time nil) nil)
    (time time)
    (t nil)))

(defun org-rl-clock-start-time (clock)
  (time-get-rl-time (nth 1 clock)))

(defun org-rl-clock-stop-time (clock)
  (time-get-rl-time (nth 2 clock)))

(defun org-rl-clock-marker (clock)
  (nth 0 clock))

(defun org-rl-clock-name (clock)
  ;;(org-rl-clock-marker clock)
  (org-get-heading-from-clock clock))


(defun org-rl-clock-start-time-set (clock time)
  (setf (cadr clock) (time-get-rl-time time))
  clock)

(defun org-rl-clock-stop-time-set (clock time)
  (setf (caddr clock) (time-get-rl-time time))
  clock)

(defun org-rl-clock-marker-set (clock marker)
  (setf (car clock) marker)
  clock)

;; helpers
;; - org-clock-clock-out
;; - org-clock-clock-in
(defun org-clock-clock-remove-last-clock (clock)
  ;; TODO
  )

(defun org-rl-clock-clock-in-out (clock &optional resume fail-quietly)
  (when (not org-clock-clocking-in)
    (org-clock-clock-in clock resume)
    (org-clock-clock-out clock fail-quietly)))

(defun org-rl-clock-clock-in (clock &optional resume)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-marker clock)
        (if (org-rl-clock-start-time clock)
            (org-clock-clock-in
             (cons
              (org-rl-clock-marker clock)
              (org-rl-clock-start-time clock))
             resume
             (org-rl-clock-start-time clock))
            (error "%s start time is null" (org-rl-clock-start-time clock)))
        (error "%s clock is null" (org-rl-clock-marker clock)))))

(defun org-rl-clock-clock-out (clock &optional fail-quietly)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-marker clock)
        (if (org-rl-clock-stop-time clock)
            (org-clock-clock-out
             (cons
              (org-rl-clock-marker clock)
              (org-rl-clock-start-time clock))
             fail-quietly
             (org-rl-clock-stop-time clock))
            (error "%s stop time is null" (org-rl-clock-stop-time clock)))
        (error "%s clock is null" (org-rl-clock-marker clock)))))

(defun org-rl-clock-clock-cancel (clock &optional fail-quietly)
  (if (org-rl-clock-marker clock)
      (if (org-rl-clock-start-time clock)
          (org-clock-clock-cancel
           (cons
            (org-rl-clock-marker clock)
            (org-rl-clock-start-time clock)))
          (error "%s start time is null" (org-rl-clock-start-time clock)))
      (error "%s clock is null" (org-rl-clock-marker clock))))

(defun org-rl-clock-clock-jump-to (clock)
  (if (org-rl-clock-marker clock)
      (org-clock-jump-to-current-clock
       (cons
        (org-rl-clock-marker clock)
        (org-rl-clock-start-time clock)))))

(defun org-rl-select-other-clock (&optional target)
  (interactive)
  (org-with-refile
      file loc (or target org-refile-targets)
      (let ((marker (make-marker)))
        (set-marker marker loc)
        marker)))

(defun org-resolve-time-debug-prompt (prev next &optional prompt stop)
  (let* (;;(base 120)
         (base 61)
         (_debug (format "prev[%s %d %d] next[%s %d %d]"
                         ;; (org-rl-clock-marker prev)
                         (org-rl-clock-name prev)
                         (if (org-rl-clock-start-time prev) (% (/ (floor (float-time (org-rl-clock-start-time prev))) 60) base) 0)
                         (if (org-rl-clock-stop-time prev)  (% (/ (floor (float-time (org-rl-clock-stop-time prev))) 60) base) 0)
                         ;; (org-rl-clock-marker next)
                         (org-rl-clock-name next)
                         (if (org-rl-clock-start-time next) (% (/ (floor (float-time (org-rl-clock-start-time next))) 60) base) 0)
                         (if (org-rl-clock-stop-time next)  (% (/ (floor (float-time (org-rl-clock-stop-time next))) 60) base) 0)))
         (debug (if prompt (concat prompt " " _debug) _debug)))
    (when stop (read-from-minibuffer (format "%s test: " debug)))
    debug))

(defun org-rl-get-time-gap (prev next)
  (/
   (floor
    (float-time
     (time-subtract
      (org-rl-clock-start-time next)
      (or
       (org-rl-clock-stop-time prev)
       (if (eq (org-rl-clock-marker next) 'imaginary)
           (org-rl-clock-stop-time next)
           (error "Can not get start time."))))))
   60))

(defun org-resolve-time (prev next &optional force close-p)
  ;; BUG how to handle current time == 'now
  ;; BUG how to handle when prev == next
  "Resolve clock time"
  (interactive)
  ;; last-input-event
  ;; last-event-frame
  ;; TODO: send some tag or signal when other frame selection
  ;; set pre-command-hook to know if other frame is getting focus
  ;; than save data for this function and abort this function invocation here
  ;; again run this function in that frame.
  (lotus-with-override-minibuffer
    (let ((debug-prompt t)
          (default (org-rl-get-time-gap prev next)))

      ;;;

      (lwarn 'org-rl-clock :warning "going to run %s with default %d" (org-resolve-time-debug-prompt prev next) default)

      (assert (> default 0))

      (let* ((options
              (append
               (when (markerp (org-rl-clock-marker prev))
                 (append
                  (org-resolve-opts-prev prev)
                  (unless (zerop default) (org-resolve-opts-prev-with-time prev))))
               (when (markerp (org-rl-clock-marker next))
                 (append
                  (org-resolve-opts-next next)
                  (unless (zerop default) (org-resolve-opts-next-with-time next))))
               (unless (zerop default) (org-resolve-opts-common-with-time prev))
               (org-resolve-opts-common prev)))
             (opt
              (cdr
               (assoc
                (completing-read
                 (if debug-prompt
                     (format "%s Select option [%d]: " (org-resolve-time-debug-prompt prev next) default)
                   (format "Select option [%d]: " default))
                 options)
                options)))
             (timelen
              (let ((default (org-rl-get-time-gap prev next)))
                (if (or (zerop default)
                        (memq opt
                              '(done
                                cancel-next-p
                                cancel-prev-p)))
                    default
                  (read-number
                   (if debug-prompt
                       (format "%s [%s] how many minutes? [%d] " (org-resolve-time-debug-prompt prev next) opt default)
                     (format "[%s] how many minutes? [%d] " opt default))
                   default)))))

        ;; (barely-started-p (< (- (float-time last-valid)
        ;;                         (float-time (cdr clock))) 45))
        ;; (start-over-p (and subtractp barely-started-p))

        ;; cancel prev and add to time

        (lwarn 'org-rl-clock :warning "You have selected opt %s and timelen %d" opt timelen)

        (let ((default (org-rl-get-time-gap prev next))) ;get default time again

          (when (> (abs timelen) default)
            (message "Error given time %d can not be greater than %d" timelen default)
            (org-resolve-time prev next))

          (let ((timelensec-time (seconds-to-time (* timelen 60))))
            (cond
             ((eq opt 'jump-prev-p)
              ;; finish here
              (org-rl-clock-clock-jump-to prev))

             ((eq opt 'jump-next-p)
              ;; finish here
              (org-rl-clock-clock-jump-to next))

             ((eq opt 'cancel-prev-p)
              (progn
                (org-rl-clock-clock-cancel prev)
                (let ((prev-start (cdr prev)))
                  (setq prev
                        (list
                         nil
                         nil
                         (org-rl-clock-start-time prev))))))
             ;; set org-clock-leftover-time here


             ((eq opt 'cancel-next-p)
              ;; cancel next clock
              ;; add next clock time
              (progn
                (org-rl-clock-clock-cancel next)
                (setq next
                      (list
                       nil
                       (org-rl-clock-stop-time prev)
                       nil))))

             ((eq opt 'include-in-prev)
              ;; include timelen in prev
              ;; update timelength
              (let ((updated-time (time-add
                                   (org-rl-clock-start-time prev) timelensec-time)))
                (if (> timelen 0)
                    (progn
                      (org-rl-clock-stop-time-set prev updated-time)
                      (org-rl-clock-clock-out prev))
                  (progn
                    (org-rl-clock-clock-out prev)
                    (setq next (list
                                (org-rl-clock-marker prev) updated-time (org-rl-clock-start-time next)))
                    (org-rl-clock-clock-in-out next)))))

             ;; set org-clock-leftover-time here



             ((eq opt 'include-in-next)
              (when (and             ;clock out if prev is open clock and next is specifying idle time.
                     (null (org-rl-clock-stop-time prev))
                     (org-rl-clock-stop-time next))
                (org-rl-clock-clock-out
                 (make-rl-clock (org-rl-clock-marker prev)
                                (org-rl-clock-start-time next)
                                (org-rl-clock-stop-time next))))
              ;; include timelen in next
              ;; update timelength
              (if (> timelen 0)

                  (let ((updated-start-time (time-add
                                             (org-rl-clock-start-time next) timelensec-time)))
                    (org-rl-clock-start-time-set next updated-start-time)
                    (when (org-rl-clock-marker next)
                      (org-rl-clock-clock-in next))) ;? imple

                (let ((updated-stop-time (time-add
                                          (org-rl-clock-start-time prev) timelensec-time)))
                  (setq prev (list
                              (org-rl-clock-marker next)
                              (org-rl-clock-start-time prev)
                              updated-stop-time))
                  ;; make prev is clocked out
                  (when (org-rl-clock-marker prev)
                    (org-rl-clock-clock-in-out prev)))))

             ((memq opt '(include-in-other
                          subtract)) ;; subtract timelen from timelength
              ;; select other clock
              ;; include timelen in other
              ;; update timelength
              ;; (if debug-prompt (org-resolve-time-debug-prompt prev next t "include-in-other"))

              (let ((other-marker
                     (if (eq opt 'include-in-other)
                         (org-rl-select-other-clock)
                       nil)))

                (if (> timelen 0)
                    (let* ((other-start-time (time-subtract
                                              (org-rl-clock-start-time next)
                                              timelensec-time))
                           (other-clock
                            (make-rl-clock other-marker other-start-time (org-rl-clock-start-time next))))

                      (setq next
                            (if (eq (org-rl-clock-marker next) 'imaginary)
                                (org-rl-clock-start-time-set next other-start-time)
                              other-clock))
                      (when other-marker
                        (org-rl-clock-clock-in-out other-clock)))

                  (let* ((other-stop-time
                          (time-subtract (org-rl-clock-stop-time prev) timelensec-time))
                         (other-clock
                          (make-rl-clock other-marker (org-rl-clock-start-time next) other-stop-time)))

                    (when (and         ;clockout prev clock
                           (null (org-rl-clock-stop-time prev))
                           (org-rl-clock-stop-time next))
                      (org-rl-clock-stop-time-set prev (org-rl-clock-stop-time next))
                      (org-rl-clock-clock-out prev t))

                    (setq prev other-clock)
                    (when other-marker (org-rl-clock-clock-in-out other-clock))))))

             ((eq opt 'done))
             (t (error "Error")))))

        (unless (eq opt 'done)
          (when (and (zerop default) close-p)
            (org-clock-out))
          (org-resolve-time prev next close-p))))))

;;;###autoload

(defvar org-clock-last-user-idle-seconds nil )

(defun org-rl-resolve-clocks-if-idle ()
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  ;; last-input-event
  ;; last-event-frame
  (when nil (message "called org-rl-resolve-clocks-if-idle"))
  (lotus-with-other-frame-event-debug "org-rl-resolve-clocks-if-idle" :restart
    (message "org-clock-last-user-idle-seconds: %s" org-clock-last-user-idle-seconds)
    ;; (message "(org-user-idle-seconds) %s" (org-user-idle-seconds))
    (when (and
           org-clock-idle-time
           (not org-clock-resolving-clocks)
           org-clock-marker
           (marker-buffer org-clock-marker))
      (let* ((org-clock-user-idle-seconds
              (or
               org-clock-last-user-idle-seconds
               (org-user-idle-seconds)))
             (org-clock-user-idle-start
              (time-subtract (current-time)
                             (seconds-to-time org-clock-user-idle-seconds)))
             (org-clock-resolving-clocks-due-to-idleness t))

        (setq org-clock-last-user-idle-seconds org-clock-user-idle-seconds)

        (when nil
         (message "1. Idle time now sec[%d] min[%d]"
                 org-clock-user-idle-seconds
                 (/ org-clock-user-idle-seconds 60)))

        (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
            (org-resolve-time
             (make-rl-clock org-clock-marker org-clock-start-time nil)
             (make-rl-clock 'imaginary 'now org-clock-user-idle-start))
          (when nil
            (message "2. Idle time now sec[%d] min[%d]"
                     org-clock-user-idle-seconds
                     (/ org-clock-user-idle-seconds 60))))
        (setq org-clock-last-user-idle-seconds nil)))))

(defalias 'org-resolve-clocks-if-idle 'org-rl-resolve-clocks-if-idle)

(defun org-rl-clock-set-correct-idle-timer ()
  (interactive)
  (let* ((minor-offset 10)
         (idle-time (+ (* org-clock-idle-time 60) minor-offset)))
    (when org-clock-idle-timer
      (cancel-timer org-clock-idle-timer)
      (setq org-clock-idle-timer
            (run-with-idle-timer
             idle-time idle-time
             'org-rl-resolve-clocks-if-idle)))))

;;;###autoload
(defun org-rl-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  ;; last-input-event
  ;; last-event-frame
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
        (let ((clocks (org-find-open-clocks file)))
          (dolist (clock clocks)
            (let ((dangling (or (not (org-clock-is-active))
                                (/= (car clock) org-clock-marker))))
              (when (or (not only-dangling-p) dangling)
                  (org-resolve-time
                   (make-rl-clock (car clock) (cdr clock) nil)
                   (make-rl-clock 'imaginary 'now (cdr clock)))))))))))

(defalias 'org-resolve-clocks 'org-rl-resolve-clocks)

;;;###autoload
(defun org-clock-resolve-clocks (clocks) ;TODO
  (let ((next (pop clocks))
        (prev (pop clocks)))
    (org-resolve-time next prev)))

;;;###autoload
(defun org-clock-resolve-advanced-insinuate ()
  (interactive)
  (defalias 'org-resolve-clocks-if-idle 'org-rl-resolve-clocks-if-idle)
  (add-hook 'org-clock-in-hook
            #'org-rl-clock-set-correct-idle-timer)
  (defalias 'org-resolve-clocks 'org-rl-resolve-clocks))

(defun org-clock-resolve-advanced-uninsinuate ()
  (remove-hook 'org-clock-in-hook
               #'org-rl-clock-set-correct-idle-timer))

(when nil                               ;testing
  (let ((currtime (current-time)))
    (org-resolve-time
     (list
      org-clock-marker
      org-clock-start-time
      nil)
     (list
      'imaginary
      'now
      (time-subtract currtime (seconds-to-time (* 8 60)))))))


(defun test-org-rl-resolve-clocks-if-idle (idle-sec)
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (when (and
         org-clock-idle-time
         (not org-clock-resolving-clocks)
         org-clock-marker
         (marker-buffer org-clock-marker))
    (let* ((org-clock-user-idle-seconds idle-sec)
           (org-clock-user-idle-start
            (time-subtract (current-time)
                           (seconds-to-time org-clock-user-idle-seconds)))
           (org-clock-resolving-clocks-due-to-idleness t))
      (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
          (org-resolve-time
           (make-rl-clock org-clock-marker org-clock-start-time nil)
           (make-rl-clock 'imaginary 'now org-clock-user-idle-start))
          (when nil
            (message "Idle time now sec[%d] min[%d]"
                     org-clock-user-idle-seconds
                     (/ org-clock-user-idle-seconds 60)))))))



(when nil
  (test-org-rl-resolve-clocks-if-idle 310)
  )

;;; org-clock-utils-lotus.el ends here
