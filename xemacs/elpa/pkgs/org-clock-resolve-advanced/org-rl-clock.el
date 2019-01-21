;;; org-rl-clock.el --- Org resolve clock            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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

(provide 'org-rl-clock)

(require 'cl-macs)
(require 'cl-generic)

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
(eval-when-compile
  (require 'org-clock-utils-lotus))
(require 'org-clock-utils-lotus)


(defun time-p (time)
  (or
   (eq 'now time)
   (and
     (consp time)
     (nth 1 time))))

(cl-defstruct org-rl-time
  time
  dirty)

(cl-defstruct org-rl-clock
  marker
  start
  stop
  active
  cancel)

(defun org-rl-make-clock (marker
                          start-time
                          stop-time
                          &optional
                          start-dirty
                          stop-dirty
                          active
                          cancel)
 (make-org-rl-clock
  :marker marker
  :start (make-org-rl-time :time start-time :dirty start-dirty)
  :stop  (make-org-rl-time :time stop-time  :dirty stop-dirty)
  :active active
  :cancel cancel))

(defun org-rl-make-time (time &optional dirty)
  (make-org-rl-time :time time :dirty dirty))

(cl-defmethod org-rl-time-get-time ((time org-rl-time))
  (let ((rl-time (org-rl-time-time time)))
    (when rl-time
      (if (time-p rl-time)
          (if (eq rl-time 'now)
              (current-time)
            rl-time)
        (error "Wring time %s passed." rl-time)))))

(cl-defmethod org-rl-clock-start-time ((clock org-rl-clock))
  (org-rl-time-get-time
   (org-rl-clock-start clock)))
(cl-defmethod org-rl-clock-start-set ((clock org-rl-clock)
                                      time
                                      &optional
                                      dirty)
  (setf
   (org-rl-clock-start clock)
   (org-rl-make-time time dirty)))
(cl-defmethod org-rl-clock-stop-time ((clock org-rl-clock))
  (org-rl-time-get-time
   (org-rl-clock-stop clock)))
(cl-defmethod org-rl-clock-stop-set ((clock org-rl-clock)
                                     time
                                     &optional
                                     dirty)
  (setf
   (org-rl-clock-stop clock)
   (org-rl-make-time time dirty)))

(cl-defmethod org-rl-clock-start-dirty ((clock org-rl-clock))
  (org-rl-dirty-dirty (org-rl-clock-start clock)))
(cl-defmethod org-rl-clock-stop-dirty ((clock org-rl-clock))
  (org-rl-dirty-dirty (org-rl-clock-stop clock)))

(cl-defmethod org-rl-clock-first-clock-beginning ((clock org-rl-clock))
  (org-clock-get-nth-half-clock-beginning
   (org-rl-clock-marker clock)))

(cl-defmethod org-rl-clock-null ((clock org-rl-clock))
  (or
   (eq (org-rl-clock-marker clock) 'imaginary)
   (null (org-rl-clock-marker clock))))


(defun org-get-heading-from-clock (clock)
  (if (markerp (car clock))
      (lotus-with-marker (car clock)
        (org-get-heading t))
    "imaginary"))

(cl-defmethod org-rl-clock-heading ((clock org-rl-clock))
  (if (markerp (org-rl-clock-marker clock))
      (lotus-with-marker (org-rl-clock-marker clock)
        (org-get-heading t))
    "imaginary"))

(cl-defmethod org-rl-format-clock ((clock org-rl-clock))
  (let ((fmt (cdr org-time-stamp-formats)))
    (let ((heading
           (if (markerp (org-rl-clock-marker clock))
               (lotus-with-marker (org-rl-clock-marker clock)
                 (org-get-heading t))
             "imaginary"))
          (start (format-time-string fmt (org-rl-clock-start-time clock)))
          (stop  (format-time-string fmt (org-rl-clock-stop-time clock))))
      (format "<%s> %s-%s" heading start stop))))

(cl-defmethod org-rl-clock-name-bracket ((clock org-rl-clock))
  ;;(org-rl-clock-marker clock)
  (concat "<" (org-rl-clock-heading clock) ">"))


(cl-defmethod org-rl-clock-for-clock-in ((clock org-rl-clock))
  (cons
   (org-rl-clock-marker clock)
   (org-rl-clock-start-time clock)))

(cl-defmethod org-rl-clock-for-clock-out ((clock org-rl-clock))
  (cons
   (org-rl-clock-first-clock-beginning clock)
   (org-rl-clock-start-time clock)))



(cl-defmethod org-clock-clock-remove-last-clock ((clock org-rl-clock)))
;; TODO

(cl-defmethod org-rl-clock-clock-cancel ((clock org-rl-clock)
                                         &optional
                                         fail-quietly)
  (org-rl-debug :warning "org-rl-clock-clock-cancel: clock[%s] fail-quietly[%s]"
                (org-rl-format-clock clock)
                fail-quietly)
  (setf (org-rl-clock-cancel clock) t)
  (if (org-rl-clock-marker clock)
      (if (org-rl-clock-start-time clock)
          (org-clock-clock-cancel
           (cons
            (org-rl-clock-marker clock)
            (org-rl-clock-start-time clock)))
        (error "%s start time is null" (org-rl-clock-start-time clock)))
    (error "%s clock is null" (org-rl-clock-marker clock))))

(cl-defmethod org-rl-clock-clock-jump-to ((clock org-rl-clock))
  (org-rl-debug :warning "org-rl-clock-clock-jump-to: clock[%s]"
                (org-rl-format-clock clock))
  (if (org-rl-clock-marker clock)
      (org-clock-jump-to-current-clock
       (cons
        (org-rl-clock-marker clock)
        (org-rl-clock-start-time clock)))))



(cl-defmethod org-rl-clock-clock-in ((clock org-rl-clock)
                                     &optional
                                     resume)
  (org-rl-debug :warning "org-rl-clock-clock-in: clock[%s] resume[%s]"
                (org-rl-format-clock clock)
                resume)
  (let ((org-clock-auto-clock-resolution nil))
    (when (not org-clock-clocking-in)
      (if (org-rl-clock-marker clock)
          (if (time-p (org-rl-clock-start-time clock))
              (org-clock-clock-in
               (org-rl-clock-for-clock-in clock)
               resume
               (org-rl-clock-start-time clock))
            (error "%s start time is null" (org-rl-clock-start-time clock)))
        (error "%s clock is null" (org-rl-clock-marker clock))))))

(cl-defmethod org-rl-clock-clock-out ((clock org-rl-clock)
                                      &optional
                                      fail-quietly)
  (org-rl-debug :warning "org-rl-clock-clock-out: clock[%s] fail-quietly[%s]"
                (org-rl-format-clock clock)
                fail-quietly)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-marker clock)
        (if (time-p (org-rl-clock-stop-time clock))
            (org-clock-clock-out (org-rl-clock-for-clock-out clock)
             fail-quietly
             (org-rl-clock-stop-time clock))
          (error "%s stop time is null" (org-rl-clock-stop-time clock)))
      (error "%s clock is null" (org-rl-clock-marker clock)))))

(cl-defmethod org-rl-clock-clock-in-out ((clock org-rl-clock)
                                         &optional
                                         resume
                                         fail-quietly)
  (org-rl-debug :warning "org-rl-clock-clock-in-out: clock[%s] resume[%s] org-clock-clocking-in[%s]"
                (org-rl-format-clock clock)
                resume
                org-clock-clocking-in)
  (let ((org-clock-auto-clock-resolution nil))
    (if (not org-clock-clocking-in)
        (progn
          (org-rl-debug :warning "org-rl-clock-clock-in-out in")

          (cl-assert (org-rl-clock-start-time clock))
          (cl-assert (org-rl-clock-stop-time clock))
          (org-rl-clock-clock-in clock resume)
          (org-rl-debug :warning "org-rl-clock-clock-in-out out")
          (org-rl-clock-clock-out clock fail-quietly)
          (org-rl-debug :warning "org-rl-clock-clock-in-out out done"))
      (error "Clock org-clock-clocking-in is %s" org-clock-clocking-in))))

(cl-defmethod org-rl-clock-action ((clock org-rl-clock)
                                   &option
                                   resume
                                   fail-quietly)
  (when (org-rl-clock-start-dirty clock)
    (org-rl-clock-clock-in clock resume fail-quietly))
  (when (org-rl-clock-start-dirty clock)
    (org-rl-clock-clock-out clock fail-quietly)))

(defun org-rl-clocks-action (resume fail-quietly &rest clocks)
  (dolist (clock clocks)
    (when nil
     (org-rl-clock-action clock resume fail-quietly)))
  clocks)


(defun org-rl-debug (level &rest args)
  (apply #'lwarn 'org-rl-clock :warning args)
  (message
   (concat
    (format "org-rl-clock %s: " :warning)
    (apply #'format args))))


(defun org-clock-idle-time-set (mins)
  (interactive
   (list (read-number "org-clock-idle-time: "
                      (if (numberp org-clock-idle-time)
                          org-clock-idle-time
                        5))))
  (setq org-clock-idle-time mins))

(defun org-clock-steel-time ()
  (interactive))



;; (defvar org-clock-clocking-in nil)
;; (defvar org-clock-resolving-clocks nil)
;; (defvar org-clock-resolving-clocks-due-to-idleness nil)


(setq
 org-rl-clock-opts-common
 '(("Done" . done)))

(cl-defmethod org-rl-clock-opts-common ((clock org-rl-clock))
  (list (cons "Done" 'done)))

(cl-defmethod org-rl-clock-opts-common-with-time ((clock org-rl-clock))
  (let ((heading (org-rl-clock-heading clock)))
    (list
     (cons "Include in other" 'include-in-other)
     (cons
      (format "subtract from prev %s" heading)
      'subtract))))

(cl-defmethod org-rl-clock-opts-prev ((clock org-rl-clock))
  (let ((heading (org-rl-clock-heading clock)))
    (list
     (cons
      (format "Cancel prev %s" heading)
      'cancel-prev-p)
     (cons
      (format "Jump to prev %s" heading)
      'jump-prev-p))))

(cl-defmethod org-rl-clock-opts-prev-with-time ((clock org-rl-clock))
  (let ((heading (org-rl-clock-heading clock)))
    (list
     (cons
      (format "Include in prev %s" heading)
      'include-in-prev))))

(cl-defmethod org-rl-clock-opts-next ((clock org-rl-clock))
  (let ((heading (org-rl-clock-heading clock))
        (marker (car clock)))
    (list
     (cons
      (if (org-rl-clock-null clock)
          "Ignore all idle time"
          (format "Cancel next %s" heading))
      'cancel-next-p)
     (cons
      (format "Jump to next %s" heading)
      'jump-next-p))))

(cl-defmethod org-rl-clock-opts-next-with-time ((clock org-rl-clock))
  (let ((heading (org-rl-clock-heading clock)))
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


(defun org-rl-select-other-clock (&optional target)
  (interactive)
  (org-rl-debug :warning "org-rl-select-other-clock: target[%s]" target)
  (org-with-refile
      file loc (or target org-refile-targets)
    (let ((marker (make-marker)))
      (set-marker marker loc)
      marker)))

(cl-defmethod org-rl-get-time-gap ((prev org-rl-clock)
                                   (next org-rl-clock))
  (/
   (floor
    (float-time
     (time-subtract
      (org-rl-clock-start-time next)
      (or
       (org-rl-clock-stop-time prev)
       (if (org-rl-clock-null next)
           (org-rl-clock-stop-time next)
         (error "Can not get start time."))))))
   60))

(cl-defmethod org-rl-clock-time-debug-prompt ((prev org-rl-clock)
                                              (next org-rl-clock)
                                              &optional
                                              prompt stop)
  (let* ( ;;(base 120) ;; TODO: why it was 120 ?
         (base 61)
         (_debug (format "prev[%s %d %d] next[%s %d %d]"
                         ;; (org-rl-clock-marker prev)
                         (org-rl-clock-name-bracket prev)
                         (if (org-rl-clock-start-time prev) (% (/ (floor (float-time (org-rl-clock-start-time prev))) 60) base) 0)
                         (if (org-rl-clock-stop-time prev)  (% (/ (floor (float-time (org-rl-clock-stop-time prev))) 60) base) 0)
                         ;; (org-rl-clock-marker next)
                         (org-rl-clock-name-bracket next)
                         (if (org-rl-clock-start-time next) (% (/ (floor (float-time (org-rl-clock-start-time next))) 60) base) 0)
                         (if (org-rl-clock-stop-time next)  (% (/ (floor (float-time (org-rl-clock-stop-time next))) 60) base) 0)))
         (debug (if prompt (concat prompt " " _debug) _debug)))
    (when stop (read-from-minibuffer (format "%s test: " debug)))
    debug))


(cl-defmethod org-rl-clock-time-adv-debug-prompt ((prev org-rl-clock)
                                                  (next org-rl-clock)
                                                  &optional
                                                  prompt
                                                  stop)
  (let* ( ;;(base 120) ;; TODO: why it was 120 ?
         (base 61)
         (_debug (format "prev[%s %d %d] next[%s %d %d]"
                         ;; (org-rl-clock-marker prev)
                         (org-rl-format-clock prev)
                         (if (org-rl-clock-start-time prev) (% (/ (floor (float-time (org-rl-clock-start-time prev))) 60) base) 0)
                         (if (org-rl-clock-stop-time prev)  (% (/ (floor (float-time (org-rl-clock-stop-time prev))) 60) base) 0)
                         ;; (org-rl-clock-marker next)
                         (org-rl-format-clock next)
                         (if (org-rl-clock-start-time next) (% (/ (floor (float-time (org-rl-clock-start-time next))) 60) base) 0)
                         (if (org-rl-clock-stop-time next)  (% (/ (floor (float-time (org-rl-clock-stop-time next))) 60) base) 0)))
         (debug (if prompt (concat prompt " " _debug) _debug)))
    (when stop (read-from-minibuffer (format "%s test: " debug)))
    debug))

(cl-defmethod org-rl-clock-build-options ((prev org-rl-clock)
                                          (next org-rl-clock)
                                          maxtimelen)
  (org-rl-debug :warning "org-rl-clock-build-options: prev[%s] next[%s] maxtimelen[%d] secs"
                (org-rl-format-clock prev)
                (org-rl-format-clock next)
                maxtimelen)
  (append
   (when (markerp (org-rl-clock-marker prev))
     (append
      (org-rl-clock-opts-prev prev)
      (unless (zerop maxtimelen) (org-rl-clock-opts-prev-with-time prev))))
   (when (markerp (org-rl-clock-marker next))
     (append
      (org-rl-clock-opts-next next)
      (unless (zerop maxtimelen) (org-rl-clock-opts-next-with-time next))))
   (unless (zerop maxtimelen) (org-rl-clock-opts-common-with-time prev))
   (org-rl-clock-opts-common prev)))

(defun org-rl-clock-read-option (prompt options default)
  (cdr
   (assoc
    (completing-read prompt options)
    options)))

(defun org-rl-clock-read-timelen (prompt option maxtimelen)
  (progn
    (if (or (zerop maxtimelen)
            (memq option
                  '(done
                    cancel-next-p
                    cancel-prev-p)))
        maxtimelen
      (read-number prompt maxtimelen))))


(cl-defmethod org-rl-clock-opt-cancel-prev ((prev org-rl-clock)
                                            (next org-rl-clock))
  (setf (org-rl-clock-cancel prev) t)
  (org-rl-clock-clock-cancel prev)
  (setf (org-rl-clock-start prev) (org-rl-clock-start prev))
  (org-rl-clock-start-set prev nil)
  (org-rl-clocks-action nil nil prev next))

(cl-defmethod org-rl-clock-opt-cancel-next ((prev org-rl-clock)
                                            (next org-rl-clock))
  ;; cancel next clock
  ;; add next clock time
  (setf (org-rl-clock-cancel next) t)
  (org-rl-clock-clock-cancel next)
  (setf (org-rl-clock-start next) (org-rl-clock-stop prev))
  (org-rl-clock-stop-set  next nil)
  (org-rl-clocks-action nil nil prev next))

;; TODO:
;; still we need to think we are resolving time between two prev next clock or
;; we are resolving time of one clock
(cl-defmethod org-rl-clock-opt-include-in-prev ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen
                                                &optional
                                                resume)
  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-prev)
  (let ((maxtimelen (org-rl-get-time-gap prev next)))
    (if (or
         (= timelen maxtimelen)
         (eq timelen 'all))
        (progn
          (org-rl-clock-stop-set prev (time-add
                                       (org-rl-clock-start-time prev) maxtimelen) t)
          (org-rl-clock-clock-out prev resume))
      (let ((updated-time (time-add
                           (org-rl-clock-start-time prev) timelen)))
        (if (> (float-time timelen) 0)
            (progn
              (org-rl-clock-stop-set prev updated-time t)
              ;;time between updated prev and next will be resolve in next call of resolve
              (org-rl-clock-clock-out prev))
          (progn
            (setf (org-rl-clock-stop-dirty prev) t)
            (org-rl-clock-clock-out prev)
            (setq next
                  (org-rl-make-clock
                   (org-rl-clock-marker prev)
                   updated-time
                   (org-rl-clock-start-time next)
                   t
                   t))
            (org-rl-clock-clock-in-out next resume))))))
  (org-rl-clocks-action resume nil prev next))

(cl-defmethod org-rl-clock-opt-include-in-next ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen)
  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-next)

  (let ((maxtimelen (org-rl-get-time-gap prev next)))

    (when (and             ;clock out if prev is open clock and next is specifying idle time.
           (null (org-rl-clock-stop-time prev))
           (org-rl-clock-stop-time next))
      (org-rl-clock-clock-out
       (org-rl-make-clock (org-rl-clock-marker prev)
                          (org-rl-clock-start-time next)
                          (org-rl-clock-stop-time next))))
    ;; include timelen in next
    ;; update timelength
    (if (> (float-time timelen) 0)
        (let ((updated-start-time (time-add
                                   (org-rl-clock-start-time next) timelen)))
          (org-rl-clock-start-set next updated-start-time)
          (when (org-rl-clock-marker next)
            (org-rl-clock-clock-in next))) ;? imple

      (let ((updated-stop-time (time-add
                                (org-rl-clock-start-time prev) timelen)))
        (setq prev
              (org-rl-make-clock
                    (org-rl-clock-marker next)
                    (org-rl-clock-start-time prev)
                    updated-stop-time))
        ;; make prev is clocked out
        (when (org-rl-clock-marker prev)
          (org-rl-clock-clock-in-out prev)))))
  (org-rl-clocks-action nil nil prev next))

(cl-defmethod org-rl-clock-opt-include-in-other ((prev org-rl-clock)
                                                 (next org-rl-clock)
                                                 timelen
                                                 opt)
  ;; select other clock
  ;; include timelen in other
  ;; update timelength
  ;; (if debug-prompt (org-rl-clock-time-debug-prompt prev next t "include-in-other"))

  ;; TODO: check what sustract is doing here
  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-other)

  (let ((maxtimelen (org-rl-get-time-gap prev next)))

    (if (eq opt 'subtract)    ;is it correct ?
        (assert (< (float-time timelen) 0)))

    (let ((other-marker
           (if (eq opt 'include-in-other)
               (org-rl-select-other-clock)
             nil)))

      (if (> (float-time timelen) 0)

          (let* ((prev-stop-time (time-subtract
                                  (org-rl-clock-start-time next)
                                  (1+ timelen)))
                 (other-start-time
                  (time-subtract (org-rl-clock-start-time next) timelen))
                 (other-clock
                  (org-rl-make-clock other-marker other-start-time (org-rl-clock-start-time next))))

            (cl-assert (> (float-tome other-start-time) org-rl-min-clocking-time))

            (setq next
                  (if (org-rl-clock-null next)
                    (org-rl-clock-start-set next other-start-time)
                    other-clock))

            (org-rl-clock-stop-set prev prev-stop-time t)

            (org-rl-clock-clock-out prev)
            (when other-marker
              ;; TODO: see if this is working
              (org-rl-clock-clock-in-out other-clock)))

        ;; should 'substract always should not get negative time.
        (let* ((other-stop-time
                (time-subtract (org-rl-clock-stop-time prev) timelen))
               (other-clock
                (org-rl-make-clock other-marker (org-rl-clock-start-time next) other-stop-time)))

          (when (and         ;clockout prev clock
                 (null (org-rl-clock-stop-time prev))
                 (org-rl-clock-stop-time next))
            (org-rl-clock-stop-set prev (org-rl-clock-stop-time next) t)
            (org-rl-clock-clock-out prev t))

          (when other-marker
            (org-rl-clock-clock-in-out other-clock))
          (setq
            prev other-clock
            next other-clock)))))


  (org-rl-clocks-action nil nil prev next))


(cl-defmethod org-rl-clock-time-process-option ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                opt
                                                timelen
                                                maxtimelen
                                                &optional close-p)
  (let* ((timelen (seconds-to-time (* timelen 60)))
         (clocks
          (cond
           ((eq opt 'jump-prev-p)
            ;; finish here
            (org-rl-clock-clock-jump-to prev))

           ((eq opt 'jump-next-p)
            ;; finish here
            (org-rl-clock-clock-jump-to next))

           ((eq opt 'cancel-prev-p)
            (org-rl-clock-opt-cancel-prev prev next))
           ;; set org-clock-leftover-time here

           ((eq opt 'cancel-next-p)
            (org-rl-clock-opt-cancel-next prev next))

           ((eq opt 'include-in-prev)
            ;; include timelen in prev
            ;; update timelength
            (org-rl-clock-opt-include-in-prev prev next timelen))
           ;; set org-clock-leftover-time here
           ((eq opt 'include-in-next)
            (org-rl-clock-opt-include-in-next prev next timelen))

           ((memq opt '(include-in-other
                        subtract)) ;; subtract timelen from timelength
            (org-rl-clock-opt-include-in-other prev next timelen opt))

           ((eq opt 'done)
            (list prev next))

           (t (error "Wrong option %s" opt)))))

    clocks))

;; NOTE: Remember here the concept of Positive and Negative and Full time.
;; Read time which could be positive or negative or full
(cl-defmethod org-rl-clock-resolve-time ((prev org-rl-clock)
                                         (next org-rl-clock)
                                         &optional
                                         force
                                         close-p)
  "Read time which could be positive or negative or full"
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
          (maxtimelen (org-rl-get-time-gap prev next)))
      ;;;
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 10 5] with maxtimelen 5
      ;; Warning (org-rl-clock): You have selected opt subtract and timelen 9
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 5 5] with maxtimelen 0
      (org-rl-debug :warning "going to run %s with maxtimelen %d" (org-rl-clock-time-adv-debug-prompt prev next) maxtimelen)
      ;; (assert (> maxtimelen 0))
      (when (> maxtimelen 0)
        (let* ((options (org-rl-clock-build-options prev next maxtimelen))
               (opt (org-rl-clock-read-option
                     (if debug-prompt
                         (format "%s Select option [%d]: " (org-rl-clock-time-debug-prompt prev next) maxtimelen)
                       (format "Select option [%d]: " maxtimelen))
                     options maxtimelen))
               (timelen (org-rl-clock-read-timelen
                         (if debug-prompt
                             (format "%s [%s] how many minutes? [%d] " (org-rl-clock-time-debug-prompt prev next) opt maxtimelen)
                           (format "[%s] how many minutes? [%d] " opt maxtimelen))
                         opt
                         (org-rl-get-time-gap prev next))))
          ;; (barely-started-p (< (- (float-time last-valid)
          ;;                         (float-time (cdr clock))) 45))
          ;; (start-over-p (and subtractp barely-started-p))
          ;; cancel prev and add to time


          (org-rl-debug :warning "You have selected opt %s and timelen %d" opt timelen)
          (let ((maxtimelen (org-rl-get-time-gap prev next))) ;get maxtimelen time again
            (if (<= (abs timelen) maxtimelen)
                (let* ((clocks
                        (org-rl-clock-time-process-option prev next
                                                               opt timelen maxtimelen close-p))
                       (prev (nth 0 clocks))
                       (next (nth 1 clocks)))

                  (unless (eq opt 'done)
                    (when (and
                           (zerop maxtimelen)
                           close-p)
                      (org-clock-out))
                    (let ((timegap (org-rl-get-time-gap prev next)))
                      (when (> timegap 0)         ;this solved the assert
                        (org-rl-clock-time prev next close-p)))))
              (progn
                (message "Error given time %d can not be greater than %d" timelen maxtimelen)
                (org-rl-clock-time prev next force close-p)))))))))


;;; org-rl-clock.el ends here
