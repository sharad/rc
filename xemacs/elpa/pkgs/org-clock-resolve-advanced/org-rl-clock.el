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


(require 'org-rl-obj)


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
    (if (= (org-rl-compare-time-gap prev next timelen) 0)
        (progn
          (org-rl-clock-stop-set prev (time-add
                                       (org-rl-clock-start-time prev) maxtimelen) t)
          (org-rl-clock-clock-out prev resume))
      (if (< (org-rl-compare-time-gap prev next timelen) 0)
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
              (org-rl-clock-clock-in-out next resume))))
        (error "timelen %d is greater than time difference %d between clocks" timelen maxtimelen))))
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
