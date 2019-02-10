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
  (org-rl-debug :warning "cancelled prev, Can not find previous clock presently [todo]")
  (setf (org-rl-clock-start prev) (org-rl-clock-start prev))
  (setf (org-rl-clock-marker prev) nil)
  (org-rl-clocks-action nil nil prev next)
  (list prev next))

(cl-defmethod org-rl-clock-opt-cancel-next ((prev org-rl-clock)
                                            (next org-rl-clock))
  ;; what to do with prev.
  ;; cancel next clock
  ;; add next clock time
  (setf (org-rl-clock-cancel next) t)
  (org-rl-clock-clock-cancel next)
  ;;should 'now be used here? todo
  (setf (org-rl-clock-start next) (org-rl-clock-stop prev))
  (setf (org-rl-clock-stop  next) (org-rl-clock-stop prev))
  (setf (org-rl-clock-marker  next) nil)
  (org-rl-clocks-action nil nil prev next)
  (list prev next))

(cl-defmethod org-rl-clock-opt-include-in-prev ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen
                                                &optional
                                                resume
                                                fail-quietly)

  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-prev)

  (let ((maxtimelen (org-rl-get-time-gap prev next)))
    (if (= (org-rl-compare-time-gap prev next timelen) 0)
        (progn
          (setf (org-rl-clock-stop-time prev) (time-add
                                               (org-rl-clock-start-time prev) maxtimelen))
          (org-rl-clock-clock-out prev fail-quietly))
      (if (< (org-rl-compare-time-gap prev next timelen) 0)
          (if (> timelen 0)
              (org-rl-clock-expand-time prev (abs timelen))
            (progn
              (org-rl-clock-clock-out prev fail-quietly)     ;if already not necessary
              (org-rl-clock-clock-out next fail-quietly)     ;if necessary
              (setf next
                    (org-rl-make-clock (org-rl-clock-marker prev)
                                       (time-subtract
                                        (org-rl-clock-start-time next)
                                        (abs timelen))
                                       (org-rl-clock-stop-time next)))
              (org-rl-clock-clock-in-out next resume fail-quietly)))
        (error "timelen %d is greater than time difference %d between clocks" timelen maxtimelen))))
  (org-rl-clocks-action resume fail-quietly prev next)
  (list prev next))

(cl-defmethod org-rl-clock-opt-include-in-next ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen
                                                &optional
                                                resume
                                                fail-quietly)
  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-next)
  (let ((maxtimelen (org-rl-get-time-gap prev next)))
    (if (= (org-rl-compare-time-gap prev next timelen) 0)
        (progn
          (setf (org-rl-clock-stop-time next) (time-add
                                               (org-rl-clock-start-time next) maxtimelen))
          (org-rl-clock-clock-out next fail-quietly))
      (if (< (org-rl-compare-time-gap next prev timelen) 0)
          (if (< timelen 0)
              (org-rl-clock-expand-time next timelen)
            (progn
              (org-rl-clock-clock-out prev fail-quietly)     ;if necessary
              (org-rl-clock-clock-out next fail-quietly)     ;if necessary
              (setf prev (org-rl-make-clock (org-rl-clock-marker next)
                                            (org-rl-clock-stop-time prev)
                                            (time-add
                                             (org-rl-clock-stop-time prev)
                                             (abs timelen))))
              (setf prev (org-rl-clock-clock-in-out prev resume fail-quietly))))
        (error "timelen %d is greater than time difference %d between clocks" timelen maxtimelen))))
  (org-rl-clocks-action resume fail-quietly prev next)
  (list prev next))

(cl-defmethod org-rl-clock-opt-include-in-other ((prev org-rl-clock)
                                                 (next org-rl-clock)
                                                 timelen
                                                 &optional
                                                 resume
                                                 fail-quietly)
  ;; (if debug-prompt (org-rl-clock-time-debug-prompt prev next t "include-in-other"))

  (org-rl-debug :warning "begin %s" 'org-rl-clock-opt-include-in-other)

  (let ((maxtimelen   (org-rl-get-time-gap prev next))
        (other-marker (org-rl-select-other-clock)))

    (org-rl-clock-clock-out prev fail-quietly)     ;if necessary
    (org-rl-clock-clock-out next fail-quietly)     ;if necessary

    (if (> timelen 0)
        (setq prev
              (org-rl-clock-clock-in-out
               (org-rl-make-clock other-marker
                                  (org-rl-clock-stop-time prev)
                                  (time-add
                                   (org-rl-clock-stop-time prev)
                                   timelen))
               resume
               fail-quietly))
      (setq next
            (org-rl-clock-clock-in-out
             (org-rl-make-clock other-marker
                                (time-subtract
                                 (org-rl-clock-start-time next)
                                 (abs timelen))
                                (org-rl-clock-stop-time next))
             resume
             fail-quietly))))
  (org-rl-clocks-action nil nil prev next)
  (list prev next))


(cl-defmethod org-rl-clock-time-process-option ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                opt
                                                timelen
                                                maxtimelen
                                                &optional close-p)
  (let* ((timelen (* timelen 60))
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

           ((eq opt 'include-in-other) ;; subtract timelen from timelength
            (org-rl-clock-opt-include-in-other prev next timelen))

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
  (org-rl-debug :warning "org-rl-clock-resolve-time: begin")
  (lotus-with-override-minibuffer
    (org-rl-debug :warning "org-rl-clock-resolve-time: [body] lotus-with-override-minibuffer")
    (let ((debug-prompt t)
          (maxtimelen (org-rl-get-time-gap prev next)))
      ;;;
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 10 5] with maxtimelen 5
      ;; Warning (org-rl-clock): You have selected opt subtract and timelen 9
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 5 5] with maxtimelen 0
      (org-rl-debug :warning
                    "org-rl-clock-resolve-time: going to run %s with maxtimelen %d"
                    (org-rl-clock-time-adv-debug-prompt prev next) maxtimelen)
      ;; (assert (> maxtimelen 0))
      (when (> maxtimelen 0)
        (let* ((maxtimelen-mins-fn #'(lambda () (/ (org-rl-get-time-gap prev next) 60)))
               (options (org-rl-clock-build-options prev next maxtimelen))
               (opt (org-rl-clock-read-option
                     org-rl-read-interval
                     #'(lambda ()
                         (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                           (if debug-prompt
                               (format "%s Select option [%d]: " (org-rl-clock-time-debug-prompt prev next) maxtimelen-mins)
                             (format "Select option [%d]: " maxtimelen-mins))))
                     options
                     maxtimelen-mins-fn))
               (timelen
                (*
                 (org-rl-clock-read-timelen
                         org-rl-read-interval
                         #'(lambda ()
                             (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                               (if debug-prompt
                                   (format "%s [%s] how many minutes? [%d] " (org-rl-clock-time-debug-prompt prev next) opt maxtimelen-mins)
                                 (format "[%s] how many minutes? [%d] " opt maxtimelen-mins))))
                         opt
                         maxtimelen-mins-fn) 60)))
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
                        (if (and
                             (org-rl-clock-null prev)
                             (org-rl-clock-null next))
                            (org-rl-debug :warning "Test1 prev:%s next:%s"
                                          (org-rl-format-clock prev)
                                          (org-rl-format-clock next))
                          (progn
                            (if (eql (org-rl-clock-marker prev) nil)
                                (message "equal nil (org-rl-clock-marker prev)")
                              (message "not equal nil (org-rl-clock-marker prev)"))

                            (org-rl-debug :warning "Test2 prev:%s test %s marker %s next:%s test %s marker %s" 
                                          (org-rl-format-clock prev)
                                          (org-rl-clock-null prev)
                                          (org-rl-clock-marker prev)
                                          (org-rl-format-clock next)
                                          (org-rl-clock-null next)
                                          (org-rl-clock-marker next))
                           (org-rl-clock-resolve-time prev next close-p)))))))
              (message "Error given time %d can not be greater than %d" timelen maxtimelen)))))))
  (org-rl-debug :warning "org-rl-clock-resolve-time: finished"))

(when nil
 (org-rl-clock-build-options
  (org-rl-make-clock org-clock-marker 'now 'now)
  (org-rl-make-clock 'imaginary 'now 'now)
  10))

;;; org-rl-clock.el ends here
