;;; org-rl-obj-cps.el --- org rl obj cps             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d _at_ _Gmail_>
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

(provide 'org-rl-obj-cps)


(defun time-aware-completing-read (interval prompt-fn options-fn &optional default-fn)
  (unwind-protect
      (progn
        (select-frame-set-input-focus-no-raise-enable)
        (with-timeout (interval
                       (time-aware-completing-read interval prompt-fn options-fn default-fn))
          (let ((prompt  (if (functionp prompt-fn)  (funcall prompt-fn) prompt-fn))
                (options (if (functionp options-fn) (funcall options-fn) options-fn))
                (default (if (functionp default-fn) (funcall default-fn) default-fn)))
            (completing-read prompt options))))
    (select-frame-set-input-focus-no-raise-disable)))

(defun time-aware-read-number (interval prompt-fn default-fn)
  (unwind-protect
      (progn
        (select-frame-set-input-focus-no-raise-enable)
        (with-timeout (interval
                       (time-aware-read-number interval prompt-fn default-fn))
          (let ((prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn))
                (default (if (functionp default-fn) (funcall default-fn) default-fn)))
            (read-number prompt default))))
    (select-frame-set-input-focus-no-raise-disable)))


(defun org-rl-clock-cps-read-option (interval prompt-fn options-fn default-fn)
  (let ((options (if (functionp options-fn) (funcall options-fn) options-fn)))
    (cdr
     (assoc
      (time-aware-completing-read interval prompt-fn options-fn)
      options))))


(defun org-rl-clock-cps-read-option (interval prompt-fn options-fn default-fn)
  (let ((options (if (functionp options-fn) (funcall options-fn) options-fn)))
    (cdr
     (assoc
      (time-aware-completing-read interval prompt-fn options-fn)
      options))))


(cl-defmethod org-rl-clock-cps-resolve-time ((prev org-rl-clock)
                                             (next org-rl-clock)
                                             &optional
                                             resume
                                             fail-quietly
                                             resume-clocks)
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

  (org-rl-clock-assert prev)
  (org-rl-clock-assert next)

  (org-rl-debug nil "org-rl-clock-resolve-time: begin")
  (lotus-with-override-minibuffer-if
    (progn
      (org-rl-debug nil "org-rl-clock-resolve-time: [minibuffer-body] lotus-with-override-minibuffer-if active minibuffer found aborting it."))
    (org-rl-debug nil "org-rl-clock-resolve-time: [body] lotus-with-override-minibuffer-if")
    (let ((debug-prompt t)
          (maxtimelen (org-rl-get-time-gap prev next)))
      ;;;
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 10 5] with maxtimelen 5
      ;; Warning (org-rl-clock): You have selected opt subtract and timelen 9
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 5 5] with maxtimelen 0
      (org-rl-debug nil
                    "org-rl-clock-resolve-time: going to run %s with maxtimelen %d"
                    (org-rl-clock-time-adv-debug-prompt prev next) maxtimelen)
      ;; (assert (> maxtimelen 0))
      (when (> maxtimelen 0)
        (let* ((maxtimelen-mins-fn #'(lambda () (/ (org-rl-get-time-gap prev next) 60)))
               (options (org-rl-clock-build-options prev next maxtimelen))
               (opt
                (org-rl-clock-read-option
                 org-rl-read-interval
                 #'(lambda ()
                     (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                       (if debug-prompt
                           (format "%s Select option [%d]: " (org-rl-clock-time-debug-prompt prev next) maxtimelen-mins)
                         (format "Select option [%d]: " maxtimelen-mins))))
                 options
                 maxtimelen-mins-fn))
               (timelen
                (org-rl-clock-read-timelen
                 org-rl-read-interval
                 #'(lambda ()
                     (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                       (if debug-prompt
                           (format "%s [%s] how many minutes? [%d] " (org-rl-clock-time-debug-prompt prev next) opt maxtimelen-mins)
                         (format "[%s] how many minutes? [%d] " opt maxtimelen-mins))))
                 opt
                 maxtimelen-mins-fn)))
          ;; (barely-started-p (< (- (float-time last-valid)
          ;;                         (float-time (cdr clock))) 45))
          ;; (start-over-p (and subtractp barely-started-p))
          ;; cancel prev and add to time


          (org-rl-debug nil "You have selected opt %s and timelen %d" opt timelen)
          (let ((maxtimelen (org-rl-get-time-gap prev next))) ;get maxtimelen time again
            (if (<=
                 (abs timelen)
                 maxtimelen)
                (let* ((clocks
                        (org-rl-clock-time-process-option prev next
                                                          opt timelen
                                                          maxtimelen
                                                          resume
                                                          fail-quietly
                                                          resume-clocks))
                       (resolve-clocks (nth 1 clocks))
                       (resume-clocks (nth 2 clocks))
                       (prev (nth 0 resolve-clocks))
                       (next (nth 1 resolve-clocks)))
                  (org-rl-debug nil "(org-rl-clock-null prev) %s" (org-rl-clock-null prev))
                  (org-rl-debug nil "(org-rl-clock-null next) %s" (org-rl-clock-null next))
                  (if (and
                       resolve-clocks
                       (not
                        (and
                         (org-rl-clock-null prev)
                         (org-rl-clock-null next)))
                       (> (org-rl-get-time-gap prev next) 0))
                      (org-rl-clock-resolve-time prev next resume fail-quietly resume-clocks)
                    (if resume-clocks
                        (org-rl-clock-resume-clock resume-clocks))
                    (org-rl-debug nil "Error1")))
              (org-rl-debug nil "Error given time %d can not be greater than %d" timelen maxtimelen)))))))
  (org-rl-debug nil "org-rl-clock-resolve-time: finished"))


;;; org-rl-obj-cps.el ends here
