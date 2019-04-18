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


(require 'org-rl-obj)
(require 'org-rl-clock)


(defun org-rl-clock-cps-process-option (timelen opt prev next maxtime resume fail-quietly resume-clocks)
  (org-rl-debug :warning "started org-rl-clock-cps-process-option selected opt=%s" opt)
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
               (resolve-clocks (nth 0 clocks))
               (resume-clocks  (nth 1 clocks))
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
              (org-rl-clock-cps-resolve-time prev next resume fail-quietly resume-clocks)
            (if resume
                (org-rl-clock-resume-clock resume-clocks))
            (if resolve-clocks
                (org-rl-debug :warning "Done prev[%s] next[%s] gap[%d]"
                              prev next (org-rl-get-time-gap prev next))
              (org-rl-debug :warning "Done no clock to resolve"))))
      (org-rl-debug nil "Error given time %d can not be greater than %d" timelen maxtimelen))))

(defun org-rl-clock-cps-process-helm-option (option)
  (org-rl-debug :warning "started org-rl-clock-cps-process-helm-option opt: %s" option)
  (let* ((debug-prompt t)
         (opt           (nth 0 option))
         (prev          (nth 1 option))
         (next          (nth 2 option))
         (maxtimelen-mins-fn #'(lambda () (/ (org-rl-get-time-gap prev next) 60)))
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
    (org-rl-debug :warning "in org-rl-clock-cps-process-helm-option opt[ %s ]" opt)
    (apply #'org-rl-clock-cps-process-option timelen option)))

(defun org-rl-helm-build-options (interval prompt-fn options-fn default-fn)
  (let ((name (if (functionp prompt-fn)
                  (funcall prompt-fn)
                prompt-fn)))
   (helm-build-sync-source name
    :candidates (if (functionp options-fn)
                    (funcall options-fn)
                  options-fn)
    :action (list
             (cons "Select" #'org-rl-clock-cps-process-helm-option))
    :action-transformer #'(lambda (actions candidate)
                            (list (cons "select" #'org-rl-clock-cps-process-helm-option))))))

(defun org-rl-clock-cps-read-option (interval prompt-fn options-fn default-fn)
  (let ((options (if (functionp options-fn) (funcall options-fn) options-fn))
        (prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn)))
    (message "%s" prompt)
    (let (helm-sources)
      (push
       (org-rl-helm-build-options interval prompt-fn options 1)
       helm-sources)
      (push (helm-build-dummy-source "Create tsk"
              :action (helm-make-actions
                       "Create tsk"
                       'sacha/helm-org-create-tsk))
            helm-sources)
      (helm helm-sources))))


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

  (org-rl-debug nil "org-rl-clock-cps-resolve-time: begin")
  (lotus-with-override-minibuffer-if
      (progn
        (org-rl-debug nil "org-rl-clock-cps-resolve-time: [minibuffer-body] lotus-with-override-minibuffer-if active minibuffer found aborting it."))
    (org-rl-debug nil "org-rl-clock-cps-resolve-time: [body] lotus-with-override-minibuffer-if")
    (let ((debug-prompt t)
          (maxtimelen (org-rl-get-time-gap prev next)))
      (org-rl-debug nil
                    "org-rl-clock-cps-resolve-time: going to run %s with maxtimelen %d"
                    (org-rl-clock-time-adv-debug-prompt prev next) maxtimelen)
      (when (> maxtimelen 0)
        (let* ((maxtimelen-mins-fn #'(lambda () (/ (org-rl-get-time-gap prev next) 60)))
               (options (org-rl-clock-build-options
                         prev next
                         maxtimelen
                         resume
                         fail-quietly
                         resume-clocks))
               (ret (message "org-rl-clock-cps-resolve-time: options %s" options))
               (opt
                (org-rl-clock-cps-read-option
                 org-rl-read-interval
                 #'(lambda ()
                     (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                       (if debug-prompt
                           (format "%s Select option [%d]: " (org-rl-clock-time-debug-prompt prev next) maxtimelen-mins)
                         (format "Select option [%d]: " maxtimelen-mins))))
                 options
                 maxtimelen-mins-fn)))))))

          ;; (barely-started-p (< (- (float-time last-valid)
          ;;                         (float-time (cdr clock))) 45))
          ;; (start-over-p (and subtractp barely-started-p))
          ;; cancel prev and add to time


          ;; (org-rl-debug nil "You have selected opt %s and timelen %d" opt timelen)


  (org-rl-debug nil "org-rl-clock-cps-resolve-time: finished"))

;;; org-rl-obj-cps.el ends here
