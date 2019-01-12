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


(provide 'org-clock-resolve-advanced)

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

(setq
 org-resolve-clock-opts-common
 '(("Done" . done)))

(defun org-resolve-clock-opts-common (clock)
  (list (cons "Done" 'done)))

(defun org-resolve-clock-opts-common-with-time (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons "Include in other" 'include-in-other)
     (cons
      (format "subtract from prev %s" heading)
      'subtract))))

(defun org-resolve-clock-opts-prev (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons
      (format "Cancel prev %s" heading)
      'cancel-prev-p)
     (cons
      (format "Jump to prev %s" heading)
      'jump-prev-p))))

(defun org-resolve-clock-opts-prev-with-time (clock)
  (let ((heading (org-get-heading-from-clock clock)))
    (list
     (cons
      (format "Include in prev %s" heading)
      'include-in-prev))))

(defun org-resolve-clock-opts-next (clock)
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

(defun org-resolve-clock-opts-next-with-time (clock)
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

(defun org-rl-clock-name-bracket (clock)
  ;;(org-rl-clock-marker clock)
  (concat "<" (org-get-heading-from-clock clock) ">"))


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

(defun org-resolve-clock-time-debug-prompt (prev next &optional prompt stop)
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

(defun org-resolve-clock-build-options (prev next maxtimelen)
  (append
   (when (markerp (org-rl-clock-marker prev))
     (append
      (org-resolve-clock-opts-prev prev)
      (unless (zerop maxtimelen) (org-resolve-clock-opts-prev-with-time prev))))
   (when (markerp (org-rl-clock-marker next))
     (append
      (org-resolve-clock-opts-next next)
      (unless (zerop maxtimelen) (org-resolve-clock-opts-next-with-time next))))
   (unless (zerop maxtimelen) (org-resolve-clock-opts-common-with-time prev))
   (org-resolve-clock-opts-common prev)))

(defun org-resolve-clock-read-option (prompt options default)
  (cdr
   (assoc
    (completing-read prompt options)
    options)))

(defun org-resolve-clock-read-timelen (prompt option maxtimelen)
  (progn
    (if (or (zerop maxtimelen)
            (memq option
                  '(done
                    cancel-next-p
                    cancel-prev-p)))
        maxtimelen
      (read-number prompt maxtimelen))))


(defun org-resolve-clock-opt-cancel-prev (prev next)
  (progn
    (org-rl-clock-clock-cancel prev)
    (let ((prev-start (cdr prev)))
      (setq prev
            (list
             nil
             nil
             (org-rl-clock-start-time prev))))))
(defun org-resolve-clock-opt-cancel-next (prev next)
  ;; cancel next clock
  ;; add next clock time
  (progn
    (org-rl-clock-clock-cancel next)
    (setq next
          (list
           nil
           (org-rl-clock-stop-time prev)
           nil))))
(defun org-resolve-clock-opt-include-in-prev (prev next timelen)
  (org-rl-debug :warning "begin %s" 'org-resolve-clock-opt-include-in-prev)
  ;; include timelen in prev
  ;; update timelength
  (let ((updated-time (time-add
                       (org-rl-clock-start-time prev) timelen)))
    (if (> (float-time timelen) 0)
        (progn
          (org-rl-clock-stop-time-set prev updated-time)
          (org-rl-clock-clock-out prev))
      (progn
        (org-rl-clock-clock-out prev)
        (setq next (list
                    (org-rl-clock-marker prev) updated-time (org-rl-clock-start-time next)))
        (org-rl-clock-clock-in-out next)))))
(defun org-resolve-clock-opt-include-in-next (prev next timelen)
  (org-rl-debug :warning "begin %s" 'org-resolve-clock-opt-include-in-next)

  (when (and             ;clock out if prev is open clock and next is specifying idle time.
         (null (org-rl-clock-stop-time prev))
         (org-rl-clock-stop-time next))
    (org-rl-clock-clock-out
     (make-rl-clock (org-rl-clock-marker prev)
                    (org-rl-clock-start-time next)
                    (org-rl-clock-stop-time next))))
  ;; include timelen in next
  ;; update timelength
  (if (> (float-time timelen) 0)
      (let ((updated-start-time (time-add
                                 (org-rl-clock-start-time next) timelen)))
        (org-rl-clock-start-time-set next updated-start-time)
        (when (org-rl-clock-marker next)
          (org-rl-clock-clock-in next))) ;? imple

    (let ((updated-stop-time (time-add
                              (org-rl-clock-start-time prev) timelen)))
      (setq prev (list
                  (org-rl-clock-marker next)
                  (org-rl-clock-start-time prev)
                  updated-stop-time))
      ;; make prev is clocked out
      (when (org-rl-clock-marker prev)
        (org-rl-clock-clock-in-out prev)))))
(defun org-resolve-clock-opt-include-in-other (prev next timelen opt)
  ;; select other clock
  ;; include timelen in other
  ;; update timelength
  ;; (if debug-prompt (org-resolve-clock-time-debug-prompt prev next t "include-in-other"))

  ;; TODO: check what sustract is doing here

  (org-rl-debug :warning "begin %s" 'org-resolve-clock-opt-include-in-other)

  (if (eq opt 'subtract)    ;is it correct ?
      (assert (< (float-time timelen) 0)))

  (let ((other-marker
         (if (eq opt 'include-in-other)
             (org-rl-select-other-clock)
           nil)))

    (if (> (float-time timelen) 0)
        (let* ((other-start-time (time-subtract
                                  (org-rl-clock-start-time next)
                                  timelen))
               (other-clock
                (make-rl-clock other-marker other-start-time (org-rl-clock-start-time next))))

          (setq next
                (if (eq (org-rl-clock-marker next) 'imaginary)
                    (org-rl-clock-start-time-set next other-start-time)
                  other-clock))
          (when other-marker
            ;; TODO: see if this is working
            (org-rl-clock-clock-in-out other-clock)))

      ;; should 'substract always should not get negative time.
      (let* ((other-stop-time
              (time-subtract (org-rl-clock-stop-time prev) timelen))
             (other-clock
              (make-rl-clock other-marker (org-rl-clock-start-time next) other-stop-time)))

        (when (and         ;clockout prev clock
               (null (org-rl-clock-stop-time prev))
               (org-rl-clock-stop-time next))
          (org-rl-clock-stop-time-set prev (org-rl-clock-stop-time next))
          (org-rl-clock-clock-out prev t))

        (setq prev other-clock)
        (when other-marker (org-rl-clock-clock-in-out other-clock))))))


(defun org-resolve-clock-time-process-option (next prev opt timelen maxtimelen &optional close-p)
  (let ((timelen (seconds-to-time (* timelen 60))))
    (cond
     ((eq opt 'jump-prev-p)
      ;; finish here
      (org-rl-clock-clock-jump-to prev))

     ((eq opt 'jump-next-p)
      ;; finish here
      (org-rl-clock-clock-jump-to next))

     ((eq opt 'cancel-prev-p)
      (org-resolve-clock-opt-cancel-prev prev next))
     ;; set org-clock-leftover-time here

     ((eq opt 'cancel-next-p)
      (org-resolve-clock-opt-cancel-next prev next))

     ((eq opt 'include-in-prev)
      ;; include timelen in prev
      ;; update timelength
      (org-resolve-clock-opt-include-in-prev prev next timelen))
     ;; set org-clock-leftover-time here
     ((eq opt 'include-in-next)
      (org-resolve-clock-opt-include-in-next prev next timelen))

     ((memq opt '(include-in-other
                  subtract)) ;; subtract timelen from timelength
      (org-resolve-clock-opt-include-in-other prev next timelen opt))

     ((eq opt 'done))
     (t (error "Error"))))

  (unless (eq opt 'done)
    (when (and (zerop maxtimelen) close-p)
      (org-clock-out))
    (let ((timegap (org-rl-get-time-gap prev next)))
      (when (> timegap 0)         ;this solved the assert
        (org-resolve-clock-time prev next close-p)))))

;; NOTE: Remember here the concept of Positive and Negative and Full time.
;; Read time which could be positive or negative or full
(defun org-resolve-clock-time (prev next &optional force close-p)
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
          (default (org-rl-get-time-gap prev next)))
      ;;;
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 10 5] with default 5
      ;; Warning (org-rl-clock): You have selected opt subtract and timelen 9
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 5 5] with default 0
      (org-rl-debug :warning "going to run %s with default %d" (org-resolve-clock-time-debug-prompt prev next) default)
      ;; (assert (> default 0))
      (when (> default 0)
        (let* ((options (org-resolve-clock-build-options prev next default))
               (opt (org-resolve-clock-read-option
                     (if debug-prompt
                         (format "%s Select option [%d]: " (org-resolve-clock-time-debug-prompt prev next) default)
                       (format "Select option [%d]: " default))
                     options default))
               (timelen (org-resolve-clock-read-timelen
                         (if debug-prompt
                             (format "%s [%s] how many minutes? [%d] " (org-resolve-clock-time-debug-prompt prev next) opt default)
                           (format "[%s] how many minutes? [%d] " opt default))
                         opt
                         (org-rl-get-time-gap prev next))))
          ;; (barely-started-p (< (- (float-time last-valid)
          ;;                         (float-time (cdr clock))) 45))
          ;; (start-over-p (and subtractp barely-started-p))
          ;; cancel prev and add to time
          (org-rl-debug :warning "You have selected opt %s and timelen %d" opt timelen)
          (let ((default (org-rl-get-time-gap prev next))) ;get default time again
            (if (> (abs timelen) default)
                (progn
                  (message "Error given time %d can not be greater than %d" timelen default)
                  (org-resolve-clock-time prev next force close-p))
              (org-resolve-clock-time-process-option next prev opt timelen default close-p))))))))

;;;###autoload
(defvar org-clock-last-user-idle-seconds nil)

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
            (org-resolve-clock-time
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
                (org-resolve-clock-time
                 (make-rl-clock (car clock) (cdr clock) nil)
                 (make-rl-clock 'imaginary 'now (cdr clock)))))))))))

;;;###autoload
(defalias 'org-resolve-clocks 'org-rl-resolve-clocks)

;;;###autoload
(defun org-clock-resolve-clocks (clocks) ;TODO
  (let ((next (pop clocks))
        (prev (pop clocks)))
    (org-resolve-clock-time next prev)))

;;;###autoload
(defun org-clock-resolve-advanced-insinuate ()
  (interactive)
  (defalias 'org-resolve-clocks-if-idle 'org-rl-resolve-clocks-if-idle)
  (add-hook 'org-clock-in-hook
            #'org-rl-clock-set-correct-idle-timer)
  (defalias 'org-resolve-clocks 'org-rl-resolve-clocks))

;;;###autoload
(defun org-clock-resolve-advanced-uninsinuate ()
  (remove-hook 'org-clock-in-hook
               #'org-rl-clock-set-correct-idle-timer))



(defun org-find-open-clocks (file)
  "Search through the given file and find all open clocks."
  (let ((buf (or (get-file-buffer file)
                 (find-file-noselect file)))
        (org-clock-re (concat org-clock-string " \\(\\[.*?\\]\\)$"))
        clocks)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-clock-re nil t)
          (push (cons (copy-marker (match-end 1) t)
                      (org-time-string-to-time (match-string 1))) clocks))))
    clocks))




(defun org-rl-first-clock-started-mins (marker)
  (let* ((clock-time (org-clock-get-nth-half-clock-time marker 1))
         (mins-spent
          (when clock-time
            (/
             (float-time
              (time-subtract
               (current-time)
               (cdr (org-clock-get-nth-half-clock-time marker 1))))
             60))))
    mins-spent))

;;;###autoload
(defun test-org-rl-resolve-clock-force (idle-sec)
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (interactive
   (let* ((marker org-clock-marker)
          (mins-spent
           (or
            (org-rl-first-clock-started-mins marker)
            0)))
     (list (*
            (read-number
             (format "clock[ %s ] Spent mins: " (org-get-heading-from-clock (list marker)))
             (org-rl-first-clock-started-mins marker))
            60))))
  (let* ((marker org-clock-marker)
         (mins-spent
          (or
           (org-rl-first-clock-started-mins marker)
           0)))
    (if (> mins-spent 1)
        (if (< 1 (/ idle-sec 60) (1- mins-spent))
            (when (and
                   org-clock-idle-time
                   (not org-clock-resolving-clocks)
                   marker
                   (marker-buffer marker))
              (let* ((org-clock-user-idle-seconds idle-sec)
                     (org-clock-user-idle-start
                      (time-subtract (current-time)
                                     (seconds-to-time org-clock-user-idle-seconds)))
                     (org-clock-resolving-clocks-due-to-idleness t))
                (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
                    (org-resolve-clock-time
                     (make-rl-clock marker org-clock-start-time nil)
                     (make-rl-clock 'imaginary 'now org-clock-user-idle-start))
                  (when t
                        (message "Idle time now min[%d] sec[%d]"
                                 (/ org-clock-user-idle-seconds 60)
                                 (% org-clock-user-idle-seconds 60)
                                 )))))
          (org-rl-debug :warning "Selected min[ = %d ] is more than mins-spent[ = %d ]" (/ idle-sec 60) mins-spent))
      (org-rl-debug :warning "Not one min is spent with clock mins-spent = %d" mins-spent))))

;;; org-clock-utils-lotus.el ends here
