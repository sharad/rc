;;; org-rl-obj.el --- org resolve clock basic objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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

(provide 'org-rl-obj)


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


(defvar org-rl-org-clock-persist nil "Control org-clock-persist at time of org-resolve clock-in")
(defvar org-rl-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of org-resolev clock-in")

(defun org-rl-straight-org-clock-clock-in (clock &optional resume start-time)
  (progn
    (lotus-org-clock-load-only)
    (let ((org-clock-persist               org-rl-org-clock-persist)
          (org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))
      (org-clock-clock-in clock resume start-time))))


(defun time-aware-completing-read (interval prompt-fn options-fn default-fn)
  (interactive)
  (let* ((currtime-secs (time-to-seconds (current-time)))
         (prompt (funcall prompt-fn)))
    (with-timeout (interval
                   (time-aware-completing-read interval prompt-fn options default-fn))
      (let ((prompt (if (fboundp prompt-fn) (funcall prompt-fn)))
            (options (if (fboundp options-fn) (funcall options-fn)))
            (default (if (fboundp default-fn) (funcall default-fn))))
        (completing-read prompt options default)))))


(defun time-aware-read-number (interval prompt-fn default-fn)
  (interactive)
  (let* ((currtime-secs (time-to-seconds (current-time)))
         (prompt (funcall prompt-fn)))
    (with-timeout (interval
                   (time-aware-read-number interval prompt-fn default-fn))
      (let ((prompt (if (fboundp prompt-fn) (funcall prompt-fn)))
            (default (if (fboundp default-fn) (funcall default-fn))))
        (read-number prompt default)))))


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

(cl-defmethod org-rl-make-clock ((marker marker)
                                 (start org-rl-time)
                                 (stop org-rl-time)
                                 &optional
                                 active
                                 cancel)
  ;; (org-rl-debug :warning "calling 1")
  (make-org-rl-clock
   :marker marker
   :start start
   :stop  stop
   :active active
   :cancel cancel))

;; (cl-defgeneric org-rl-make-clock ((marker marker)
;;                                   start-time
;;                                   stop-time
;;                                   &optional
;;                                   start-dirty
;;                                   stop-dirty
;;                                   active
;;                                   cancel))

(cl-defmethod org-rl-make-clock ((marker marker)
                                 start-time
                                 stop-time
                                 &optional
                                 start-dirty
                                 stop-dirty
                                 active
                                 cancel)
  ;; (org-rl-debug :warning "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time :dirty start-dirty)
   :stop  (make-org-rl-time :time stop-time  :dirty stop-dirty)
   :active active
   :cancel cancel))

(cl-defmethod org-rl-make-clock ((marker symbol)
                                 start-time
                                 stop-time
                                 &optional
                                 start-dirty
                                 stop-dirty
                                 active
                                 cancel)
  ;; (org-rl-debug :warning "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time :dirty start-dirty)
   :stop  (make-org-rl-time :time stop-time  :dirty stop-dirty)
   :active active
   :cancel cancel))

;; good
;; (org-rl-make-clock (point-marker) (current-time) 1)
;; (org-rl-make-clock (point-marker) (make-org-rl-time :time (current-time)) (make-org-rl-time :time (current-time)))



(defun org-rl-make-time (time &optional dirty)
  (make-org-rl-time :time time :dirty dirty))

(defun org-rl-make-current-time (&optional dirty)
  (make-org-rl-time :time 'now :dirty dirty))


(defun time-get-time (time)
  (when time
    (if (time-p time)
        (if (eq time 'now)
            (current-time)
          time)
      (error "Wring time %s passed." time))))

(cl-defmethod org-rl-format (time)
  (let ((fmt (cdr org-time-stamp-formats)))
    (format-time-string fmt (time-get-time time))))

(cl-defmethod org-rl-format ((time org-rl-time))
  (let ((fmt (cdr org-time-stamp-formats)))
    (format-time-string fmt (org-rl-time-get-time time))))

(cl-defmethod org-rl-time-get-time ((time org-rl-time))
  (let ((rl-time (org-rl-time-time time)))
    (time-get-time rl-time)))

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
  (org-rl-time-dirty (org-rl-clock-start clock)))
(cl-defmethod (setf org-rl-clock-start-dirty) (dirty (clock org-rl-clock))
  (setf (org-rl-time-dirty (org-rl-clock-start clock)) dirty))
(cl-defmethod org-rl-clock-stop-dirty ((clock org-rl-clock))
  (org-rl-time-dirty (org-rl-clock-stop clock)))
(cl-defmethod (setf org-rl-clock-stop-dirty) (dirty (clock org-rl-clock))
  (setf (org-rl-time-dirty (org-rl-clock-stop clock)) dirty))

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



(cl-defmethod org-rl-clock-half-p ((clock org-rl-clock))
  (save-excursion
    (let ((marker (org-rl-clock-marker clock)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (let ((clock-reg
                (concat "^ *CLOCK: *\\[" org-ts-regexp0 "\\]$"))
              (beginning (line-beginning-position))
              (end (line-end-position)))
          (when (move-beginning-of-line nil)
            (re-search-forward clock-reg end t)))))))

(cl-defmethod org-rl-clock-full-p ((clock org-rl-clock))
  (save-excursion
    (let ((marker (org-rl-clock-marker clock)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (let ((clock-reg
               (concat " *CLOCK: *\\["
                       org-ts-regexp0 "\\]\\(?:--\\[\\)?" org-ts-regexp0))
              (beginning (line-beginning-position))
              (end (line-end-position)))
          (move-beginning-of-line nil)
          (re-search-forward clock-reg end t))))))

(cl-defmethod org-rl-clock-insert-range ((clock org-rl-clock))
  (let ((fmt (cdr org-time-stamp-formats)))
    (let ((start (format-time-string fmt (org-rl-clock-start-time clock)))
          (stop  (format-time-string fmt (org-rl-clock-stop-time clock))))
      (org-insert-time-stamp start t t "CLOCK: ")
      (insert "--")
      (org-insert-time-stamp stop t t)
      (org-update-all-dblocks))))

(cl-defmethod org-rl-clock-replace ((clock org-rl-clock) &optional terminal)
  (if (org-rl-clock-marker clock)
    (if (org-rl-clock-full-p clock)
        (save-excursion
          (let ((marker (org-rl-clock-marker clock)))
            (with-current-buffer (marker-buffer marker)
              (let ((clock-reg
                     (concat " *CLOCK: *\\["
                             "\\(" org-ts-regexp0 "\\)"
                             "\\]\\(?:--\\[\\)?"
                             "\\(" org-ts-regexp0 "\\)"
                             "\\(?:\\] *=> *\\([0-9]+:[0-9]\\{2\\}\\)\\)"))
                    (beginning (line-beginning-position))
                    (end (line-end-position)))
               (when (and
                      (goto-char marker)
                      (move-beginning-of-line nil))
                 (when (re-search-forward clock-reg end t)
                   (let ((file-clock-start (org-time-string-to-time (match-string 1)))
                         (file-clock-stop  (org-time-string-to-time (match-string 2))))
                     (cond
                      ((eq terminal 'start)
                       (if (= file-clock-stop (org-rl-clock-stop-time clock))
                           (progn
                            (kill-line)
                            (org-rl-clock-insert-range clock))))
                      ((eq terminal 'stop)
                       (if (= file-clock-start (org-rl-clock-start-time clock))
                           (progn
                             (kill-line)
                             (org-rl-clock-insert-range clock))))
                      (t
                       (kill-line)
                       (org-rl-clock-insert-range clock))))))))))
      (error "clock %s is not full" (org-rl-clock-name-bracket clock)))
    (message "%s clock is null" (org-rl-clock-marker clock))))


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
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-marker clock)
        (if (time-p (org-rl-clock-start-time clock))
            (org-rl-straight-org-clock-clock-in
             (org-rl-clock-for-clock-in clock)
             resume
             (org-rl-clock-start-time clock))
          (error "%s start time is null" (org-rl-clock-start-time clock)))
      (error "%s clock is null" (org-rl-clock-marker clock)))))

(cl-defmethod org-rl-clock-clock-out ((clock org-rl-clock)
                                      &optional
                                      fail-quietly)
  (org-rl-debug :warning "org-rl-clock-clock-out: clock[%s] fail-quietly[%s]"
                (org-rl-format-clock clock)
                fail-quietly)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-marker clock)
        (if (time-p (org-rl-clock-stop-time clock))
            (if (org-rl-clock-half-p clock)
                (org-clock-clock-out (org-rl-clock-for-clock-out clock)
                  fail-quietly
                  (org-rl-clock-stop-time clock))
               (org-rl-clock-replace clock))
          (error "%s stop time is null" (org-rl-clock-stop-time clock)))
      (message "%s clock is null" (org-rl-clock-marker clock)))))

(cl-defmethod org-rl-clock-clock-in-out ((clock org-rl-clock)
                                         &optional
                                         resume
                                         fail-quietly)
  (org-rl-debug :warning "org-rl-clock-clock-in-out: clock[%s] resume[%s] org-clock-clocking-in[%s]"
                (org-rl-format-clock clock)
                resume
                org-clock-clocking-in)
  (let ((org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))
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


;; https://github.com/dfeich/org-clock-convenience/blob/master/org-clock-convenience.el
;; https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly

(cl-defmethod org-rl-clock-expand-time ((clock org-rl-clock) sec)
  "if sec is positive expand in future else expand in past."
  ;; do clock in clock out accordingly
  (if (> sec 0)
      (progn
       (setf (org-rl-clock-stop-time clock) (time-add (org-rl-clock-stop-time clock) sec))
       (org-rl-clock-clock-out clock))
    (progn
      (setf (org-rl-clock-start-time clock) (time-subtract (org-rl-clock-stop-time clock) sec))
      (org-rl-clock-replace clock))))

(cl-defmethod org-rl-clock-contract-time ((clock org-rl-clock) sec)
  "if sec is positive contract from future else contract from past.")


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
     ;; (cons
     ;;  (format "subtract from prev %s" heading)
     ;;  'subtract)
     )))

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

(cl-defmethod org-rl-get-time-gap ((prev org-rl-clock)
                                   (next org-rl-clock))
  (/
   (floor
    (float-time
     (time-subtract
      (org-rl-clock-start-time next)
      (org-rl-clock-stop-time prev))))
   60))

(cl-defmethod org-rl-compare-time-gap ((prev org-rl-clock)
                                       (next org-rl-clock)
                                       timelen)
  (org-rl-debug :warning "timelen %s" timelen)
  (if (eq timelen 'all)
      0
    (- timelen (float-time (org-rl-get-time-gap prev next)))))


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

(defvar org-rl-read-interval 60)

(defun org-rl-clock-read-option (interval prompt-fn options-fn default-fn)
  (cdr
   (assoc
    (time-aware-completing-read interval prompt-fn options-fn default-fn)
    options)))

(defun org-rl-clock-read-timelen (interval prompt-fn option-fn maxtimelen-fn)
  (let ((option (if (fboundp option-fn) (funcall option-fn) option-fn)))
    (if (or (zerop maxtimelen-fn)
            (memq option
                  '(done
                    cancel-next-p
                    cancel-prev-p)))
        maxtimelen-fn
      (time-aware-read-number interval prompt-fn maxtimelen-fn))))

;;; org-rl-obj.el ends here
