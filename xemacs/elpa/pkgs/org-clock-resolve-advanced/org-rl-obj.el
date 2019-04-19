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
      (org-clock-clock-in
       (org-rl-clock-for-clock-in clock)
       resume start-time)
      (setf (org-rl-clock-marker clock) org-clock-marker)
      (setf (org-rl-clock-current clock) t)
      clock)))


(defun org-rl-debug (level &rest args)
  (let* ((level (or level :debug))
         (ts (time-stamp-string))
         (fmt (format "%s: %s" ts (car args)))
         (args (append (list fmt) (cdr args))))
    ;; (apply #'lwarn 'org-rl-clock level args)
    (message
     (concat
      (format "org-rl-clock %s: " level)
      (apply #'format args)))))


(defun time-aware-completing-read (interval prompt-fn options-fn &optional default-fn)
  (with-select-frame-set-input-disable-raise
    (with-timeout (interval
                   (time-aware-completing-read interval prompt-fn options-fn default-fn))
      (let ((prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn))
            (options (if (functionp options-fn) (funcall options-fn) options-fn))
            (default (if (functionp default-fn) (funcall default-fn) default-fn)))
        (completing-read prompt options)))))

(defun time-aware-read-number (interval prompt-fn default-fn)
  (with-select-frame-set-input-disable-raise
    (with-timeout (interval
                   (time-aware-read-number interval prompt-fn default-fn))
      (let ((prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn))
            (default (if (functionp default-fn) (funcall default-fn) default-fn)))
        (read-number prompt default)))))


(defun time-p (time)
  (or
   (eq 'now time)
   (and
    (consp time)
    (nth 1 time))))

(defun time-eq (time1 time2)
  (< (abs (time-to-seconds (time-subtract time1 time2))) 60))


(cl-defstruct org-rl-time
  time
  clean)

(cl-defstruct org-rl-clock
  marker
  start
  stop
  current
  cancel)

(cl-defmethod org-rl-make-clock ((marker marker)
                                 (start org-rl-time)
                                 (stop org-rl-time)
                                 &optional
                                 current
                                 cancel)
  (make-org-rl-clock
   :marker marker
   :start start
   :stop  stop
   :current current
   :cancel cancel))

(cl-defmethod org-rl-make-clock ((marker marker)
                                 start-time
                                 stop-time
                                 &optional
                                 current
                                 start-clean
                                 stop-clean
                                 cancel)
  ;; (org-rl-debug nil "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time :clean start-clean)
   :stop  (make-org-rl-time :time stop-time  :clean stop-clean)
   :current current
   :cancel cancel))

(cl-defmethod org-rl-make-clock ((marker symbol)
                                 start-time
                                 stop-time
                                 &optional
                                 current
                                 start-clean
                                 stop-clean
                                 cancel)
  ;; (org-rl-debug nil "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time :clean start-clean)
   :stop  (make-org-rl-time :time stop-time  :clean stop-clean)
   :current current
   :cancel cancel))

;; good
;; (org-rl-make-clock (point-marker) (current-time) 1 t)
;; (org-rl-make-clock (point-marker) (make-org-rl-time :time (current-time)) (make-org-rl-time :time (current-time)))



(defun org-rl-make-time (time &optional clean)
  (make-org-rl-time :time time :clean clean))

(defun org-rl-make-current-time (&optional clean)
  (make-org-rl-time :time 'now :clean clean))


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

(cl-defmethod org-rl-time-current-delta-secs ((time org-rl-time))
  (let ((gap (float-time
              (time-subtract
               (org-rl-time-get-time time)
               (current-time)))))
    (org-rl-debug :warning "org-rl-time-current-delta-secs: time=<%s> from current time = %d"
                  (org-rl-format time) gap)
    gap))

(cl-defmethod org-rl-time-current-min-p ((time org-rl-time))
  (org-rl-debug :warning "org-rl-time-current-min-p: time=<%s>" (org-rl-format time))
  (< (abs (org-rl-time-current-delta-secs time)) 60))

(cl-defmethod org-rl-clock-current-real ((clock org-rl-clock))
  (unless (org-rl-clock-null clock)
    (org-rl-clock-current clock)))

(cl-defmethod org-rl-clock-start-time ((clock org-rl-clock))
  (org-rl-time-get-time
   (org-rl-clock-start clock)))
(cl-defmethod (setf org-rl-clock-start-time) (time (clock org-rl-clock))
  (setf
   (org-rl-time-clean (org-rl-clock-start clock))
   (time-eq (org-rl-clock-start-time clock) time))
  (setf (org-rl-time-time (org-rl-clock-start clock)) time))
(cl-defmethod org-rl-clock-start-set ((clock org-rl-clock)
                                      time)
  (setf (org-rl-clock-start-time clock) time))

(cl-defmethod org-rl-clock-stop-time ((clock org-rl-clock))
  (org-rl-time-get-time
   (org-rl-clock-stop clock)))
(cl-defmethod (setf org-rl-clock-stop-time) (time (clock org-rl-clock))
  (setf
   (org-rl-time-clean (org-rl-clock-stop clock))
   (time-eq (org-rl-clock-stop-time clock) time))
  (setf (org-rl-time-time (org-rl-clock-stop clock)) time))
(cl-defmethod org-rl-clock-stop-set ((clock org-rl-clock)
                                      time)
  (setf (org-rl-clock-stop-time clock) time))


(cl-defmethod org-rl-clock-start-clean ((clock org-rl-clock))
  (org-rl-time-clean (org-rl-clock-start clock)))
(cl-defmethod (setf org-rl-clock-start-clean) (clean (clock org-rl-clock))
  (setf (org-rl-time-clean (org-rl-clock-start clock)) clean))
(cl-defmethod org-rl-clock-stop-clean ((clock org-rl-clock))
  (org-rl-time-clean (org-rl-clock-stop clock)))
(cl-defmethod (setf org-rl-clock-stop-clean) (clean (clock org-rl-clock))
  (setf (org-rl-time-clean (org-rl-clock-stop clock)) clean))

(cl-defmethod org-rl-clock-first-clock-beginning ((clock org-rl-clock))
  (org-clock-get-nth-half-clock-beginning
   (org-rl-clock-marker clock)))

(cl-defmethod org-rl-clock-null (clock)
  ;; TODO??
  (null clock))

(cl-defmethod org-rl-clock-null ((clock org-rl-clock))
  (let ((marker (org-rl-clock-marker clock)))
    ;; (org-rl-debug nil "org-rl-clock-null: clock[%s] =  %s"
    ;;               (org-rl-format-clock clock)
    ;;               marker)
    (or
     (equal marker 'imaginary)
     (null marker)
     (not (markerp marker)))))

;; (org-rl-clock-null (org-rl-make-clock 'imaginary 'now 'now))
;;
;; (setq x (org-rl-make-clock nil 'now 'now))
;; (setq y (org-rl-make-clock 'imaginary 'now 'now))
;;
;; (setf (org-rl-clock-marker x) nil)
;; (org-rl-clock-null x)
;; (org-rl-format-clock x)
;; (org-rl-get-time-gap-secs x y )

(cl-defmethod org-rl-clock-real-p ((clock org-rl-clock))
  (let ((marker (org-rl-clock-marker clock)))
    ;; (org-rl-debug nil "org-rl-clock-null: clock[%s] =  %s"
    ;;               (org-rl-format-clock clock)
    ;;               marker)
    (not
     (or
      (equal marker 'imaginary)
      (null marker)
      (not (markerp marker))))))

(cl-defmethod org-rl-clock-duration ((clock org-rl-clock))
  (let ((start (org-rl-clock-start-time clock))
        (stop  (org-rl-clock-stop-time  clock)))
    (let ((duration-sec (time-to-seconds
                         (time-subtract stop start))))
      (org-rl-debug nil "org-rl-clock-duration: duration %d" duration-sec)
      duration-sec)))

(cl-defmethod org-rl-clock-assert ((clock org-rl-clock))
  (cl-assert (>= (org-rl-clock-duration clock) 0)))

;; (let ((prev (org-rl-make-clock nil 'now 'now)))
;;   (org-rl-clock-start-set prev nil)
;;   (org-rl-clock-null prev))


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

(cl-defmethod org-rl-format-clock ((clock null))
  (format "null"))

(cl-defmethod org-rl-format-clock ((clock org-rl-clock))
  (let ((fmt (cdr org-time-stamp-formats)))
    (let* ((marker (org-rl-clock-marker clock))
           (heading
            (if (markerp marker)
                (lotus-with-marker marker
                  (org-get-heading t))
              "imaginary"))
           (start (format-time-string fmt (org-rl-clock-start-time clock)))
           (stop  (format-time-string fmt (org-rl-clock-stop-time clock))))
      (format "<%s %s> %s-%s %s" heading marker start stop (org-rl-clock-current clock)))))

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
      (setf (org-rl-clock-marker clock) (point-marker))
      (org-insert-time-stamp start t t "CLOCK: ")
      (insert "--")
      (org-insert-time-stamp stop t t)
      (org-update-all-dblocks)
      clock)))

(cl-defmethod org-rl-clock-replace ((clock org-rl-clock) &optional terminal)
  (org-rl-debug nil "org-rl-clock-replace: clock[%s] resume[%s]"
                (org-rl-format-clock clock)
                terminal)
  (if (org-rl-clock-null clock)
      (org-rl-debug nil "org-rl-clock-replace: %s clock is null" (org-rl-clock-marker clock))
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
                              (setf clock (org-rl-clock-insert-range clock)))))
                       ((eq terminal 'stop)
                        (if (= file-clock-start (org-rl-clock-start-time clock))
                            (progn
                              (kill-line)
                              (setf clock (org-rl-clock-insert-range clock)))))
                       (t
                        (kill-line)
                        (setf clock (org-rl-clock-insert-range clock)))))))))))
      (error "clock %s is not full" (org-rl-clock-name-bracket clock)))
    clock))


(cl-defmethod org-rl-clock-for-clock-op ((clock org-rl-clock))
  (cons
   (org-rl-clock-marker clock)
   (org-rl-clock-start-time clock)))

(cl-defmethod org-rl-clock-for-clock-in ((clock org-rl-clock))
  (org-rl-clock-for-clock-op clock))

(cl-defmethod org-rl-clock-for-clock-out ((clock org-rl-clock))
  (org-rl-clock-for-clock-op clock))


(cl-defmethod org-clock-clock-remove-last-clock ((clock org-rl-clock)))
;; TODO

(cl-defmethod org-rl-clock-clock-cancel ((clock org-rl-clock)
                                         &optional
                                         fail-quietly)
  (org-rl-debug nil "org-rl-clock-clock-cancel: clock[%s] fail-quietly[%s]"
                (org-rl-format-clock clock)
                fail-quietly)
  (setf (org-rl-clock-cancel clock) t)
  (if (org-rl-clock-real-p clock)
      (if (org-rl-clock-marker clock)
          (if (org-rl-clock-start-time clock)
              (org-clock-clock-cancel
               (org-rl-clock-for-clock-op clock))
            (error "%s start time is null" (org-rl-clock-start-time clock)))
        (error "org-rl-clock-clock-cancel: %s clock is null" (org-rl-clock-marker clock)))
    (org-rl-debug :warning "org-rl-clock-clock-cancel: clock %s is not real."
                  (org-rl-format-clock clock))))

(cl-defmethod org-rl-clock-clock-jump-to ((clock org-rl-clock))
  (org-rl-debug nil "org-rl-clock-clock-jump-to: clock[%s]"
                (org-rl-format-clock clock))
  (if (org-rl-clock-real-p clock)
      (org-clock-jump-to-current-clock
       (org-rl-clock-for-clock-op clock))
    (org-rl-debug :warning "org-rl-clock-clock-jump-to: clock %s is not real."
                  (org-rl-format-clock clock)))
  nil)


(cl-defmethod org-rl-clock-clock-in ((clock org-rl-clock)
                                     &optional
                                     resume)
  (org-rl-debug nil "org-rl-clock-clock-in: clock[%s] resume[%s]"
                (org-rl-format-clock clock)
                resume)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-real-p clock)
        (if (time-p (org-rl-clock-start-time clock))
            (org-rl-straight-org-clock-clock-in
             clock
             resume
             (org-rl-clock-start-time clock))
          (error "%s start time is null" (org-rl-clock-start-time clock)))
      (org-rl-debug :warning "org-rl-clock-clock-in: clock %s is not real."
                    (org-rl-format-clock clock)))))

(cl-defmethod org-rl-clock-clock-out ((clock org-rl-clock)
                                      &optional
                                      fail-quietly)
  ;;TODO: not updating org-clock-marker
  (org-rl-debug nil "org-rl-clock-clock-out: clock[%s] fail-quietly[%s]"
                (org-rl-format-clock clock)
                fail-quietly)
  (when (not org-clock-clocking-in)
    (if (org-rl-clock-real-p clock)
        (if (time-p (org-rl-clock-stop-time clock))
            (if (org-rl-clock-half-p clock)
                (progn
                  (if (or
                       (org-is-active-clock (org-rl-clock-for-clock-op clock))
                       (org-rl-clock-current clock))                   ;TODO: check for current clock? find some other way to find active clock like matching with org-clock-marker
                      (progn
                        (org-clock-out org-clock-out-switch-to-state
                                       fail-quietly
                                       (org-rl-clock-stop-time clock))
                        (setf (org-rl-clock-current clock) nil)
                        (setf (org-rl-clock-marker clock) (car org-clock-history)))
                    (org-clock-clock-out (org-rl-clock-for-clock-out clock)
                                         fail-quietly
                                         (org-rl-clock-stop-time clock)))
                  (setf (org-rl-clock-current clock) nil))
              (org-rl-clock-replace clock))
          (error "org-rl-clock-clock-out: %s stop time is null" (org-rl-clock-stop-time clock)))
      (org-rl-debug :warning "org-rl-clock-clock-out: clock %s is not real."
                    (org-rl-format-clock clock)))
    clock))

(cl-defmethod org-rl-clock-resume-if-stop-on-current-min ((clock org-rl-clock) resume)
  (when (and
         resume
         (org-rl-time-current-min-p (org-rl-clock-stop clock)))
    (if (eq resume t)
        t
      (y-or-n-p
       (format "Should resume clock %s: "
               (org-rl-format-clock clock))))))

(cl-defmethod org-rl-clock-clock-in-out ((clock org-rl-clock)
                                         &optional
                                         resume
                                         fail-quietly)
  (org-rl-debug nil "org-rl-clock-clock-in-out: clock[%s] resume[%s] org-clock-clocking-in[%s]"
                (org-rl-format-clock clock)
                resume
                org-clock-clocking-in)
  (let ((org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))
    (if (not org-clock-clocking-in)
        (if (org-rl-clock-real-p clock)
            (progn
              (org-rl-debug nil "org-rl-clock-clock-in-out in")

              (cl-assert (org-rl-clock-start-time clock))
              (cl-assert (org-rl-clock-stop-time clock))

              (let ((clock (org-rl-clock-clock-in clock resume)))
                ;; (setf (org-rl-clock-marker clock) marker)
                (org-rl-debug nil "org-rl-clock-clock-in-out out")
                (unless (org-rl-clock-resume-if-stop-on-current-min
                         clock
                         resume)
                  ;; (setf (org-rl-clock-current clock) t)
                  (org-rl-clock-clock-out clock fail-quietly))
                (org-rl-debug nil "org-rl-clock-clock-in-out out done")
                clock))
          (progn
            (org-rl-debug :warning "org-rl-clock-clock-in-out: clock %s is not real."
                          (org-rl-format-clock clock))
            clock))
      (error "Clock org-clock-clocking-in is %s" org-clock-clocking-in))))

(cl-defmethod org-rl-clock-restart-now ((clock org-rl-clock) resume)
  (let ((newclock (org-rl-make-clock
                   (org-rl-clock-marker clock)
                   'now
                   nil)))
    (org-rl-clock-clock-in newclock)))

(cl-defmethod org-rl-clock-action ((clock org-rl-clock)
                                   &option
                                   resume
                                   fail-quietly)
  ;; (when (org-rl-clock-start-clean clock)
  ;;   (org-rl-clock-clock-in clock resume fail-quietly))
  ;; (when (org-rl-clock-start-clean clock)
  ;;   (org-rl-clock-clock-out clock fail-quietly))
  )

(defun org-rl-clocks-action (resume fail-quietly &rest clocks)
  (dolist (clock clocks)
    (when nil
      (org-rl-clock-action clock resume fail-quietly)))
  clocks)


;; https://github.com/dfeich/org-clock-convenience/blob/master/org-clock-convenience.el
;; https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly

(cl-defmethod org-rl-clock-expand-time ((clock org-rl-clock) sec resume)
  "if sec is positive expand in future else expand in past."
  ;; do clock in clock out accordingly
  (org-rl-debug nil "org-rl-clock-expand-time: clock[%s] org-clock-clocking-in[%s]"
                (org-rl-format-clock clock)
                org-clock-clocking-in)
  (if (> sec 0)
      (progn
        (setf (org-rl-clock-stop-time clock) (time-add (org-rl-clock-stop-time clock) sec))
        (unless (org-rl-clock-resume-if-stop-on-current-min
                 clock
                 resume)
          (org-rl-clock-clock-out clock)))
    (progn
      (setf (org-rl-clock-start-time clock) (time-subtract (org-rl-clock-stop-time clock) sec))
      (org-rl-clock-replace clock)))
  clock)

(cl-defmethod org-rl-clock-contract-time ((clock org-rl-clock) sec)
  "if sec is positive contract from future else contract from past."
  (org-rl-debug nil "org-rl-clock-contract-time: clock[%s] org-clock-clocking-in[%s]"
                (org-rl-format-clock clock)
                org-clock-clocking-in))


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

;; TODO: Re-org here.

;; (setq
;;  org-rl-clock-opts-common
;;  '(("Done" . done)))

(cl-defmethod org-rl-clock-opts-common ((prev org-rl-clock)
                                        (next org-rl-clock)
                                        maxtimelen
                                        resume
                                        fail-quietly
                                        resume-clocks)
  (list
   (cons "Restart" 'restart)
   (when (and
          (org-rl-clock-real-p prev)
          (org-rl-clock-real-p next))
     (list (cons "Done" 'done)))))

(cl-defmethod org-rl-clock-opts-common-with-time ((prev org-rl-clock)
                                                  (next org-rl-clock)
                                                  maxtimelen
                                                  resume
                                                  fail-quietly
                                                  resume-clocks)
  (let ((args
         (list prev next maxtimelen resume fail-quietly resume-clocks)))
    (list
     (cons "Include in other" 'include-in-other))))

(cl-defmethod org-rl-clock-opts-prev ((prev org-rl-clock)
                                      (next org-rl-clock)
                                      maxtimelen
                                      resume
                                      fail-quietly
                                      resume-clocks)
  ;; (org-rl-debug nil :debug "calling org-rl-clock-opts-prev")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (when (org-rl-clock-real-p prev)
      (list
       (cons
        (format "Jump to prev %s" prev-heading)
        'jump-prev)
       (cons
        (format "Cancel prev %s" prev-heading)
        'cancel-prev)))))

(cl-defmethod org-rl-clock-opts-prev-with-time ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                maxtimelen
                                                resume
                                                fail-quietly
                                                resume-clocks)
  ;; (org-rl-debug nil :debug "calling org-rl-clock-opts-prev-with-time")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (list
     (cons
      (if (org-rl-clock-real-p prev)
          (format "Include in prev %s" prev-heading)
        (if (org-rl-clock-real-p next)
            (format "Subtract from next %s" next-heading)
          "No idea include-in-prev"))
      'include-in-prev))))

(cl-defmethod org-rl-clock-opts-next ((prev org-rl-clock)
                                      (next org-rl-clock)
                                      maxtimelen
                                      resume
                                      fail-quietly
                                      resume-clocks)
  ;; (org-rl-debug nil :debug "calling org-rl-clock-opts-next")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (when (org-rl-clock-real-p next)
      (list
       (cons
        (format "Jump to next %s" next-heading)
        'jump-next)
       (cons
        (format "Cancel next %s" prev-heading)
        'cancel-next)))))

(cl-defmethod org-rl-clock-opts-next-with-time ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                maxtimelen
                                                resume
                                                fail-quietly
                                                resume-clocks)
  ;; (org-rl-debug nil :debug "calling org-rl-clock-opts-next-with-time")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (list
     (cons
      (if (org-rl-clock-real-p next)
          (format "Include in next %s" next-heading)
        (if (org-rl-clock-real-p prev)
            (format "Subtract from prev %s" prev-heading)
          "No idea include-in-next"))
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
  (org-rl-debug nil "org-rl-select-other-clock: target[%s]" target)
  (org-with-refile
      file loc (or target org-refile-targets) "Refile other org heading"
    (let ((marker (make-marker)))
      (set-marker marker loc)
      marker)))

(cl-defmethod org-rl-get-time-gap ((prev org-rl-clock)
                                   (next org-rl-clock))
  (floor
   (float-time
    (time-subtract
     (org-rl-clock-start-time next)
     (org-rl-clock-stop-time prev)))))

(cl-defmethod org-rl-get-time-gap-secs ((prev org-rl-clock)
                                        (next org-rl-clock))
  (org-rl-get-time-gap prev next))

(cl-defmethod org-rl-get-time-gap-mins ((prev org-rl-clock)
                                        (next org-rl-clock))
  (floor (/ (org-rl-get-time-gap prev next) 60)))

(cl-defmethod org-rl-compare-time-gap ((prev org-rl-clock)
                                       (next org-rl-clock)
                                       timelen)
  (cl-assert (> (float-time (org-rl-get-time-gap prev next)) 0))
  (if (eq timelen 'all)
      0
    (-
     (float-time (org-rl-get-time-gap prev next))
     (abs timelen))))


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


(when nil
  (cl-defmethod org-rl-clock-build-options-OLD ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                maxtimelen
                                                resume
                                                fail-quietly
                                                resume-clocks)
    (org-rl-debug nil "org-rl-clock-build-options: prev[%s] next[%s] maxtimelen[%d] secs"
                  (org-rl-format-clock prev)
                  (org-rl-format-clock next)
                  maxtimelen)

    (let ((args (list prev
                      next
                      maxtimelen
                      resume
                      fail-quietly
                      resume-clocks)))
      (append
       (append
        (apply #'org-rl-clock-opts-prev args)
        (unless (zerop maxtimelen) (apply #'org-rl-clock-opts-prev-with-time args)))
       (append
        (apply #'org-rl-clock-opts-next args)
        (unless (zerop maxtimelen) (apply #'org-rl-clock-opts-next-with-time args)))
       (unless (zerop maxtimelen) (apply #'org-rl-clock-opts-common-with-time args))
       (apply #'org-rl-clock-opts-common args)))))

(cl-defmethod org-rl-clock-build-options ((prev org-rl-clock)
                                          (next org-rl-clock)
                                          maxtimelen
                                          resume
                                          fail-quietly
                                          resume-clocks)
  (org-rl-debug nil "org-rl-clock-build-options: prev[%s] next[%s] maxtimelen[%d] secs"
                (org-rl-format-clock prev)
                (org-rl-format-clock next)
                maxtimelen)

  (let* ((args (list prev
                     next
                     maxtimelen
                     resume
                     fail-quietly
                     resume-clocks))
         (options
          (append
           (if (org-rl-clock-null next)
               (append
                (apply #'org-rl-clock-opts-prev-with-time args)
                (apply #'org-rl-clock-opts-next-with-time args)
                (unless (zerop maxtimelen)
                  (apply #'org-rl-clock-opts-common-with-time args))
                (apply #'org-rl-clock-opts-next args)
                (apply #'org-rl-clock-opts-prev args))
             (append
              (apply #'org-rl-clock-opts-next-with-time args)
              (apply #'org-rl-clock-opts-prev-with-time args)
              (unless (zerop maxtimelen)
                (apply #'org-rl-clock-opts-common-with-time args))
              (apply #'org-rl-clock-opts-prev args)
              (apply #'org-rl-clock-opts-next args)))
           (apply #'org-rl-clock-opts-common args))))
    (mapcar
     #'(lambda (opt)
         (append
          (list (car opt) (cdr opt))
          args))
     options)))

(defvar org-rl-read-interval 60)

(defun org-rl-clock-read-option (interval prompt-fn options-fn default-fn)
  (let* ((options (if (functionp options-fn) (funcall options-fn) options-fn))
         (desopt (assoc (time-aware-completing-read interval prompt-fn options-fn) options))
         (des (car desopt))
         (opt (cdr desopt)))
    (org-rl-debug :warning "Selected option is %s[ %s ]" des (car opt))
    opt))

(defvar org-rl-clock-time-direction-reverse nil)

(defun org-rl-clock-read-timelen (interval prompt-fn option-fn maxtimelen-fn)
  "read in mins return secs"
  (let ((option     (if (functionp option-fn)     (funcall option-fn) option-fn))
        (maxtimelen (if (functionp maxtimelen-fn) (funcall maxtimelen-fn) maxtimelen-fn)))
    (if (or
         (zerop maxtimelen)
         (memq option
               '(done
                 cancel-next
                 cancel-prev)))
        maxtimelen
      (*
       (*
        (if org-rl-clock-time-direction-reverse -1 1)
        (time-aware-read-number interval prompt-fn maxtimelen-fn))
       60))))


(defun helm-cand-sel ()
  (interactive)
  (let ((retval
         (helm :sources '((name . "HELM")
                          (match (lambda (_candidate)
                                   (lwarn 'test :warning "_candidate %s (helm-get-selection) %s" _candidate (helm-get-selection))
                                   (evenp (string-to-number _candidate))))
                          (candidates . (1 2 3 4))
                          (action . (("open" . (lambda (candidate)
                                                 (list
                                                  (when (minibufferp (current-buffer))
                                                    (with-current-buffer "* Minibuffer 1"
                                                      (minibuffer-contents-no-properties)))

                                                  (with-current-buffer "* Minibuffer 1"
                                                    (minibuffer-contents-no-properties))
                                                  candidate
                                                  (helm-get-selection))))))))))
    (org-rl-debug nil "retval %s" retval)))


;; org-rl-clock :debug: 2019-04-19 17:00:58 s: org-rl-clock-cps-resolve-time: [body] lotus-with-override-minibuffer-if
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; org-rl-clock :debug: 2019-04-19 17:00:58 s: org-rl-clock-cps-resolve-time: going to run prev[<STARTED Unnamed task 812> <2019-04-16 Tue 13:29>-<2019-04-16 Tue 13:29> nil 6 6] next[<imaginary> <2019-04-19 Fri 17:00>-<2019-04-19 Fri 17:00> nil 23 23] with maxtimelen 271918
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; org-rl-clock :debug: 2019-04-19 17:00:58 s: org-rl-clock-build-options: prev[<STARTED Unnamed task 812> <2019-04-16 Tue 13:29>-<2019-04-16 Tue 13:29> nil] next[<imaginary> <2019-04-19 Fri 17:00>-<2019-04-19 Fri 17:00> nil] maxtimelen[271918] secs
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org [4 times]
;; org-rl-clock-cps-resolve-time: options ((Include in prev STARTED Unnamed task 812 include-in-prev [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (Subtract from prev STARTED Unnamed task 812 include-in-next [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (Include in other include-in-other [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (Jump to prev STARTED Unnamed task 812 jump-prev [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (Cancel prev STARTED Unnamed task 812 cancel-prev [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (Restart restart [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil) (nil nil [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil))
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; prev[<STARTED Unnamed task 812> 6 6] next[<imaginary> 23 23] Select option [4531]:
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; Parsing org-rl-obj-cps.el (LL)...done
;; org-rl-clock :warning: 2019-04-19 17:04:39 s: started org-rl-clock-cps-process-helm-option opt: (include-in-prev [cl-struct-org-rl-clock #<marker (moves after insertion) at 460021 in Unnamed.org> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23733 35652) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 271918 nil nil nil)
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; org-rl-clock :warning: 2019-04-19 17:04:39 s: in org-rl-clock-cps-process-helm-option opt[ include-in-prev]
;; org-rl-clock :warning: 2019-04-19 17:04:39 s: started org-rl-clock-cps-process-option selected opt=include-in-prev
;; org-rl-clock :debug: 2019-04-19 17:04:39 s: org-rl-clock-time-process-option: begin
;; org-rl-clock :warning: 2019-04-19 17:04:39 s: started org-rl-clock-time-process-option: selected opt=include-in-prev
;; org-rl-clock :debug: 2019-04-19 17:04:39 s: begin org-rl-clock-opt-include-in-prev
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; org-rl-clock :debug: 2019-04-19 17:04:39 s: org-rl-clock-expand-time: clock[<STARTED Unnamed task 812> <2019-04-16 Tue 13:29>-<2019-04-16 Tue 13:29> nil] org-clock-clocking-in[nil]
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org
;; org-rl-clock :debug: 2019-04-19 17:04:39 s: org-rl-clock-clock-out: clock[<STARTED Unnamed task 812> <2019-04-16 Tue 13:29>-<2019-04-19 Fri 17:04> nil] fail-quietly[nil]
;; Clock stopped at [2019-04-19 Fri 17:04] after 3d 3:35
;; Saving file /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/Unnamed.org...
;; Wrote /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/.Unnamed.org.~undo-tree~
;; Wrote /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/Unnamed.org
;; Error callling git diff:

;; Preparing diary...
;; No diary entries for Friday, April 19, 2019: Good Friday
;; Preparing diary...done
;; Appointment reminders enabled
;; Could disable it with disable-diary-appt-display-for function.
;; Loading /home/s/hell/.emacs.d/.cache/.org-timestamps/tasks-notes.cache...done
;; Publishing file /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/Unnamed.org using ‘org-html-publish-to-html’
;; Saving file /home/s/hell/Documents/CreatedContent/gen/virtual/org/default/tasks/html/Unnamed.html...
;; Wrote /home/s/hell/Documents/CreatedContent/gen/virtual/org/default/tasks/html/Unnamed.html
;; Preparing diary...
;; No diary entries for Friday, April 19, 2019: Good Friday
;; Preparing diary...done
;; Appointment reminders enabled
;; Could disable it with disable-diary-appt-display-for function.
;; auto published blog
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: finish org-rl-clock-opt-include-in-prev
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-time-process-option: finished
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: (org-rl-clock-null prev[<nil> <2019-04-16 Tue 13:29>-<2019-04-19 Fri 17:04> nil]) nil
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: (org-rl-clock-null next[<imaginary> <2019-04-19 Fri 17:04>-<2019-04-19 Fri 17:04> nil]) t
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-duration: duration 272100
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-duration: duration 0
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-cps-resolve-time: begin
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-cps-resolve-time: finished
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-cps-resolve-time: [body] lotus-with-override-minibuffer-if
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-cps-resolve-time: going to run prev[<nil> <2019-04-16 Tue 13:29>-<2019-04-19 Fri 17:04> nil 6 27] next[<imaginary> <2019-04-19 Fri 17:04>-<2019-04-19 Fri 17:04> nil 27 27] with maxtimelen 41
;; org-rl-clock :debug: 2019-04-19 17:04:41 s: org-rl-clock-build-options: prev[<nil> <2019-04-16 Tue 13:29>-<2019-04-19 Fri 17:04> nil] next[<imaginary> <2019-04-19 Fri 17:04>-<2019-04-19 Fri 17:04> nil] maxtimelen[41] secs
;; org-rl-clock-cps-resolve-time: options ((Include in prev nil include-in-prev [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (Subtract from prev nil include-in-next [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (Include in other include-in-other [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (Jump to prev nil jump-prev [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (Cancel prev nil cancel-prev [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (Restart restart [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))) (nil nil [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil] [cl-struct-org-rl-clock imaginary [cl-struct-org-rl-time now nil] [cl-struct-org-rl-time now nil] nil nil] 41 nil nil ((:prev . [cl-struct-org-rl-clock #<marker (moves after insertion) in no buffer> [cl-struct-org-rl-time (23733 35652) nil] [cl-struct-org-rl-time (23737 45608) nil] nil nil]))))
;; prev[<> 6 27] next[<imaginary> 27 27] Select option [0]:
;; Error in post-command-hook (org-add-log-note): (error "Can’t expand minibuffer to full frame")
;; Mark set [2 times]


;;; org-rl-obj.el ends here
