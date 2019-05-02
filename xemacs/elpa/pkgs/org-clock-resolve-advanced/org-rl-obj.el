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


(defun org-rl-debug (level &rest args)
  (let* ((ilevel (or level :debug))
         (ts (time-stamp-string))
         (fmt (format "%s: %s" ts (car args)))
         (args (append (list fmt) (cdr args))))
    (apply #'lwarn 'org-rl-clock ilevel args)

    (when level
      (message
       "%s"
       (concat
        (format "org-rl-clock %s: " ilevel)
        (apply #'format args))))))


(defvar org-rl-org-clock-persist nil "Control org-clock-persist at time of org-resolve clock-in")
(defvar org-rl-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of org-resolev clock-in")

;; lotus-with-file-pos-new-win: selecting buf report.org<hostapdng> [2 times]
;; org--align-node-property: Match data clobbered by buffer modification hooks
;; TODO: FIX: org--align-node-property: Match data clobbered by buffer modification hooks
(defun org-rl-straight-org-clock-clock-in (clock &optional resume start-time)
  (progn
    (org-rl-debug nil "org-rl-straight-org-clock-clock-in: begin")
    (lotus-org-clock-load-only)
    (let ((org-clock-persist               org-rl-org-clock-persist)
          (org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))

      (org-with-narrow-to-marker (org-rl-clock-marker clock)
        (lotus-org-with-safe-modification
          (org-entry-put nil "Effort" "10")))

      (org-clock-clock-in
       (org-rl-clock-for-clock-in clock)
       resume start-time)

      (setf (org-rl-clock-marker clock) org-clock-marker)
      (setf (org-rl-clock-current clock) t)
      clock)))


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
  time)

(cl-defstruct org-rl-clock
  marker
  start
  stop
  current)

(cl-defmethod org-rl-make-clock ((marker marker)
                                 (start org-rl-time)
                                 (stop org-rl-time)
                                 &optional
                                 current)
  (make-org-rl-clock
   :marker marker
   :start start
   :stop  stop
   :current current))

(cl-defmethod org-rl-make-clock ((marker marker)
                                 start-time
                                 stop-time
                                 &optional
                                 current)
  ;; (org-rl-debug nil "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time)
   :stop  (make-org-rl-time :time stop-time)
   :current current))

(cl-defmethod org-rl-make-clock ((marker symbol)
                                 start-time
                                 stop-time
                                 &optional
                                 current)
  ;; (org-rl-debug nil "calling 2")
  (make-org-rl-clock
   :marker marker
   :start (make-org-rl-time :time start-time)
   :stop  (make-org-rl-time :time stop-time)
   :current current))

;; good
;; (org-rl-make-clock (point-marker) (current-time) 1 t)
;; (org-rl-make-clock (point-marker) (make-org-rl-time :time (current-time)) (make-org-rl-time :time (current-time)))



(defun org-rl-make-time (time)
  (make-org-rl-time :time time))

(defun org-rl-make-current-time ()
  (make-org-rl-time :time 'now))


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

(defun org-get-heading-from-clock (clock)
  (let ((mrk (car clock)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (lotus-with-marker mrk
          (org-rl-debug :warning "org-get-heading-from-clock: marker = %s" mrk)
          (org-get-heading t))
      "imaginary")))

(cl-defmethod org-rl-clock-heading ((clock org-rl-clock))
  (let ((mrk (org-rl-clock-marker clock)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (lotus-with-marker mrk
         (org-rl-debug :warning "org-get-heading-from-clock: marker = %s" mrk)
         (org-get-heading t)))
    "imaginary"))

(cl-defmethod org-rl-format ((clock null))
  (format "null"))

(cl-defmethod org-rl-format ((clock org-rl-clock))
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
  (setf (org-rl-time-time (org-rl-clock-start clock)) time))
(cl-defmethod org-rl-clock-start-set ((clock org-rl-clock)
                                      time)
  (setf (org-rl-clock-start-time clock) time))

(cl-defmethod org-rl-clock-stop-time ((clock org-rl-clock))
  (org-rl-time-get-time
   (org-rl-clock-stop clock)))
(cl-defmethod (setf org-rl-clock-stop-time) (time (clock org-rl-clock))
  (setf (org-rl-time-time (org-rl-clock-stop clock)) time))
(cl-defmethod org-rl-clock-stop-set ((clock org-rl-clock)
                                     time)
  (setf (org-rl-clock-stop-time clock) time))

(cl-defmethod org-rl-clock-first-clock-beginning ((clock org-rl-clock))
  (org-clock-get-nth-half-clock-beginning
   (org-rl-clock-marker clock)))

(cl-defmethod org-rl-clock-null (clock)
  ;; TODO??
  (null clock))

(cl-defmethod org-rl-clock-null ((clock org-rl-clock))
  (let ((marker (org-rl-clock-marker clock)))
    (or
     (equal marker 'imaginary)
     (null marker)
     (not (markerp marker)))))

(cl-defmethod org-rl-clock-real-p ((clock org-rl-clock))
  (let ((marker (org-rl-clock-marker clock)))
    (not (org-rl-clock-null clock))))

(cl-defmethod org-rl-clock-duration ((clock org-rl-clock))
  (let ((start (org-rl-clock-start-time clock))
        (stop  (org-rl-clock-stop-time  clock)))
    (let ((duration-sec (time-to-seconds
                         (time-subtract stop start))))
      (org-rl-debug nil "org-rl-clock-duration: duration %d" duration-sec)
      duration-sec)))

(cl-defmethod org-rl-clock-assert ((clock org-rl-clock))
  (cl-assert (>= (org-rl-clock-duration clock) 0)))


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
                (org-rl-format clock)
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
                (org-rl-format clock)
                fail-quietly)
  ;; (setf (org-rl-clock-cancel clock) t)
  (if (org-rl-clock-real-p clock)
      (if (org-rl-clock-marker clock)
          (if (org-rl-clock-start-time clock)
              (org-clock-clock-cancel
               (org-rl-clock-for-clock-op clock))
            (error "%s start time is null" (org-rl-clock-start-time clock)))
        (error "org-rl-clock-clock-cancel: %s clock is null" (org-rl-clock-marker clock)))
    (org-rl-debug :warning "org-rl-clock-clock-cancel: clock %s is not real."
                  (org-rl-format clock))))

(cl-defmethod org-rl-clock-clock-jump-to ((clock org-rl-clock))
  (org-rl-debug nil "org-rl-clock-clock-jump-to: clock[%s]"
                (org-rl-format clock))
  (if (org-rl-clock-real-p clock)
      (org-clock-jump-to-current-clock
       (org-rl-clock-for-clock-op clock))
    (org-rl-debug :warning "org-rl-clock-clock-jump-to: clock %s is not real."
                  (org-rl-format clock)))
  nil)


(cl-defmethod org-rl-clock-clock-in ((clock org-rl-clock)
                                     &optional
                                     resume)
  (org-rl-debug nil "org-rl-clock-clock-in: clock[%s] resume[%s]"
                (org-rl-format clock)
                resume)
  (if (not org-clock-clocking-in)
    (if (org-rl-clock-real-p clock)
        (if (time-p (org-rl-clock-start-time clock))
            (org-rl-straight-org-clock-clock-in
             clock
             resume
             (org-rl-clock-start-time clock))
          (error "%s start time is null" (org-rl-clock-start-time clock)))
      (org-rl-debug :warning "org-rl-clock-clock-in: clock %s is not real."
                    (org-rl-format clock)))
    (org-rl-debug nil "org-rl-clock-clock-in: Error org-clock-clocking-in = %s" org-clock-clocking-in)))

(cl-defmethod org-rl-clock-clock-out ((clock org-rl-clock)
                                      &optional
                                      fail-quietly)
  ;;TODO: not updating org-clock-marker
  (org-rl-debug nil "org-rl-clock-clock-out: clock[%s] fail-quietly[%s]"
                (org-rl-format clock)
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
                    (org-rl-format clock)))
    clock))

(cl-defmethod org-rl-clock-resume-if-stop-on-current-min ((clock org-rl-clock) resume)
  (when (and
         resume
         (org-rl-time-current-min-p (org-rl-clock-stop clock)))
    (if (eq resume t)
        t
      (y-or-n-p
       (format "Should resume clock %s: "
               (org-rl-format clock))))))

(cl-defmethod org-rl-clock-clock-in-out ((clock org-rl-clock)
                                         &optional
                                         resume
                                         fail-quietly)
  (org-rl-debug nil "org-rl-clock-clock-in-out: clock[%s] resume[%s] org-clock-clocking-in[%s]"
                (org-rl-format clock)
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
                          (org-rl-format clock))
            clock))
      (error "Clock org-clock-clocking-in is %s" org-clock-clocking-in))))

(cl-defmethod org-rl-clock-restart-now ((clock org-rl-clock) resume)
  (let ((newclock (org-rl-make-clock
                   (org-rl-clock-marker clock)
                   'now
                   nil)))
    (org-rl-clock-clock-in newclock)))


;; https://github.com/dfeich/org-clock-convenience/blob/master/org-clock-convenience.el
;; https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly

(cl-defmethod org-rl-clock-expand-time ((clock org-rl-clock) sec resume)
  "if sec is positive expand in future else expand in past."
  ;; do clock in clock out accordingly
  (org-rl-debug nil "org-rl-clock-expand-time: clock[%s] org-clock-clocking-in[%s]"
                (org-rl-format clock)
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
                (org-rl-format clock)
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
                         (org-rl-format prev)
                         (if (org-rl-clock-start-time prev) (% (/ (floor (float-time (org-rl-clock-start-time prev))) 60) base) 0)
                         (if (org-rl-clock-stop-time prev)  (% (/ (floor (float-time (org-rl-clock-stop-time prev))) 60) base) 0)
                         ;; (org-rl-clock-marker next)
                         (org-rl-format next)
                         (if (org-rl-clock-start-time next) (% (/ (floor (float-time (org-rl-clock-start-time next))) 60) base) 0)
                         (if (org-rl-clock-stop-time next)  (% (/ (floor (float-time (org-rl-clock-stop-time next))) 60) base) 0)))
         (debug (if prompt (concat prompt " " _debug) _debug)))
    (when stop (read-from-minibuffer (format "%s test: " debug)))
    debug))


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

(defun org-rl-select-other-clock (&optional target)
  (interactive)
  (org-rl-debug nil "org-rl-select-other-clock: target[%s]" target)
  (org-with-refile
      file loc (or target org-refile-targets) "Refile other org heading"
    (let ((marker (make-marker)))
      (set-marker marker loc)
      marker)))


(defvar org-rl-clock-fixed-heading
  '())

(defun org-rl-find-heading-marker (file heading)
  (let ((buff (find-file-noselect file)))
    (when buff
      (with-current-buffer buff
        (org-find-exact-headline-in-buffer heading)))))

(cl-defgeneric org-rl-find-all-other-marker-options (prev
                                                     next
                                                     maxtimelen
                                                     resume
                                                     fail-quietly
                                                     resume-clocks)
  "")

(cl-defmethod org-rl-find-all-other-marker-options ((prev org-rl-clock)
                                                    (next org-rl-clock)
                                                    maxtimelen
                                                    resume
                                                    fail-quietly
                                                    resume-clocks)
  (remove nil
          (mapcar #'(lambda (file-heading)
                      (let* ((file    (car file-heading))
                             (heading (cdr file-heading))
                             (marker  (org-rl-find-heading-marker file heading)))
                        (when marker
                          (list
                           :option
                           (format "Include in %s" heading)
                           (cons 'include-in-other marker)
                           prev next maxtimelen resume fail-quietly resume-clocks))))
                  org-rl-clock-fixed-heading)))


(defvar org-rl-capture+-helm-templates-alist org-capture+-helm-templates-alist)

;; (setq
;;  org-rl-capture+-helm-templates-alist
;;  '(("TODO" "* TODO %? %^g% %i [%a]" "* MILESTONE %? %^g %i [%a]")
;;    ("MEETING" "* MEETING %? %^g %i [%a]")))

;; (defun org-rl-build-capture+-option (interval prompt-fn options-fn default-fn)
;;   "To create new org entry"
;;   (let ((action #'(lambda ()
;;                     (let ((template (occ-capture+-helm-select-template)))
;;                       (when template
;;                         (let ((mrk (get-marker)))
;;                           (with-org-capture+ 'entry `(marker ,mrk) template '(:empty-lines 1)
;;                             (let ((capture-clock (make-org-rl-clock (point))))
;;                               t))))))))
;;     (helm-build-sync-source name
;;       :candidates (if (functionp options-fn)
;;                       (funcall options-fn)
;;                     options-fn)
;;       :action (list
;;                (cons "New Task" 'new-task))
;;       :action-transformer #'(lambda (actions candidate)
;;                               (list (cons "select"))))))
(cl-defgeneric org-rl-find-all-new-template-options (prev
                                                     next
                                                     maxtimelen
                                                     resume
                                                     fail-quietly
                                                     resume-clocks)
  "")

(cl-defmethod org-rl-find-all-new-template-options ((prev org-rl-clock)
                                                    (next org-rl-clock)
                                                    maxtimelen
                                                    resume
                                                    fail-quietly
                                                    resume-clocks)

  (mapcar
   #'(lambda (list)
       (cons
        (car list)
        (append
         (list (list :helm :multiline t))
         (mapcar #'(lambda (template)
                      (list
                       :option
                       (format "%s" template)
                       (cons 'include-in-new template)
                       prev next maxtimelen resume fail-quietly resume-clocks))
                  (cdr list)))))
   org-rl-capture+-helm-templates-alist))


(cl-defmethod org-rl-clock-opts-common ((prev org-rl-clock)
                                        (next org-rl-clock)
                                        maxtimelen
                                        resume
                                        fail-quietly
                                        resume-clocks)
  (list
   (list :option "Restart" 'restart prev next maxtimelen resume fail-quietly resume-clocks)
   (list :option "Done"    'done prev next maxtimelen resume fail-quietly resume-clocks)))

(cl-defmethod org-rl-clock-opts-other-clock-with-time ((prev org-rl-clock)
                                                       (next org-rl-clock)
                                                       maxtimelen
                                                       resume
                                                       fail-quietly
                                                       resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-other-clock-with-time")
  (let ((args
         (list prev next maxtimelen resume fail-quietly resume-clocks)))
    (append
     (apply #'org-rl-find-all-other-marker-options args)
     (list
      (list :option
            "Include in other"
            'include-in-other prev next maxtimelen resume fail-quietly resume-clocks)))))

(cl-defmethod org-rl-clock-opts-new-clock-with-time ((prev org-rl-clock)
                                                     (next org-rl-clock)
                                                     maxtimelen
                                                     resume
                                                     fail-quietly
                                                     resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-new-clock-with-time")
  (let ((args
         (list prev next maxtimelen resume fail-quietly resume-clocks)))
    (append
     (apply #'org-rl-find-all-new-template-options args)
     (list
      (list
       :option
       "Include in new"
       'include-in-new prev next maxtimelen resume fail-quietly resume-clocks)))))

(cl-defmethod org-rl-clock-opts-prev ((prev org-rl-clock)
                                      (next org-rl-clock)
                                      maxtimelen
                                      resume
                                      fail-quietly
                                      resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-prev")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (when (org-rl-clock-real-p prev)
      (list
       (list
        :option
        (format "Jump to prev %s" prev-heading)
        'jump-prev prev next maxtimelen resume fail-quietly resume-clocks)
       (list
        :option
        (format "Cancel prev %s" prev-heading)
        'cancel-prev prev next maxtimelen resume fail-quietly resume-clocks)))))

(cl-defmethod org-rl-clock-opts-prev-with-time ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                maxtimelen
                                                resume
                                                fail-quietly
                                                resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-prev-with-time")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (list
     (list
      :option
      (if (org-rl-clock-real-p prev)
          (format "Include in prev %s" prev-heading)
        (if (org-rl-clock-real-p next)
            (format "Subtract from next %s" next-heading)
          "No idea include-in-prev"))
      'include-in-prev prev next maxtimelen resume fail-quietly resume-clocks))))

(cl-defmethod org-rl-clock-opts-next ((prev org-rl-clock)
                                      (next org-rl-clock)
                                      maxtimelen
                                      resume
                                      fail-quietly
                                      resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-next")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (when (org-rl-clock-real-p next)
      (list
       (list
        :option
        (format "Jump to next %s" next-heading)
        'jump-next prev next maxtimelen resume fail-quietly resume-clocks)
       (list
        :option
        (format "Cancel next %s" prev-heading)
        'cancel-next prev next maxtimelen resume fail-quietly resume-clocks)))))

(cl-defmethod org-rl-clock-opts-next-with-time ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                maxtimelen
                                                resume
                                                fail-quietly
                                                resume-clocks)
  (org-rl-debug nil "calling org-rl-clock-opts-next-with-time")
  (let ((prev-heading (org-rl-clock-heading prev))
        (next-heading (org-rl-clock-heading next)))
    (list
     (list
      :option
      (if (org-rl-clock-real-p next)
          (format "Include in next %s" next-heading)
        (if (org-rl-clock-real-p prev)
            (format "Subtract from prev %s" prev-heading)
          "No idea include-in-next"))
      'include-in-next prev next maxtimelen resume fail-quietly resume-clocks))))


(cl-defmethod org-rl-clock-build-options ((prev org-rl-clock)
                                          (next org-rl-clock)
                                          maxtimelen
                                          resume
                                          fail-quietly
                                          resume-clocks)
  (org-rl-debug nil "org-rl-clock-build-options: prev[%s] next[%s] maxtimelen[%d] secs"
                (org-rl-format prev)
                (org-rl-format next)
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
               (list
                (cons
                 "Usual"
                 (append
                  (apply #'org-rl-clock-opts-prev-with-time args)
                  (apply #'org-rl-clock-opts-next-with-time args)))
                (unless (zerop maxtimelen)
                  (cons "Other"
                        (apply #'org-rl-clock-opts-other-clock-with-time args)))
                (unless (zerop maxtimelen)
                  (cons "News"
                        (apply #'org-rl-clock-opts-new-clock-with-time args)))
                (cons
                 "Cancel"
                 (append
                  (apply #'org-rl-clock-opts-next args)
                  (apply #'org-rl-clock-opts-prev args))))
             (list
              (cons
               "Usual"
               (append
                (apply #'org-rl-clock-opts-next-with-time args)
                (apply #'org-rl-clock-opts-prev-with-time args)))
              (unless (zerop maxtimelen)
                (cons "Other"
                      (apply #'org-rl-clock-opts-other-clock-with-time args)))
              (unless (zerop maxtimelen)
                (cons "News"
                      (apply #'org-rl-clock-opts-new-clock-with-time args)))
              (cons
               "Cancel"
               (cons
                (apply #'org-rl-clock-opts-prev args)
                (apply #'org-rl-clock-opts-next args)))))
           (list
            (cons
             "Common"
             (apply #'org-rl-clock-opts-common args))))))
    (org-rl-debug nil "org-rl-clock-build-options: options %s" options)
    (org-rl-debug nil "org-rl-clock-build-options: done")
    options))

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

;;; org-rl-obj.el ends here
