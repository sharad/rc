;;; org-timesheet.el --- org-timesheet               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(require 'org-table)
(require 'org-clock)

(defcustom org-time-sheet-date-formatter
  (lambda (day month year) (format "%4d-%02d-%02d" year month day))
  "Function to format date in time sheets.
It takes three numbers as arguments: day month year."
  :type 'function
  :group 'org-clock)

(defcustom org-time-sheet-time-formatter
  (lambda (start end hour minutes headings)
    (list (format-time-string "%F %R" (apply 'encode-time minutes-start))
          (format-time-string "%F %R" (apply 'encode-time minutes-end))
          (format "%2d00--%2d00" hour (1+ hour)) (or (nth 1 headings) "") (or (nth 2 headings) "") minutes))
  "Callback function returning one table line in a time sheet (as list).
The arguments of the function are:
START:    start time with format as in `decode-time'
END:     end time with format as in `decode-time'
MINUTES:  number of minutes between start time and end time
HEADINGS: the heading titles of the current entry and all its parents as a list starting with the top-parent."
  :type 'function
  :group 'org-clock)

(eval-when-compile
  (require 'cl-lib))
(require 'org-element)
(require 'ob-core)

(defun org-element-parent (element &optional type)
  "Get parent of ELEMENT or nil if there is none.
If TYPE is non-nil get next parent of that type."
  (let* ((props (cadr element))
         (parent (plist-get props :parent)))
    (if type
        (when parent
          (if (eq (car parent) type)
              parent
              (org-element-parent parent type)))
        parent)))

(defun org-element-timestamp-less-p (ts1 ts2 &optional end)
  "Non-nil if timestamp TS1 is less than timestamp TS2.
TS1 and TS2 is timestamp data as returned by `org-element-timestamp-parser'.
If end is non-nil the end-time of TS1 and TS2 is compared else the start time."
  (cl-assert (eq (car ts1) 'timestamp) "TS1 is not a timestamp")
  (cl-assert (eq (car ts2) 'timestamp) "TS2 is not a timestamp")
  (let ((p1 (cadr ts1))
        (p2 (cadr ts2))
        (tests '("year" "month" "day" "hour" "minute"))
        ret)
    (while (and (let* ((what (intern-soft (concat ":" (car tests) (if end "-end" "-start"))))
                       (t1 (plist-get p1 what))
                       (t2 (plist-get p2 what)))
                  (cond
                    ((< t1 t2)
                     (setq ret t)
                     nil)
                    ((= t1 t2) t)))
                (setq tests (cdr tests))))
    ret))

(defun time-day-month-year (time)
  "Return the list (day month year) from TIME.
TIME may be the time as returned by `current-time' or by `decode-time'."
  (if (<= (length time) 4)
      (setq time (decode-time time)))
  (mapcar (lambda (el) (nth el time)) '(3 4 5)))

(defun org-element-timestamp-to-time (timestamp &optional start/end encode)
  "Convert start or end of TIMESTAMP returned by `org-element-timestamp-parser'
to time format as defined in the documentation of `decode-time'.
START/END is either the symbol 'start or 'end or nil which is equivalent to 'start.
If ENCODE is non-nil the return value is encoded as described in the documentation for `current-time'."
  (cl-assert (eq (car timestamp) 'timestamp) "Argument is not a timestamp")
  (unless start/end (setq start/end 'start))
  (let* ((p (cadr timestamp))
         (ret (append
               '(0)
               (mapcar (lambda (what) (plist-get p (intern-soft (concat ":" what "-" (symbol-name start/end))))) '("minute" "hour" "day" "month" "year"))
               (list 0 nil (car (current-time-zone))))))
    (if encode
        (apply #'encode-time ret)
        ret)))

(defmacro decoded-time-complete-timezone (t1 t2)
  "If only one of the time specifications T1 and T2 has time-zone information
append that to the other one."
  `(let ((n1 (length ,t1))
         (n2 (length ,t2)))
     (cond
       ((> n1 n2)
        (setq ,t2 (copy-sequence ,t2))
        (setf (nthcdr n2 ,t2) (nthcdr n2 ,t1)))
       ((< n1 n2)
        (setq ,t1 (copy-sequence ,t1))
        (setf (nthcdr n1 ,t1) (nthcdr n1 ,t2))))))

(defun decoded-time-less-p (t1 t2)
  "Like `time-less-p' but for decoded time values as `decode-time' returns."
  (decoded-time-complete-timezone t1 t2)
  (time-less-p (apply 'encode-time t1) (apply 'encode-time t2)))

(defun decoded-time-advance (time dt)
  "Return TIME advanced by DT but for decoded time values as `decode-time' returns.
The time zone information of time is used for the result."
  (decode-time (apply 'encode-time (append (cl-mapcar #'+ (butlast time (- (length time) 6)) (butlast dt (- (length dt) 6))) (nthcdr 6 time)))))

(defun org-time-sheet (&optional tStart tEnd &optional dont-sum)
  "Create time sheet for time span from tStart to tEnd from current org buffer.
When called non-interactively each of the parameters tStart and tEnd may be nil
or must be decoded time (see `decode-time').
Do not sum up minutest of a project within an hour if dont-sum is non-nil.
Interactively do not sum if called with prefix arg."
  (interactive (list
                (decode-time (org-read-date t t nil "Start time:" '(0 0)))
                (decode-time (org-read-date t t nil "End time:"))
                current-prefix-arg))
  (org-time-sheet-shedule (org-time-sheet-collect tStart tEnd) (called-interactively-p 'any) dont-sum))

(defun org-time-sheet-collect (tStart tEnd)
  "Returns ordered time sheet collection of current buffer
for clocked items with start time within the range from tStart to tEnd."
  (if (> (length tStart) 4)
      (setq tStart (apply 'encode-time tStart)))
  (if (> (length tEnd) 4)
      (setq tEnd (apply 'encode-time tEnd)))
  (let ((tree (org-element-parse-buffer)))
    (cl-stable-sort
     (org-element-map tree 'clock
       (lambda (clock)
         ;; get the relevant data of the clocks
         (let* ((timestamp (plist-get (cadr clock) :value))
                (parent clock)
                (headers (nreverse (cl-loop while (setq parent (org-element-parent parent 'headline)) collect (car (plist-get (cadr parent) :title))))))
           (cl-assert timestamp nil "Clock line without timestamp")
           (when (and (or (null tStart) (null (time-less-p (org-element-timestamp-to-time timestamp 'start t) tStart)))
                      (or (null tEnd) (time-less-p (org-element-timestamp-to-time timestamp 'end t) tEnd)))
             (list (org-element-timestamp-to-time timestamp 'start)
                   (org-element-timestamp-to-time timestamp 'end)
                   headers))
           )))
     #'time-less-p
     :key (lambda (clock) (apply 'encode-time (car clock))))))

(defun org-time-sheet-shedule (clocks &optional interactive dont-sum)
  "Creates time sheet shedule from ordered time sheet clock collection (see `org-time-sheet-collect')."
  ;; sheduling
  (when clocks
    (setq clocks (cons nil clocks))
    (let* ((start (copy-sequence (caadr clocks)))
           (day-month-year (time-day-month-year start))
           (shedule (list (list (apply org-time-sheet-date-formatter day-month-year)))))
      (setf (nth 1 start) 0) ;; clear minutes
      (while (cdr clocks)
        (let ((end (decoded-time-advance start '(0 0 1 0 0 0)))
              project-alist
              (iter clocks))
          (while (decoded-time-less-p (cl-caadr iter) end) ;; collect clocks starting before the end of current hour
            (let* ((start-time (cl-caadr iter))
                   (end-time (cl-cadadr iter))
                   (minutes-start (if (decoded-time-less-p start-time start) start start-time))
                   (minutes-end (if (decoded-time-less-p end end-time) end end-time))
                   (minutes (/ (nth 1 (time-subtract (apply 'encode-time minutes-end) (apply 'encode-time minutes-start))) 60))
                   (headlines (nth 2 (cadr iter)))
                   (project (assoc headlines project-alist)))
              (if (and project (null dont-sum))
                  (setcdr project (list (+ (cadr project) minutes) minutes-start minutes-end))
                  (setq project-alist (cons (list headlines minutes minutes-start minutes-end) project-alist)))
              (if (decoded-time-less-p end end-time)
                  (setq iter (cdr iter))
                  ;; delete clock that also finishes in this hour:
                  (setcdr iter (cddr iter))) ;; delete clock entry
              ))
          (setq project-alist (nreverse project-alist))
          ;; Compose shedule for hour:
          (while project-alist
            (let ((headlines (caar project-alist))
                  (minutes (nth 1 (car project-alist)))
                  (minutes-start (nth 2 (car project-alist)))
                  (minutes-end (nth 3 (car project-alist))))
              (setq shedule (cons (funcall org-time-sheet-time-formatter minutes-start minutes-end (nth 2 start) minutes headlines) shedule)))
            (setq project-alist (cdr project-alist)))
          ;; calculate new time:
          (when (cdr clocks)
            (let ((next-hour-start-time (decoded-time-advance start '(0 0 1 0 0 0)))
                  (next-hour-end-time (decoded-time-advance start '(0 0 2 0 0 0))))
              (setq start (copy-sequence (caadr clocks)))
              (setf (nth 1 start) 0) ;; minutes
              (when (decoded-time-less-p start next-hour-end-time)
                (setq start next-hour-start-time))
              (let ((new-day-month-year (time-day-month-year start)))
                (unless (equal day-month-year new-day-month-year)
                  (setq shedule (cons (list (apply org-time-sheet-date-formatter new-day-month-year)) shedule)
                        day-month-year new-day-month-year)))))))
      (setq shedule (nreverse shedule))
      (when interactive
        (insert (with-temp-buffer
                  (insert "#+begin_src emacs-lisp\n#+end_src\n")
                  (let ((pt (point)))
                    (org-babel-insert-result shedule)
                    (delete-region (point-min) pt))
                  (buffer-string))))
      shedule)))

(provide 'org-timesheet)
;;; org-timesheet.el ends here
