;;; d-time.el --- A collection of useful time functions

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Meal time
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-time

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-time)

;;; Known Bugs:

;; none!

(if emacs-dialect--xemacs-p
    (setq display-time-compatible t))

(defun seconds-of-time-difference (old new)
  "Returns the number of seconds that separate two time-measurments,
as returned by the function `current-time'."
  (let ((super-old (+ (* 65536.0 (car old)) (cadr old)))
        (super-new (+ (* 65536.0 (car new)) (cadr new))))
    (- super-new super-old)))

;; (setq value 123)
;; (setq units "minute")
;; (say-number-in-words value units t)
(defun say-number-in-words (value units &optional short)
  (if short
      (if (/= 0 value)
          (concat
           (int-to-string value)
           (concat (substring units 0 1))))
    (if (/= 0 value)
        (concat
         (int-to-string value)
         (if (= 1 value)
             (concat " " units " ")
           (concat " " units "s "))))))

;; (seconds-to-readable-string 123 nil nil)
;; (seconds-to-readable-string 123 nil t)
(defun seconds-to-readable-string (time-in-secs &optional no-seconds short)
  "Converts TIME-IN-SECS to a readable value of years, weeks,
days, hours, minutes, seconds.  Called with x nil nil this
function is the inverse of `timer-duration'. Assumes there are 60
seconds in 1 minute, 60 minutes in 1 hour, 24 hours in 1 day, 7
days in 1 week, 4 weeks in 1 month (this is an approximation) and
12 months in 1 year.  Note: months are not returned because
months (m) conflict with minutes (also m)."
  (let* ((secs-per-hour (* 60 60))
         (secs-per-day   (* 24 secs-per-hour))
         (secs-per-week  (* 7 secs-per-day))
         ;;(secs-per-month (* 4 secs-per-week))
         (secs-per-year  (* 365.25 secs-per-day))

         (years        (floor (/ time-in-secs secs-per-year)))
         (time-in-secs (- time-in-secs (* secs-per-year years)))

         ;;(months       (floor (/ time-in-secs secs-per-month)))
         ;;(time-in-secs (- time-in-secs (* secs-per-month months)))

         (weeks        (floor (/ time-in-secs secs-per-week)))
         (time-in-secs (- time-in-secs (* secs-per-week weeks)))

         (days         (floor (/ time-in-secs secs-per-day)))
         (time-in-secs (- time-in-secs (* secs-per-day days)))

         (hours        (floor (/ time-in-secs secs-per-hour)))
         (time-in-secs (- time-in-secs (* secs-per-hour hours)))

         (minutes      (floor (/ time-in-secs 60)))
         (time-in-secs (- time-in-secs (* 60 minutes)))

         (seconds      (round time-in-secs))

         ;; (setq years 12)
         (string (concat (say-number-in-words years   "year" short)
                         ;;(say-number-in-words months  "month" short)
                         (say-number-in-words weeks   "week" short)
                         (say-number-in-words days    "day" short)
                         (say-number-in-words hours   "hour" short)
                         (say-number-in-words minutes "minute" short)
                         (if (not no-seconds)
                             (say-number-in-words seconds "second" short)))))
    (if (string= "" string)
        "Zero time!"
      (if short string (substring string 0 -1)))))


;;;
;;; DISPLAY THE TIME ON MODELINE:
(condition-case err
    (progn
      (setq display-time-string-forms
            '(year
              "-"
              (format "%02d" (read month))
              "-"
              (format "%02d" (read day))
              " "
              dayname " "
              24-hours ":" minutes))
      (setq display-time-string-forms nil)
;;      (setq display-time-day-and-date t)
;;      (setq display-time-24hr-format t)
      (display-time))
  (error
   (message "Cannot display time %s" (cdr err))))

(require 'timer)

;;; (d-time--get-stamp d-emacs-start-time)
;;; (d-time--get-stamp)
(defun d-time--get-stamp (&optional time)
  (interactive "Senter time string: ")
  ;;(debug)
  ;;(setq time (eval time))
  ;;(message "time=%s" time)
  ;;(message "time=%s" time)
  ;;(debug)
  (let ((time-list (decode-time time)))
    (setq yyyymmdd (format "%04d%02d%02d" (nth 5 time-list) (nth 4 time-list) (nth 3 time-list)))
    (setq hhmmss   (format "%02d%02d%02d" (nth 2 time-list) (nth 1 time-list) (nth 0 time-list)))
    (setq dt-stamp (concat yyyymmdd "-" hhmmss))
    ;;(message "Time stamp=%s" dt-stamp)
    dt-stamp
    ))

;;;
;;; (d-time--decode-time-readable d-emacs-start-time)
;;;
(defun d-time--decode-time-readable (&optional time)
  (interactive)
  (let* ((decoded  (decode-time time))
         (year     (nth 5 decoded))
         (month    (nth 4 decoded))
         (day      (nth 3 decoded))
         (hour     (nth 2 decoded))
         (minute   (nth 1 decoded))
         (second   (nth 0 decoded))
         (yyyymmdd (format "%04d%02d%02d" year month day))
         (hhmmss   (format "%02d%02d%02d" hour minute second)))
    (concat yyyymmdd "-" hhmmss)))

(defun d-time--frame-title ()
  (let (time dow year month day hour minute second)
    (setq time (decode-time (current-time)))
    (setq dow     (aref "SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT" (nth 6 time)))
    (setq year    (nth 5 time))
    (setq month   (nth 4 time))
    (setq day     (nth 3 time))
    (setq hour    (nth 2 time))
    (setq minute  (nth 1 time))
    (setq second  (nth 0 time))
    (setq frame-title-format (format "%02d:%02d:%02d %s %04d-%02d-%02d" hour minute second dow year month day))))
;;(setq frame-title-format (format "%04d-%02d-%02d %02dh%02dm%02ds %s" year month day hour minute second dow))))

;;(byte-compile 'd-frame-title)
;;(compile-defun 'd-frame-title)
;;(ad-compile-function 'd-frame-title)

(run-with-timer 1 1 'd-time--frame-title)

(provide 'd-time)
 
