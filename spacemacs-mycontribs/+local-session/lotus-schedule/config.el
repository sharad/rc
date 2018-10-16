;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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



(defun get-time (prompt)
  "Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be one of: a string giving an absolute time like
\"11:23pm\" (the acceptable formats are those recognized by
`diary-entry-time'; note that such times are interpreted as times
today, even if in the past); a string giving a relative time like
\"2 hours 35 minutes\" (the acceptable formats are those
recognized by `timer-duration'); nil meaning now; a number of
seconds from now; a value from `encode-time'; or t (with non-nil
REPEAT) meaning the next integral multiple of REPEAT.  REPEAT may
be an integer or floating point number.  The action is to call
FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive)

  (let ((time (read-from-minibuffer prompt)))
    ;; Special case: nil means "now" and is useful when repeating.
    (if (null time)
        (setq time (current-time)))

    ;; Special case: t means the next integral multiple of REPEAT.
    (if (and (eq time t) repeat)
        (setq time (timer-next-integral-multiple-of-time (current-time) repeat)))

    ;; Handle numbers as relative times in seconds.
    (if (numberp time)
        (setq time (timer-relative-time (current-time) time)))

    ;; Handle relative times like "2 hours 35 minutes"
    (if (stringp time)
        (let ((secs (timer-duration time)))
          (if secs
              (setq time (timer-relative-time (current-time) secs)))))

    ;; Handle "11:23pm" and the like.  Interpret it as meaning today
    ;; which admittedly is rather stupid if we have passed that time
    ;; already.  (Though only Emacs hackers hack Emacs at that time.)
    (if (stringp time)
        (progn
          (require 'diary-lib)
          (let ((hhmm (diary-entry-time time))
                (now (decode-time)))
            (if (>= hhmm 0)
                (setq time
                      (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
                                   (nth 4 now) (nth 5 now) (nth 8 now)))))))

    (or (consp time)
        (error "Invalid time format"))
    time))

;;; config.el ends here
