;;; rsi-brk.el --- Periodic R.S.I. typing breakes

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Current Function method
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

;; http://davin.50webs.com/research/2010/mopa2e2.html#rsi-brk

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'rsi-brk)

;;; Known Bugs:

;; None so far!

(defvar rsi-period (* 5 60)
  "The time (in seconds) between the mandatory rsi-breaks.")

(defvar rsi-break  10
  "The time (in seconds) of the mandatory rsi-breaks.")

(defvar rsi-last-break nil
  "String containing the time of the last rsi-break.")

(defun rsi-start-timer ()
  "Starts the rsi-break system in operation.  Cannot call this twice without errors."
  (if (boundp 'rsi-timer)
      (cancel-timer rsi-timer))
  (setq rsi-timer (run-with-timer rsi-period rsi-period 'rsi-timer-function))
  (setq rsi-last-plugged-in (current-time-string))
  (add-hook 'post-command-hook 'rsi-increment-keycount))

(defun rsi-stop-timer ()
  "Stops the rsi-break system."
  (cancel-timer rsi-timer)
  (remove-hook 'post-command-hook 'rsi-increment-keycount))



;; (rsi-timer-function)
;; (setq rsi-keystroke-count 0)
(defun rsi-timer-function ()
  (interactive)
  (let ((timer-idle-list nil)
        (keys-per-minute (/ rsi-keystroke-count (/ rsi-period 60.0))))
    (let ((message-log-max nil)) 
      (setq rsi-last-break (current-time))
      ;;(redraw-display)
      (when t;;(> keys-per-minute 30)
        (let ((i rsi-break))
          (while (> i 0)
            ;;(if (< (- rsi-break 2) i)
            (let ((visible-bell t))
              (ding))
            (let ((j 8))
              (while (> j 0)
                (message (format (concat
                                  "%s %3d"
                                  "          "
                                  "%5d  keys-per-minute"
                                  "          "
                                  "%3d %s")
                                 (cond ((= 0 (mod j 2))
                                        "TYPING BREAK...")
                                       (t
                                        "               "))
                                 i keys-per-minute i
                                 (cond ((= 0 (mod j 2))
                                        "...TYPING BREAK")
                                       (t
                                        "               "))))
                (sleep-for 0.1)
                (setq j (1- j))))
            (setq i (1- i)))
          (message "")
          (setq rsi-keystroke-count 0))))
    (message "Last rsi-brk commencing at %s keys-per-minute=%d" 
             (d-time--decode-time-readable rsi-last-break)
             keys-per-minute))
  )

(setq rsi-keystroke-count 0)
(defun rsi-increment-keycount ()
  (setq rsi-keystroke-count
        (+ rsi-keystroke-count (length (this-command-keys)))))

(if (not noninteractive)
    (rsi-start-timer))

(provide 'rsi-brk)

 
