;;; d-meal-timer.el --- A meal timer count-down system that improves over appt.el

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

;;; Commentary:

;; Have you ever sat programmming on your computer only to realise
;; that the cooking on your stove top has been burnt for want of lack
;; of care of your cooking.  For those of you who are out there,
;; this system is for you.  You simply execute M-x meal-timer
;; and the minibuffer prompts you for a time to elapse before a
;; sound sample is played.  While the timer is counting down, the
;; number of hours, minutes and seconds remaining is continuously
;; updated on the mode line.  This is a superior replacement from
;; the built-in appt.el system.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-meal-timer

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-meal-timer)

;;; Known Bugs:

;; none!

(setq meal-timer-on nil)

;;(defvar meal-timer--file "c:/music/already-burnt-8GB/B52s--Love-Shack.wma")
;;(defvar meal-timer--file "c:/music/mc-hammer--keep.wma")
;;(defvar meal-timer--file "c:/music/midi-150MB/Midi-Popular/B52s/Love-Shack.mid")
;;(defvar meal-timer--file "~/3-libd/2007/R4/music/Africa.mid")

;; (seconds-to-readable-string 123 nil t)
;; (seconds-to-readable-string 12.3 nil t)
(defun meal-timer (minutes)
  (interactive "nEnter Meal Timer (minutes): ")

  (if meal-timer-on
      (cancel-timer meal-timer))

  (if (< minutes 0)
      (error "You cannot enter a negative time"))

  ;;(message "minutes=%s" minutes)
  (setq meal-timer--on t)
  (setq meal-timer--beep-time (current-time))
  (setq meal-timer--duration (seconds-to-readable-string (* 60 minutes) nil t))

  (let (hi lo seconds)

    (setq hi (car meal-timer--beep-time))
    (setq lo (cadr meal-timer--beep-time))
    (setq seconds (* 60.0 minutes))
    (setq lo (+ lo seconds))

    (while (>= lo 65536)
      (setq lo (- lo 65535))
      (setq hi (+ hi 1)))

    (assert (< lo 65536))
    (assert (< hi 65536))

    (setcar meal-timer--beep-time hi)
    (setcar (cdr meal-timer--beep-time) lo)

    (setq meal-timer (run-with-timer seconds nil 'meal-timer-function))))

(defun meal-timer-function ()
  ;;(d-nbeeps 5 "Time is up!")
  ;; NOTE: Better replacement for sit-for
  (setq meal-timer--on nil)
  ;;(d-random-play-emacs-midi)
  (let (f)
    (setq f "completed-meal-timer.wav")
    (if (file-exists-p f)
        (play-sound (list 'sound :file f :volume .99)))
    (setq f "c:/sound-samples/emacs/completed-meal-timer.wav")
    (if (file-exists-p f)
        (play-sound (list 'sound :file f :volume .99))))
  (message "Meal timer expired at %s elasped=%s" (d-time--get-stamp) meal-timer--duration)
  )

;;(setq minor-mode-alist (cons '(meal-timer--on " MEAL-TIMER") minor-mode-alist))

(setq minor-mode-alist (cons '(meal-timer--on (:eval (meal-timer-get-string))) minor-mode-alist))

;; (setq s "123 ")
;; (substring s 0 -1)
(defun meal-timer-get-string ()
  (let* ((count (seconds-of-time-difference (current-time) meal-timer--beep-time))
         (count (floor count))
         (str (seconds-to-readable-string count nil t)))
    (format " Timer=(%s/%s)" str meal-timer--duration))
  )

(run-with-timer 1 1 'force-mode-line-update)

;;;
;;; TODO: not needing to add an timer function ...
;;;
;;; minor-mode-alist
;;; mode-line-format's
;;;
;;;

(provide 'd-meal-timer)

 
