;;; buff-trans.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d@gmail.com>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(require 'activity-base)

(provide 'buff-trans)

(defsubclass-gen@ @transition-span-dectector-class :gen-buffer-trans (&optional note)
  (setf @:timer nil)
  (def@ @@ :make-transition ()
    "Make buffer change transition."
    (let ((curr (current-buffer)))
      (message "running :make-transition")
      (unless (eql
               @:prev
               curr)
        (@! (@! @:tran :new) :send @:prev curr)
        (setf @:prev curr))))

  (def@ @@ :schedule-transition ()
    (let ((curr (current-buffer)))
      (message "running :make-transition")
      (unless (eql
               @:prev
               curr)
        (cancel @:timer)
        (setf @:timer
              (run-with-timer @:minimum-time-span
                              (lambda ()
                                (funcall @:make-transition @@)))))))

  (def@ @@ :activate ()
    (let ((schedule-transition
            (lambda ()
              (message "running make-transition")
              (@! @@ :schedule-transition))))
      (when nil
        ;; (add-hook 'buffer-list-update-hook     schedule-transition)
        (add-hook 'elscreen-screen-update-hook schedule-transition)
        (add-hook 'elscreen-goto-hook          schedule-transition))))

  (def@ @@ :deactivate ()
    )

  (def@ @@ :initialize ()
    (@:reinitialize)
    (setf @:tran
          (defsubobj@ @transition-class "buffer transition"
            (def@ @@ :send (prev next)
              (@! @:note :send "switched from buffer %s to %s on %s"
                  prev next (@:occuredon)))

            (def@ @@ :dispatch (&optional note)
              (@:init)  ;; BUG what is the issue.
              (setf @:note note
                    ;; (or note
                    ;;     (@! @note-class :gen-format-msg "test"))
                    ))

            (@:dispatch note)))
    (@:activate))

  (def@ @@ :reinitialize ()
    (setf
     @:minimum-time-span 10)
    (setf
     @:prev (current-buffer)
     @:start (current-time)))

  (def@ @@ :dispatch (&optional note)
    (@:initialize))

  (@:dispatch note))


(let* ((timer-gap 10)
       (time-threshold-gap timer-gap)
       (time-start (current-time))
       (timer nil)
       (idle-thresh-hold 5)
       (idle-times nil)
       (currbuf (current-buffer))
       (display-time-passed nil))

  (defun buffer-chg-action ()
    (message
     "Detected buffer change buffer %s time spend "
     (current-buffer)
     display-time-passed))

  (defun add-idle-timer-hook ()
    (let ((idle-time (or (current-idle-time) 0)))
      (when (> idle-time idle-thresh-hold)
        (push idle-time idle-times))))

  (defun detect-buffer-chg-use ()
    (let* ((cumulatibe-idle-time
             (reduce #'+
                     idle-times))
           (time-passed
             (-
              (float-time (current-time))
              (float-time time-start)))
           (time-passed
             (- time-passed cumulatibe-idle-time)))

      (if (- time-passed time-threshold-gap)
          (progn
            (setq idle-times nil)
            (setq time-start (current-time))
            (when timer
              (cancel-timer timer)
              (setq timer nil))
            (setq display-time-passed time-passed)
            (buffer-chg-action))
          (progn
            (when timer
              (cancel-timer timer)
              (setq timer
                    (run-with-timer timer-gap
                                    nil
                                    #'detect-buffer-chg-use)))))))

  (defun run-detect-buffer-chg-use ()
    (unless (eq currbuf (current-buffer))
      (setq currbuf (current-buffer))
      (when timer
        (cancel-timer timer))
      (setq timer
            (run-with-timer timer-gap
                            nil
                            #'detect-buffer-chg-use))))


  (defun get-timer ()
    (interactive)
    (message "Timer %s" timer))

  (defun get-idle-times ()
    (interactive)
    (message "Idle Times %s" timeridle-times))

  (defun enable-detect-buffer-chg-use ()
    (interactive)
    (when timer
      (cancel-timer timer)
      (setq timer nil))
    (add-hook 'post-command-hook           #'add-idle-timer-hook)
    (add-hook 'buffer-list-update-hook     #'run-detect-buffer-chg-use)
    (add-hook 'elscreen-screen-update-hook #'run-detect-buffer-chg-use)
    (add-hook 'elscreen-goto-hook          #'run-detect-buffer-chg-use))


  (defun disable-detect-buffer-chg-use ()
    (interactive)
    (when timer
      (cancel-timer timer)
      (setq timer nil))
    (remove-hook 'post-command-hook           #'add-idle-timer-hook)
    (remove-hook 'buffer-list-update-hook     #'run-detect-buffer-chg-use)
    (remove-hook 'elscreen-screen-update-hook #'run-detect-buffer-chg-use)
    (remove-hook 'elscreen-goto-hook          #'run-detect-buffer-chg-use))


  (enable-detect-buffer-chg-use))


(disable-detect-buffer-chg-use)


;; post-command-hook

;; ()

;;; buff-trans.el ends here
