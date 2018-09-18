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

(when nil
  (setf @buff-transition-span-detector
        (@! @transition-span-dectector-class :gen-buffer-trans "test")))

(when nil
  (progn
    (defvar idle-start (current-time))
    (defvar idle-detect-timer nil)
    (defun print-last-idle ()
      (let* ((currtime (float-time (current-time)))
             (idle-starttime (float-time idle-start))
             (idle-secs (- currtime idle-starttime)))
        (message "idle for %d secs" idle-secs)))
    (defun idle-set ()
      (message "adding idle-set on read-char")
      (setq idle-start (current-time))
      (add-hook
       'pre-command-hook
       'print-last-idle-start-timer))
    (defun print-last-idle-start-timer (&rest args)
      (interactive)
      (print-last-idle)
      (message "removing idle-set on read-char")
      (remove-hook
       'pre-command-hook
       'print-last-idle-start-timer)
      (when idle-detect-timer
        (cancel-timer idle-detect-timer)
        (setq idle-detect-timer nil))
      (message "starting idle-set timer")
      (setq
       idle-detect-timer
       (run-with-idle-timer
        7 nil
        'idle-set)))

    (print-last-idle-start-timer)))

(when nil
  (progn
    ;; https://emacs.stackexchange.com/questions/32040/setting-and-clearing-an-is-idle-variable-when-going-in-and-out-of-idle-mode

    ;; Even though (current-idle-time) only seems to return a meaningful value
    ;; within the context of an idle handler, I figured out how to use it for my
    ;; purpose ...

    ;; Initialize to idle mode upon emacs startup.
    (defvar my-last-idle t
      "*Last idle time value.")

    (defun my-pre-command-hook ()
      (setq my-last-idle nil))

    (defun my-idle-timer-hook ()
      (setq my-last-idle (current-idle-time)))

    (add-hook 'pre-command-hook 'my-pre-command-hook)
    (run-with-idle-timer 1 t 'my-idle-timer-hook)

    ;; Then, my sigusr1/sigusr2 handler can do this ...

    (when my-last-idle
      ;; Do my signal-handling stuff.
      ;; ... etc. ...
      )))

(when nil
  (defun time-tracker-test ()
    (interactive)
    (lexical-let* ((delay 10)
                   (start (current-time))
                   (active-time 0)
                   (idle-time 0)
                   (later
                    (run-with-timer
                     delay
                     nil
                     (lambda ()
                       (let ((active-time
                              (-
                               (float-time (current-time))
                               (float-time start))))
                         (message
                          "active-time %d, idle-time %d"
                          active-time
                          idle-time))))))
      later))


  (read-event)




  (time-tracker-test))


;;; buff-trans.el ends here
