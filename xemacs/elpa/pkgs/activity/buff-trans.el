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

  (let* (startfn
         (buff-chg-timer nil)
         (timer-gap 10)
         (time-threshold-gap timer-gap)
         (time-start (current-time))
         (timer nil)
         (idle-thresh-hold 5)
         (idle-times nil)
         (currbuf (current-buffer))
         (display-time-spent nil))


    (setf
     @:startfn            nil
     @:buff-chg-timer     nil
     @:timer-gap          10
     @:time-threshold-gap @:timer-gap
     @:time-start         (current-time)
     @:timer              nil
     @:idle-thresh-hold   5
     @:idle-times         nil
     @:currbuf            (current-buffer)
     @:display-time-spent nil)


    (defun notify-buf-chg (fmt &rest args)
      (let ((msg
              (concat
               (current-time-string)
               ": "
               (apply #'format fmt args))))
        (message msg)
        (notify "buffer-chg" msg)))

    (defun get-timer ()
      (interactive)
      (message "Timer %s" timer))

    (defun get-idle-times ()
      (interactive)
      (message "Idle Times %s" idle-times))

    (defun buffer-chg-print-info (&optional msg)
      (interactive)
      (let ((msg (or msg "info"))
            (time-passed
              (-
               (float-time (current-time))
               (float-time time-start))))
        (notify-buf-chg
         "%s: currbuf %s, Idle timer %s, idle times %s, time passed %d"
         msg
         currbuf
         timer
         idle-times
         time-passed)))

    (defun buffer-chg-action ()
      (buffer-chg-print-info "inaction")
      (notify-buf-chg
       "Detected buffer change buffer %s time spend %d"
       (current-buffer)
       display-time-spent))

    (defun add-idle-timer-hook ()
      (let* ((idle-time-internal (current-idle-time))
             (idle-time (if idle-time-internal
                            (float-time idle-time-internal)
                            0)))
        (when (> idle-time idle-thresh-hold)
          (push idle-time idle-times))))

    (defun cancel-detect-buffer-chg-use ()
      (progn
        (when timer
          (cancel-timer timer)
          (setq timer nil))
        (setq idle-times nil)
        ;; (setq currbuf (current-buffer))
        (setq time-start (current-time))))

    (defun detect-buffer-chg-use ()
      (let* ((cumulatibe-idle-time
               (reduce #'+
                       idle-times))
             (time-passed
               (-
                (float-time (current-time))
                (float-time time-start)))
             (time-spent
               (- time-passed cumulatibe-idle-time)))

        (if (and
             (>= time-spent time-threshold-gap)
             (is-run-detect-buffer-chg-use)
             (not (eq currbuf (current-buffer))))
            (progn
              (when timer
                (cancel-timer timer)
                (setq timer nil))
              (setq display-time-spent time-spent)
              (buffer-chg-action)
              (cancel-detect-buffer-chg-use)
              (setq currbuf (current-buffer))
              (notify-buf-chg "detect-buffer-chg-use total stop timer %s" timer))
            (progn
              (buffer-chg-print-info "detect-buffer-chg-use")
              (when timer
                (cancel-timer timer)
                (setq timer
                      (run-with-timer timer-gap
                                      nil
                                      #'detect-buffer-chg-use)))
              (notify-buf-chg "detect-buffer-chg-use reschd timer %s" timer)))))

    (defun is-run-detect-buffer-chg-use ()
      (and
       (not
        (or
         (string-match "^*helm" (buffer-name))
         (minibufferp)))
       (eq
        (current-buffer)
        (window-buffer))))

    (defun run-detect-buffer-chg ()
      (when (is-run-detect-buffer-chg-use)
        (unless (eq currbuf (current-buffer))
          (notify-buf-chg
           "run-detect-buffer-chg: schd timer prev %s curr %s"
           currbuf (current-buffer))
          ;; (setq currbuf (current-buffer))
          (cancel-detect-buffer-chg-use)
          (setq timer
                (run-with-timer timer-gap
                                nil
                                #'detect-buffer-chg-use)))))

    (defun run-detect-buffer-chg-use ()
      (when (is-run-detect-buffer-chg-use)
        (when buff-chg-timer (cancel-timer buff-chg-timer))
        (setq
         buff-chg-timer (run-with-idle-timer 1 nil #'run-detect-buffer-chg))))

    (setq
     startfn #'run-detect-buffer-chg-use)

    (defun enable-detect-buffer-chg-use ()
      (interactive)
      (cancel-detect-buffer-chg-use)
      (when buff-chg-timer
        (cancel-timer buff-chg-timer)
        (setq buff-chg-timer nil))
      (add-hook 'post-command-hook           #'add-idle-timer-hook)
      (add-hook 'buffer-list-update-hook     startfn)
      (add-hook 'elscreen-screen-update-hook startfn)
      (add-hook 'elscreen-goto-hook          startfn))

    (defun disable-detect-buffer-chg-use ()
      (interactive)
      (cancel-detect-buffer-chg-use)
      (when buff-chg-timer
        (cancel-timer buff-chg-timer)
        (setq buff-chg-timer nil))
      (remove-hook 'post-command-hook           #'add-idle-timer-hook)
      (remove-hook 'buffer-list-update-hook     startfn)
      (remove-hook 'elscreen-screen-update-hook startfn)
      (remove-hook 'elscreen-goto-hook          startfn))

    (enable-detect-buffer-chg-use)))


  ;; (def@ @@ :dispatch (&optional note)
  ;;   (@:initialize))

  ;; (@:dispatch note)
































(let* (startfn
       (timer-gap 10)
       (time-threshold-gap timer-gap)
       (time-start (current-time))
       (timer nil)
       (idle-thresh-hold 5)
       (idle-times nil)
       (currbuf-detect-buffer-chg-use (current-buffer))
       (currbuf-run-detect-buffer-chg (current-buffer))
       (display-time-spent nil))

  (defun ptrace (&optional msg)
    (let ((msg (or msg "ptrace"))
          (trace (with-temp-buffer
                     (backtrace)
                   (buffer-string))))
      (message "%s: %s" msg trace)))

  (defun notify-buf-chg (fmt &rest args)
    (let ((msg
            (concat
             (current-time-string)
             ": "
             (apply #'format fmt args))))
      (message msg)))
  ;; (notify "buffer-chg" msg)


  (defun get-timer ()
    (interactive)
    (message "Timer %s" timer))

  (defun get-idle-times ()
    (interactive)
    (message "Idle Times %s" idle-times))

  (defun buffer-chg-print-info (&optional msg)
    (interactive)
    (let ((msg (or msg "info"))
          (time-passed
            (-
             (float-time (current-time))
             (float-time time-start))))
      (when nil
        (notify-buf-chg
         "%s: prev currbuf-detect-buffer-chg-use %s, currbuf-run-detect-buffer-chg currbuf %s, (current-buffer) %s, (window-buffer) %s, idle-times %s, time-passed %d, Idle timer %s"
         msg
         currbuf-detect-buffer-chg-use
         currbuf-run-detect-buffer-chg
         (current-buffer)
         (window-buffer)
         idle-times
         time-passed
         (if timer t)))))
      

  (defun buffer-chg-action (prevbuf currbuf)
    (buffer-chg-print-info "inaction")
    (notify-buf-chg
     "Detected buffer change buffer %s prevbuf %s currbuf %s time spend %d"
     (current-buffer)
     prevbuf
     currbuf
     display-time-spent))

  (defun add-idle-timer-hook ()
    (let* ((idle-time-internal (current-idle-time))
           (idle-time (if idle-time-internal
                          (float-time idle-time-internal)
                          0)))
      (when (> idle-time idle-thresh-hold)
        (push idle-time idle-times))))

  (defun cancel-detect-buffer-chg-use ()
    (progn
      (when timer
        (cancel-timer timer)
        (setq timer nil))
      (setq idle-times nil)
      (setq time-start (current-time))))

  (defun detect-buffer-chg-use (buffs)
    ;; (message "detect-buffer-chg-use: %s" buffs)
    (let* ((prev (car buffs))
           (curr (cadr buffs))
           (cumulatibe-idle-time
             (reduce #'+ idle-times))
           (time-passed
             (-
              (float-time (current-time))
              (float-time time-start)))
           (time-spent
             (- time-passed cumulatibe-idle-time)))

       (when nil
         (message "detect-buffer-chg-use: (>= time-spent time-threshold-gap) %s" (>= time-spent time-threshold-gap))
         (message "detect-buffer-chg-use: (is-run-detect-buffer-chg-use) %s" (is-run-detect-buffer-chg-use))
         (message "detect-buffer-chg-use: (not (eq currbuf-detect-buffer-chg-use (current-buffer))) %s" (not (eq currbuf-detect-buffer-chg-use (current-buffer)))))

      (if (and
           (>= time-spent time-threshold-gap)
           (is-run-detect-buffer-chg-use)
           (not (eq currbuf-detect-buffer-chg-use (current-buffer))))
          (progn
            ;; (when timer
            ;;   (cancel-timer timer)
            ;;   (setq timer nil))
            (cancel-detect-buffer-chg-use)
            (setq display-time-spent time-spent)
            (buffer-chg-action prev curr)
            (cancel-detect-buffer-chg-use)
            (setq currbuf-detect-buffer-chg-use (current-buffer))
            (buffer-chg-print-info "detect-buffer-chg-use total stop timer"))
          (progn
            (buffer-chg-print-info "detect-buffer-chg-use: else ")
            (when timer
              (cancel-timer timer)
              (setq timer
                    (run-with-timer timer-gap
                                    nil
                                    #'detect-buffer-chg-use buffs)))
            (buffer-chg-print-info "detect-buffer-chg-use reschd timer")))))

  (defun is-run-detect-buffer-chg-use ()
    (and
     (not
      (or
       ;; (current-idle-time)
       (string-match "^*helm" (buffer-name))
       (string-match "^*Minibuf-" (buffer-name))
       (minibufferp)))
     (eq
      (current-buffer)
      (window-buffer))))
  ;; (current-idle-time)

  (defun run-detect-buffer-chg (prev curr)
    (buffer-chg-print-info "run-detect-buffer-chg1")
    (if (and
         (is-run-detect-buffer-chg-use)
         (not (eq currbuf-run-detect-buffer-chg (current-buffer)))
         (not (eq currbuf-detect-buffer-chg-use (current-buffer))))
        (progn
          (buffer-chg-print-info "run-detect-buffer-chg")
          (cancel-detect-buffer-chg-use)
          (setq timer
                (run-with-timer timer-gap
                                nil
                                #'detect-buffer-chg-use (list currbuf-run-detect-buffer-chg curr))))
        (setq currbuf-run-detect-buffer-chg curr)
        (when (eq currbuf-detect-buffer-chg-use (current-buffer))
          ;; (message "cancel timer")
          (cancel-detect-buffer-chg-use))))

  (defun enable-detect-buffer-chg-use ()
    (interactive)
    (cancel-detect-buffer-chg-use)


    (add-hook 'post-command-hook #'add-idle-timer-hook)
    (add-hook 'switch-buffer-functions #'run-detect-buffer-chg))


  (defun disable-detect-buffer-chg-use ()
    (interactive)
    (cancel-detect-buffer-chg-use)

    (remove-hook 'post-command-hook #'add-idle-timer-hook)
    (remove-hook 'switch-buffer-functions #'run-detect-buffer-chg))

  (enable-detect-buffer-chg-use))

(disable-detect-buffer-chg-use)

;;; buff-trans.el ends here
