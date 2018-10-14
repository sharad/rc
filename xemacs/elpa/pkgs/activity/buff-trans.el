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

  (def@ @@ :ptrace (&optional msg)
    (let ((msg (or msg "ptrace"))
          (trace (with-temp-buffer
                     (backtrace)
                   (buffer-string))))
      (message "%s: %s" msg trace)))

  (def@ @@ :notify-buf-chg (fmt &rest args)
    (let ((msg
            (concat
             (current-time-string)
             ": "
             (apply #'format fmt args))))
      (message msg)))

  (def@ @@ :get-timer ()
     (message "Timer %s" @:timer))

  (def@ @@ :get-idle-times ()
    (message "Idle Times %s" idle-times))

  (def@ @@ :buffer-chg-print-info (&optional msg)
    (let ((msg (or msg "info"))
          (time-passed
            (-
             (float-time (current-time))
             (float-time @:time-start))))
      (when @:debug-switch-buf
         (@:notify-buf-chg
          "%s: prev currbuf-detect-buffer-chg-use %s, currbuf-run-detect-buffer-chg currbuf %s, (current-buffer) %s, (window-buffer) %s, idle-times %s, time-passed %d, Idle timer %s"
          msg
          @:currbuf-detect-buffer-chg-use
          @:currbuf-run-detect-buffer-chg
          (current-buffer)
          (window-buffer)
          @:idle-times
          time-passed
          (if @:timer t)))))

  (def@ @@ :buffer-chg-action (prevbuf currbuf time-spent)
     (@:buffer-chg-print-info "inaction")
     (@:notify-buf-chg
      "Detected buffer change buffer %s prevbuf %s currbuf %s time spend %d"
      (current-buffer)
      prevbuf
      currbuf
      time-spent))

  (def@ @@ :add-idle-timer-hook ()
    (let* ((idle-time-internal (current-idle-time))
           (idle-time (if idle-time-internal
                          (float-time idle-time-internal)
                          0)))
      (when (> idle-time @:idle-thresh-hold)
        (push idle-time @:idle-times))))

  (def@ @@ :cancel-detect-buffer-chg-use ()
    (progn
      (when @:timer
        (cancel-timer @:timer)
        (setf @:timer nil))
      (setf @:idle-times nil)
      (setf @:time-start (current-time))))

  (def@ @@ :detect-buffer-chg-use (prev curr)
    (let* ((cumulatibe-idle-time (reduce #'+ @:idle-times))
           (time-passed
             (-
              (float-time (current-time))
              (float-time @:time-start)))
           (time-spent
             (- time-passed cumulatibe-idle-time)))

      (when @:debug-switch-buf
        (message "detect-buffer-chg-use: (>= time-spent time-threshold-gap) %s" (>= time-spent @:time-threshold-gap))
        (message "detect-buffer-chg-use: (is-run-detect-buffer-chg-use) %s" (@:is-run-detect-buffer-chg-use))
        (message "detect-buffer-chg-use: (not (eq currbuf-detect-buffer-chg-use (current-buffer))) %s" (not (eq @:currbuf-detect-buffer-chg-use (current-buffer)))))

      (if (and
           (>= time-spent @:time-threshold-gap)
           (@:is-run-detect-buffer-chg-use)
           (not (eq @:currbuf-detect-buffer-chg-use (current-buffer))))
          (progn
            (@:cancel-detect-buffer-chg-use)
            (@:buffer-chg-action prev curr time-spent)
            (@:cancel-detect-buffer-chg-use)
            (setf @:currbuf-detect-buffer-chg-use curr)
            (@:buffer-chg-print-info "detect-buffer-chg-use total stop timer"))
          (progn
            (@:buffer-chg-print-info "detect-buffer-chg-use: else ")
            (when @:timer
              (cancel-timer @:timer)
              (setf @:timer
                    (run-with-timer @:timer-gap
                                    nil
                                    @:detect-buffer-chg-use @@ prev curr))) ;todo
            (@:buffer-chg-print-info "detect-buffer-chg-use reschd timer")))))

  (def@ @@ :is-run-detect-buffer-chg-use ()
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

  (def@ @@ :run-detect-buffer-chg (prev curr)
    (@:buffer-chg-print-info "run-detect-buffer-chg1")
    (if (and
         (@:is-run-detect-buffer-chg-use)
         (not (eq @:currbuf-run-detect-buffer-chg (current-buffer)))
         (not (eq @:currbuf-detect-buffer-chg-use (current-buffer))))
        (progn
          (@:buffer-chg-print-info "run-detect-buffer-chg")
          (@:cancel-detect-buffer-chg-use)
          (setf @:timer
                (run-with-timer @:timer-gap
                                nil
                                @:detect-buffer-chg-use @@ @:currbuf-run-detect-buffer-chg curr))
          (setf @:currbuf-run-detect-buffer-chg curr))
        (when (eq @:currbuf-detect-buffer-chg-use (current-buffer))
          (when @:debug-switch-buf (message "cancel timer"))
          (@:cancel-detect-buffer-chg-use))))

  (def@ @@ :enable-detect-buffer-chg-use ()
    (@:cancel-detect-buffer-chg-use)
    (add-hook 'post-command-hook #'(lambda () (@:add-idle-timer-hook)))
    (add-hook 'switch-buffer-functions #'(lambda (prev curr) (@:run-detect-buffer-chg prev curr))))

  (def@ @@ :disable-detect-buffer-chg-use ()
    (@:cancel-detect-buffer-chg-use)
    (remove-hook 'post-command-hook #'(lambda () (@:add-idle-timer-hook)))
    (remove-hook 'switch-buffer-functions #'(lambda (prev curr) (@:run-detect-buffer-chg prev curr))))

  (def@ @@ :initialize ()
    (setf @:debug-switch-buf t)
    (setf @:timer-gap 10)
    (setf @:time-threshold-gap @:timer-gap)
    (setf @:idle-thresh-hold 5)

    (setf @:time-start (current-time))
    (setf @:timer nil)
    (setf @:idle-times nil)
    (setf @:currbuf-detect-buffer-chg-use (current-buffer))
    (setf @:currbuf-run-detect-buffer-chg (current-buffer))
    (@:enable-detect-buffer-chg-use)
    (@:buffer-chg-print-info "run-detect-buffer-chg1")
    t)

  (def@ @@ :uninitialize ()
    (@:disable-detect-buffer-chg-use)
    t))



;; (@:enable-detect-buffer-chg-use)

(when nil
  (progn
   (setf @buff-trans (@! @transition-span-dectector-class :gen-buffer-trans "test"))
   (@! @buff-trans :initialize))

  (@! @buff-trans :uninitialize)

  (length switch-buffer-functions)
  (setq switch-buffer-functions nil)


 ;; switch-buffer-functions


 (@ @buff-trans :timer-gap)
 (functionp (@ @buff-trans :detect-buffer-chg-use)))

(car switch-buffer-functions)

(add-hook 'switch-buffer-functions
          #'(lambda (prev curr)
              (message "prev %s, curr %s" prev curr)))

(defun enable-detect-buffer-chg-use ()
  (@:cancel-detect-buffer-chg-use)
  (add-hook 'post-command-hook #'add-idle-timer-hook)
  (add-hook 'switch-buffer-functions #'run-detect-buffer-chg))


(defun disable-detect-buffer-chg-use ()
  (@:cancel-detect-buffer-chg-use)
  (remove-hook 'post-command-hook #'add-idle-timer-hook)
  (remove-hook 'switch-buffer-functions #'run-detect-buffer-chg))

;; (def@ @@ :dispatch (&optional note)
;;   (@:initialize))

;; (@:dispatch note)

;; (macroexpand-1
;;  '(def@ @@ :notify-buf-chg (fmt &rest args) (apply #'message fmt args)))





















(let* ((debug-switch-buf t)
       (timer-gap 10)
       (time-threshold-gap timer-gap)
       (time-start (current-time))
       (timer nil)
       (idle-thresh-hold 5)
       (idle-times nil)
       (currbuf-detect-buffer-chg-use (current-buffer))
       (currbuf-run-detect-buffer-chg (current-buffer)))

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
      (when debug-switch-buf
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


  (defun buffer-chg-action (prevbuf currbuf time-spent)
    (buffer-chg-print-info "inaction")
    (notify-buf-chg
     "Detected buffer change buffer %s prevbuf %s currbuf %s time spend %d"
     (current-buffer)
     prevbuf
     currbuf
     time-spent))

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

  (defun detect-buffer-chg-use (prev curr)
    (let* ((cumulatibe-idle-time (reduce #'+ idle-times))
           (time-passed
             (-
              (float-time (current-time))
              (float-time time-start)))
           (time-spent
             (- time-passed cumulatibe-idle-time)))

      (when debug-switch-buf
         (message "detect-buffer-chg-use: (>= time-spent time-threshold-gap) %s" (>= time-spent time-threshold-gap))
         (message "detect-buffer-chg-use: (is-run-detect-buffer-chg-use) %s" (is-run-detect-buffer-chg-use))
         (message "detect-buffer-chg-use: (not (eq currbuf-detect-buffer-chg-use (current-buffer))) %s" (not (eq currbuf-detect-buffer-chg-use (current-buffer)))))

      (if (and
           (>= time-spent time-threshold-gap)
           (is-run-detect-buffer-chg-use)
           (not (eq currbuf-detect-buffer-chg-use (current-buffer))))
          (progn
            (cancel-detect-buffer-chg-use)
            (buffer-chg-action prev curr time-spent)
            (cancel-detect-buffer-chg-use)
            (setq currbuf-detect-buffer-chg-use curr)
            (buffer-chg-print-info "detect-buffer-chg-use total stop timer"))
          (progn
            (buffer-chg-print-info "detect-buffer-chg-use: else ")
            (when timer
              (cancel-timer timer)
              (setq timer
                    (run-with-timer timer-gap
                                    nil
                                    #'detect-buffer-chg-use prev curr)))
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
                                #'detect-buffer-chg-use currbuf-run-detect-buffer-chg curr))
          (setq currbuf-run-detect-buffer-chg curr))
        (when (eq currbuf-detect-buffer-chg-use (current-buffer))
          (when debug-switch-buf (message "cancel timer"))
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
