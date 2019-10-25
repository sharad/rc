
(when nil)

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
      (@:message "%s: %s" msg trace)))

  (defun notify-buf-chg (fmt &rest args)
    (let ((msg
            (concat
             (current-time-string)
             ": "
             (apply #'format fmt args))))
      (@:message msg)))

  (defun get-timer ()
    (interactive)
    (@:message "Timer %s" timer))

  (defun get-idle-times ()
    (interactive)
    (@:message "Idle Times %s" idle-times))

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
         (@:message "detect-buffer-chg-use: (>= time-spent time-threshold-gap) %s" (>= time-spent time-threshold-gap))
         (@:message "detect-buffer-chg-use: (is-run-detect-buffer-chg-use) %s" (is-run-detect-buffer-chg-use))
         (@:message "detect-buffer-chg-use: (not (eq currbuf-detect-buffer-chg-use (current-buffer))) %s" (not (eq currbuf-detect-buffer-chg-use (current-buffer)))))

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
          (when debug-switch-buf (@:message "cancel timer"))
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

  (enable-detect-buffer-chg-use)

  (disable-detect-buffer-chg-use))
