
(defun idle-prints (print)
  (defvar *test-timer* nil)
  (defvar emacs-idle-times-list nil)
  (if print
      (progn
        (if *test-timer* (cancel-timer *test-timer*))
        (when t
          (setq
           *test-timer*
           (run-with-timer 1 2
                           '(lambda ()
                              ;; (message "Test: From timer idle for org %d secs emacs %d secs" (org-emacs-idle-seconds) (float-time (current-idle-time)))
                              (let* ((idle (float-time (current-idle-time)))
                                    (idle (or idle 0)))
                               (message "Test: From timer idle for secs emacs %d secs" idle))
                             ;; (push (org-emacs-idle-seconds) emacs-idle-times-list)
                             )))))
      (cancel-timer *test-timer*)))

(idle-prints t)
(idle-prints nil)


(message "emacd-idle-times-list %s" emacs-idle-times-list)

(when nil
  (defmacro timer-debug (debug)
    `(defun timer-event-handler (timer)
       "Call the handler for the timer TIMER.
This function is called, by name, directly by the C code."
       (setq timer-event-last-2 timer-event-last-1)
       (setq timer-event-last-1 timer-event-last)
       (setq timer-event-last timer)
       (let ((inhibit-quit t))
         (timer--check timer)
         (let ((retrigger nil)
               (cell
                ;; Delete from queue.  Record the cons cell that was used.
                (cancel-timer-internal timer)))
           ;; If `cell' is nil, it means the timer was already canceled, so we
           ;; shouldn't be running it at all.  This can happen for example with the
           ;; following scenario (bug#17392):
           ;; - we run timers, starting with A (and remembering the rest as (B C)).
           ;; - A runs and a does a sit-for.
           ;; - during sit-for we run timer D which cancels timer B.
           ;; - timer A finally finishes, so we move on to timers B and C.
           (when cell
             ;; Re-schedule if requested.
             (if (timer--repeat-delay timer)
                 (if (timer--idle-delay timer)
                     (timer-activate-when-idle timer nil cell)
                     (timer-inc-time timer (timer--repeat-delay timer) 0)
                     ;; If real time has jumped forward,
                     ;; perhaps because Emacs was suspended for a long time,
                     ;; limit how many times things get repeated.
                     (if (and (numberp timer-max-repeats)
                              (< 0 (timer-until timer (current-time))))
                         (let ((repeats (/ (timer-until timer (current-time))
                                           (timer--repeat-delay timer))))
                           (if (> repeats timer-max-repeats)
                               (timer-inc-time timer (* (timer--repeat-delay timer)
                                                        repeats)))))
                     ;; Place it back on the timer-list before running
                     ;; timer--function, so it can cancel-timer itself.
                     (timer-activate timer t cell)
                     (setq retrigger t)))


             (let ((old-idle-time (org-emacs-idle-seconds))
                   (new-idle-time 0))


               ;; Block start
               ;; Run handler.
               (condition-case-unless-debug err
                   ;; Timer functions should not change the current buffer.
                   ;; If they do, all kinds of nasty surprises can happen,
                   ;; and it can be hellish to track down their source.
                   (save-current-buffer
                     ;; (message "running fun %s with %s" (timer--function timer) (timer--args timer))
                     (apply (timer--function timer) (timer--args timer)))
                 (error (message "Error running timer%s: %S"
                                 (if (symbolp (timer--function timer))
                                     (format " `%s'" (timer--function timer)) "")
                                 err)))
               ;; Block finish


               (setq new-idle-time (org-emacs-idle-seconds))
               (if (and
                    (/= 0 old-idle-time)
                    (> old-idle-time new-idle-time))
                   (message "timer-event-handler: culprit fun %s arg %s"
                            (timer--function timer) (timer--args timer))
                   (when ,debug
                     (message "%f timer-event-handler: culprit fun %s arg %s"
                              old-idle-time
                              (timer--function timer) (timer--args timer)))
                   ;; (when nil
                   ;;  (message "old %f new %f timer-event-handler: culprit fun %s arg %s"
                   ;;          old-idle-time new-idle-time
                   ;;          (timer--function timer) (timer--args timer)))
                   ))



             (when (and retrigger
                        ;; If the timer's been canceled, don't "retrigger" it
                        ;; since it might still be in the copy of timer-list kept
                        ;; by keyboard.c:timer_check (bug#14156).
                        (memq timer timer-list))
               (setf (timer--triggered timer) nil)))))))





  org-clock-update-mode-line

  eldoc-mode

  sp-show--pair-function

  hl-paren-highlight

                                        ; org-timer-update-mode-line

  (defun stop-timer (sym)
    (when (boundp sym)
      (when (symbol-value sym)
        (cancel-timer (symbol-value sym)))))

  (defun disable-mode (sym)
    (when (boundp sym)
      (when (and (fboundp sym)
                 (symbol-value sym))
        (funcall (symbol-function sym) nil) )))
  (progn
    (defun disable-timers ()

      (dolist (b (buffer-list))
        (with-current-buffer b
          (progn
            (smartparens-global-mode 0)
            (smartparens-mode 0)
            (show-smartparens-mode 0)
            (stop-timer 'sp-show-pair-idle-timer)

            (eldoc-mode 0)
            (stop-timer 'eldoc-timer)

            (highlight-parentheses-mode 0)

            (global-pabbrev-mode 0)
            (pabbrev-mode 0)
            (stop-timer 'pabbrev-short-idle-timer)

            (stop-timer 'semantic-idle-scheduler-work-timer)
            (stop-timer 'semantic-idle-scheduler-timer)

            (disable-mode 'semantic-idle-scheduler-mode)
            (disable-mode 'global-semantic-idle-scheduler-mode)

            (which-function-mode nil)
            (stop-timer 'which-func-update-timer)

            (disable-mode 'jit-lock-mode)
            (stop-timer 'jit-lock-context-timer)

            (disable-mode 'persp-mode)
            ))))

    (disable-timers)))

;; (timer-debug t)
;; (timer-debug nil)


(when nil
  (defun start-idle-test ()
    (disable-timers)
    (timer-debug t)
    (idle-prints t))


  (defun stop-idle-test ()
    ;; (disable-timers)
    (timer-debug nil)
    (idle-prints nil))


  (start-idle-test)


  (stop-idle-test))
