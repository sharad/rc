;;; timerfunctions.el, 
; ---Deepak Goel (deego@glue.umd.edu)  11/20/00
; GPL'ed. 9/8/00, as under the GNU's license. 


(defun time-difference-my (timeplus timesub)
  "Gives the time in seconds elaspsed from TIMESUB to TIMEPLUS.
Almost like \(- TIMEPLUS TIMESUB \)."
  (+ (* (expt 2 16) (- (car timeplus) (car timesub)))
     (- (cadr timeplus) (cadr timesub)))
)


(defun tf-run-with-idle-timer
  (secs repeat redosecs redorepeat includeruntime function &rest args) 
  "Similar to run-with-idle-timer, except that provides more options.
Emacs seems to lack this very important thing a user might want. The
user might want emacs to repeat an action every 10 seconds if emacs
remains idle. But run-with-idle-timer, even with the repeat argument,
will do that action exactly once 10 seconds and not 20 or 30
seconds. This tf-run-with-idle-timer takes care of that.

SECS is the number of seconds to wait once emacs has first gone
idle. 

If REDOREPEAT is non-nil, the action is repeated as long emacs remains
idle.  REDOSECS is the number of additional seconds (after the action
has been done) to wait if emacs remains idle before performing the
action again. If INCLUDERUNTIME is non-nil, REDOSECS is the number of
additional seconds to wait after the action has been invoked (not
finished).

If REPEAT is nonnil, the entire cycle is repeated every time emacs
next goes idle.. (as in the default run-with-idle-timer."
  (apply 'run-with-idle-timer 
	 secs repeat 'tf-run-while-idle 
	 redosecs redorepeat includeruntime
	 function args)
  )


(defun tf-run-while-idle (redosecs redorepeat includeruntime
function &rest args)
  "Runs FUNCTION with ARGS and optionally repeats if emacs idle.
Probably is of no use unless used in programs.
 If REDOREPEAT is non-nil, the function is repeated periodically every
REDOSECS as long as emacs remains idle. By default, emacs waits
REDOSECS *after* the function is done executing to repeat. If you want
the execution-time to count towards REDOSECS, make INCLUDERUNTIME
non-nil."
  (if (not includeruntime)
      (progn
	(apply function args)
	(if redorepeat
	    (while (sit-for redosecs)
	      (apply function args))))
    (progn
      (let ((before-time (current-time)))
	(apply function args)
	(if redorepeat
	    (while (sit-for (- redosecs (time-difference-my (current-time)
							    before-time)))
	      (setq before-time (current-time))
	      (apply function args))))))
)


;;;====================================================
;;;TESTS FOLLOW
(defun tf-test-display-time-internal
  ()
  (let ((thisbuffer (buffer-name)))
    (switch-to-buffer-other-window "*scratch*")
    (goto-char (point-max))
    (insert (concat "\n" (format "%S" (cadr (current-time)))))
    (recenter)
    (switch-to-buffer-other-window thisbuffer))
)


(defun tf-test ()
  "Run this and watch..Play around with the options.."
  (interactive)
  (tf-run-with-idle-timer
  1 t 3 t nil 'tf-test-display-time-internal)
)





