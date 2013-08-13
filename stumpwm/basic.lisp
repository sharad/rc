
(if (find-package :pa-fnstumpwm)
    (push :pa *FEATURES*))

;; (if (find-package :pa-fnstumpwm)
;;    ;; (make-package :pa-fnstumpwm)
;;     (progn
;;       (import 'pa-fnstumpwm::run-cli-command)
;;       (import 'pa-fnstumpwm::run-wcli-command))
;;     (progn
;;       (defcommand run-cli-command (cmd) ((:shell "program: "))
;;         (run-shell-command cmd))
;;       (defcommand run-wcli-command (cmd) ((:shell "program: "))
;;         (run-shell-command cmd))))

;; ;; (defun usepa ()
;; ;;   (string-equal (getenv "STUMPWMPA") "yes"))

;; ;; (if (or t (usepa))
;; ;;     (import 'pa-fnstumpwm::run-wcli-command)
;; ;;     (import 'pa-fnstumpwm::run-cli-command))



#-pa
(defcommand run-cli-command (cmd) ((:shell "program: "))
  (run-shell-command cmd))
#+pa (import 'pa-fnstumpwm::run-cli-command)
#-pa
(defcommand run-wcli-command (cmd) ((:shell "program: "))
        (run-shell-command cmd))
#+pa (import 'pa-fnstumpwm::run-wcli-command)
#-pa
(defun process-pid (process)
  #+sbcl (sb-ext:process-pid process)
  #-sbcl (error 'not-implemented))
#+pa (import 'pa-fnstumpwm::process-pid)



