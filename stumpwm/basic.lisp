
(in-package :stumpwm)


;;{{{
(defvar *interactive-debug* 0 "Interactive debug.")
(defparameter *debug-wait* 0)
(defun debug-sleep ()
  (when (> *debug-wait* 0)
    (sleep *debug-wait*)))

(defun show-dbg (msg &key (prompt "test:"))
  (if (> *interactive-debug* 0)
      (read-one-line (current-screen) prompt :initial-input (or msg "NOTHING"))))
;;}}}

(defparameter *debug-level* 10)
(redirect-all-output (data-dir-file "debug-output" "log"))

;;WARNING: this is specific to clisp
;;{{{ Parameters
(defparameter *hostname* (or (getenv "HOSTNAME") (getenv "HOST") (sb-unix::unix-gethostname)))
(defparameter *home-dir* (getenv "HOME"))
(defparameter *login-user* (getenv "USER"))
(defparameter *initdir* (concat *home-dir* "/.stumpwm/"))
(defparameter *session-dir* (concat *initdir* "/session-start/"))
; (defparameter *data-dir* (concat *home-dir* "/.stumpwm.d/"))
(defparameter *stumpish* "/usr/local/share/stumpwm/contrib/stumpish")
;; (defparameter *desktop-background* nil)
;;}}}

;;{{{ Load all subfiles
(defun sharad/load-dir-files (dir)
  (dolist (file (directory (concat dir "*.lisp")))
    (debug-sleep)
    (message "loading ~a" file)
    (load file)
    (message "Loaded ~a" file)))
;;}}}


(if (find-package :pa-fnstumpwm)
    (push :pa *FEATURES*))

(load (concat  *session-dir* "/modules.lisp"))

;; #-pa
;; (defcommand run-cli-command (cmd) ((:shell "program: "))
;;   (run-shell-command cmd))
;; #+pa (import 'pa-fnstumpwm::run-cli-command)
;; #-pa
;; (defcommand run-wcli-command (cmd) ((:shell "program: "))
;;         (run-shell-command cmd))
;; #+pa (import 'pa-fnstumpwm::run-wcli-command)
;; #-pa
;; (defun process-pid (process)
;;   #+sbcl (sb-ext:process-pid process)
;;   #-sbcl (error 'not-implemented))
;; #+pa (import 'pa-fnstumpwm::process-pid)
