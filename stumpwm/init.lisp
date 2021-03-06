;; -*-lisp-*-

(in-package :stumpwm)

;; (asdf:clear-source-registry)
;; (push #p"/home/s/hell/.guix-profile/lib/sbcl/" asdf:*central-registry*)
;; (push #p"/home/s/hell/.guix-profile/lib/sbcl/contrib/" asdf:*central-registry*)
;; (asdf:initialize-source-registry)

 ;;Debugging
;; (setq *debug-level* 10)
(setq *debug-level* 10)


(set-prefix-key (kbd "C-quoteleft"))


(defparameter *home-dir* (getenv "HOME"))
(defparameter *initdir* #p"~/.stumpwm/")

;;{{{ Basic files loading
(load #p"~/.stumpwm/asdf.lisp")


(load #p"~/.stumpwm/basic.lisp")
(load #p"~/.stumpwm/macros.lisp")
;;}}}



;;(add-hook *destroy-window-hook* 'kill-empty-group)


;;{{{ Computer specific setup
;; setup may depend on computer since I share this rc file between
;; multiple machines here we load machine specific setup
(let* ((setup-dir (concat "~/.stumpwm.d/" *hostname* "/"))
       (setup-file (concat setup-dir  "setup.lisp")))
  (when (probe-file setup-file)
    (setf *data-dir* (parse-namestring setup-dir))
    (load setup-file)))
;; Load all sub files
(message "loading ~a" *session-dir*)
(debug-sleep)
;; (sharad/load-dir-files *session-dir*)
(sharad/load-dir-files "~/.stumpwm/session-start/")
(message "loaded ~a" *session-dir*)
(debug-sleep)
;; (sharad/load-dir-files (concat *session-dir* "contrib"))
;;}}}


;;{{{ Basic files loading
(load (concat *initdir* "/run.lisp"))
;;}}}

;; (setq *debug-level* 0)
(stumpwm::run-with-timer
 (* 3 60)
 nil
 #'(lambda ()
     (let ((old *debug-level*)
           (new 0))
       (message "Setting debugg level back from ~a to ~a, can set it with set-debug-level command" old new)
       (setf *debug-level* new))))

