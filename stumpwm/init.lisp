;; -*-lisp-*-

(in-package :stumpwm)

 ;;Debugging
;; (setq *debug-level* 10)
(setq *debug-level* 10)

;;{{{ Basic files loading
(load (concat *initdir* "/basic.lisp"))
(load (concat *initdir* "/macros.lisp"))
;;}}}



;;(add-hook *destroy-window-hook* 'kill-empty-group)

;;{{{ Computer specific setup

;; setup may depend on computer since I share this rc file between
;; multiple machines here we load machine specific setup
(let* ((setup-dir (concat ".stumpwm.d/" *hostname* "/"))
       (setup-file (concat setup-dir  "setup.lisp")))
  (when (probe-file setup-file)
    (setf *data-dir* (parse-namestring setup-dir))
    (load setup-file)))
;; Load all sub files
(sharad/load-dir-files *session-dir*)
(sharad/load-dir-files (concat *session-dir* "contrib"))
;;}}}


;;{{{ Basic files loading
(load (concat *initdir* "/run.lisp"))
;;}}}


;; (setq *debug-level* 0)



