

(in-package :stumpwm)

;; Turn off initially
(run-shell-command "synclient TouchpadOff=1")
(message "Loaded.... 1")

;; Get rid of cursor initially
; (banish-pointer)
(message "Loaded.... 2")

;; (if (init-mpd-connection)
;;     (message "MPD connected!!"))
    ;;(mpd-send-command "play"))

(message "Loaded.... 3")

;; (progn
;;   (when (find-package :pa-fnstumpwm) ;; (or t (usepa))
;;     (pa-fnstumpwm::initpa)
;;     (pa-fnstumpwm::select-plan-task)))

#+pa
(ignore-errors ;if this fails, don't enter debugger
  (pa-fnstumpwm::initpa)
  (pa-fnstumpwm::select-plan-task))

(if (scratchpad)
    (dotimes (c 2) (hsplit)))

;; (let ((sg (find-group (current-screen) ".scratchpad"))) **
;;   (if sg (switch-to-group sg))) **

(sleep 2)

(start-wm-components)

(sleep 2)


(cd
 (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
   (if (probe-file paradise)
       (probe-file paradise)
       (probe-file (getenv "HOME")))))

(set-profile :myprofile)
