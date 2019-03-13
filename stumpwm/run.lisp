
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

(message "Loaded.... pa")

(if (scratchpad)
    (dotimes (c 2) (hsplit)))
(message "Loaded.... scratchpad")

;; (let ((sg (find-group (current-screen) ".scratchpad"))) **
;;   (if sg (switch-to-group sg))) **

(sleep 2)

(start-wm-components)
(message "Loaded.... start-wm-components")

(sleep 2)

(cd
 (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
   (or
    (probe-file paradise)
    (probe-file (getenv "HOME")))))
(message "cd to paradise")

(set-profile :myprofile)
(message "set myprofile")


(vgroups)
(message "vgroups")



;; enable
#+stumptray
(when (fboundp 'stumptray:stumptray)
  (stumptray:stumptray))

#+clipboard-history
(progn
  (define-key *root-map* (kbd "C-y") "show-clipboard-history")
  ;; start the polling timer process
  (clipboard-history:start-clipboard-manager))

