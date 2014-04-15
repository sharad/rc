;; -*-lisp-*-

(in-package :stumpwm)

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

;;{{{ Basic files loading
(load (concat *initdir* "/basic.lisp"))
(load (concat *initdir* "/macros.lisp"))
;;}}}


;;{{{ Load all subfiles
(defun sharad/load-dir-files (dir)
    (dolist (file (directory (concat dir "*.lisp")))
        (load file)
        (message "Loaded ~a" file)))
;; Load all sub files
(sharad/load-dir-files *session-dir*)
(sharad/load-dir-files (concat *session-dir* "contrib"))
;;}}}

(setf *group-format* "%t [%s]")
;; ;;{{{ Load module
(when (probe-file *contrib-dir*)
  (load-module "amixer")
  (load-module "aumix")
  (load-module "battery")
  (load-module "battery-portable")
  ;;(load-module "cpu")
  (load-module "disk")
  (load-module "g15-keysyms")
  (load-module "maildir")
  (load-module "mem")
  (load-module "mpd")
  (load-module "net")
  (load-module "notifications")
  (load-module "productivity")
  #+sbcl (load-module "sbclfix")
  (load-module "surfraw")
  (load-module "wifi")
  (load-module "window-tags")
  ;;(load-module "wmii-like-stumpwmrc")
  )
;;}}}

;;{{{ Notification
(define-key *root-map* (kbd "N") '*notifications-map*)
;;}}}

;;{{{ Color
;; colors
;; 0 = black, 1 = red, 2 = green, 3 = yellow,
;; 4 = blue, 5 = magenta, 6 = cyan, 7 = white
;;}}}

;;{{{ Additional keysyms

(define-keysym #x1008ff31 "XF86AudioPause")
(define-keysym #x1008ff15 "XF86AudioStop")
(define-keysym #x1008ff17 "XF86AudioNext")
(define-keysym #x1008ff16 "XF86AudioPrev")
(define-keysym #x1008ff87 "XF86Video")

;; aumixer
(define-key *top-map* (kbd "XF86AudioLowerVolume")   "amixer-PCM-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")   "amixer-PCM-1+")
(define-key *top-map* (kbd "XF86AudioMute")          "amixer-PCM-toggle")

(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "C-XF86AudioMute")        "amixer-Master-toggle")

(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "amixer-Headphone-1-")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "amixer-Headphone-1+")
(define-key *top-map* (kbd "M-XF86AudioMute")        "amixer-Headphone-toggle")
(define-key *top-map* (kbd "S-XF86AudioMute")        "amixer-sense-toggle")

;;}}}

;;{{{ Customization:  change default parameters

(set-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
(set-frame-outline-width 1)
(set-focus-color "Green")
(set-unfocus-color "White")
(set-bg-color "gray25")
;; (set-bg-color "coral")
(setf
 *frame-indicator-text* " Current frame "
 *message-window-gravity* :top-right
 *new-window-preferred-frame* '(:empty :focused)
 *menu-maximum-height* 50
 *menu-scrolling-step* 1
 *suppress-frame-indicator* nil
 *normal-border-width* 1
 *window-border-style* :thight
 ;; change window numbers
 *frame-number-map* "1234567890"
 *group-number-map* "123456789"
;;Run-or-raise work through multiple screens
 *run-or-raise-all-screens* t
 *run-or-raise-all-groups*  t
 ;; mode-line
 *mode-line-position* :top
 *disk-usage-paths* '("/" "/home" "/usr" "/pacific")
 ;;Debugging
 *debug-level* 10
 in.net.sharad.pa-frontend-stumpwm:*menu-selection-file* (data-dir-file "selections" "dump"))

;;}}}


;;{{{
(defvar *interactive-debug* 0 "Interactive debug.")

(defun show-dbg (msg &key (prompt "test:"))
  (if (> *interactive-debug* 0)
      (read-one-line (current-screen) prompt :initial-input (or msg "NOTHING"))))
;;}}}

(defun run-on-screen (screen cmd props)
  (switch-to-screen screen)
  (run-or-raise cmd props))

(defun kill-empty-group (win)
  (declare (ignore win))
  (unless (group-windows (current-group))
    (gkill)))

;;(add-hook *destroy-window-hook* 'kill-empty-group)

;;{{{ Notification system

(defun msg-notify (fmt args)
  (let ((*executing-stumpwm-command* nil)
        (*message-window-gravity* :center))
    (message-no-timeout fmt args)))

(defcommand notify (msg) ((:rest "Notify: "))
  (msg-notify "~a" msg))
;;}}}

;;{{{ Computer specific setup

;; setup may depend on computer since I share this rc file between
;; multiple machines here we load machine specific setup
(let* ((setup-dir (concat ".stumpwm.d/" *hostname* "/"))
       (setup-file (concat setup-dir  "setup.lisp")))
  (when (probe-file setup-file)
    (setf *data-dir* (parse-namestring setup-dir))
    (load setup-file)))
;;}}}


;; load all file in session dir
;; (dolist (x (directory (concat *session-dir* "*.lisp")))
;;   (load x)
;;   (message "Loaded ~a" x))

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

;; (define-frame-preference ".scratchpad"
;;     (1                                  ;frame-number
;;      t                                  ;raise
;;      t                                  ;lock
;;      t                                  ;create
;;      ;; &keys
;;      nil                                ;restore
;;      "class"                            ;window's class
;;      "instance"                         ;window's instance/resource name
;;      "type"                             ;window's type
;;      "role"                             ;window's role
;;      "title"                            ;window's title
;;      ))

;; (setf *window-placement-rules* nil)

(define-frame-preference
    ;; frame raise lock (lock AND raise == jumpto)
    ".scratchpad"


    ;; (0 t t :class "Empathy" :instance "empathy" :title nil ;"Contact List"
    ;;    :role nil)
    ;; (0 nil t ::title "xterm") ;; test

    (0 nil t :class "Pidgin"  :instance "Pidgin"  :title "Buddy List"   :role "buddy_list")
    (2 t nil :class "Pidgin" :instance "Pidgin" :title  nil ;"FN LN"
     :role "conversation")
    (2 nil t :class "Skype" :instance "skype" :title    nil ;"skypeid - Skype? Beta"
     :role "MainWindow"))




;; (progn
;;   (when (find-package :pa-fnstumpwm) ;; (or t (usepa))
;;     (pa-fnstumpwm::initpa)
;;     (pa-fnstumpwm::select-plan-task)))

#+pa
(progn
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
