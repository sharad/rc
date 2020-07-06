
(in-package :stumpwm)


;;{{{ Color
;; colors
;; 0 = black, 1 = red, 2 = green, 3 = yellow,
;; 4 = blue, 5 = magenta, 6 = cyan, 7 = white
;;}}}

(stumpwm:set-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
(stumpwm:set-frame-outline-width 1)
(stumpwm:set-focus-color "Green")
(stumpwm:set-unfocus-color "White")
(stumpwm:set-bg-color "gray25")
;; (set-bg-color "coral")


;;{{{ Customization:  change default parameters

(setf stumpwm:*frame-indicator-text*       " Current frame "
      stumpwm:*message-window-gravity*     :top-right
      stumpwm:*new-window-preferred-frame* '(:empty :focused)
      stumpwm::*menu-maximum-height*       50
      stumpwm::*menu-scrolling-step*       1
      stumpwm:*suppress-frame-indicator*   nil
      stumpwm:*normal-border-width*        1
      stumpwm:*window-border-style*        :thight
      ;; stumpwm:mode-line
      stumpwm:*mode-line-position*         :top)


(setf *group-format* "%t [%s]")

;; https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks#grabbed-pointer
;; xfd -fn cursor
(setf *grab-pointer-foreground*     (xlib:make-color :red 0.1 :green 0.25 :blue 0.5)
      *grab-pointer-background*     (lookup-color (current-screen) "Cyan") ;; (lookup-color (current-screen) "DeepSkyBlue")
      *grab-pointer-character*      50
      *grab-pointer-character-mask* 51
      *grab-pointer-font*           "cursor")

;;}}}


;;{{ Pointer
(setq *grab-pointer-timeout* 4
      ;; change window numbers
      stumpwm:*frame-number-map* "1234567890"
      stumpwm::*group-number-map* "123456789"
      ;;Run-or-raise work through multiple screens
      stumpwm:*run-or-raise-all-screens* t
      stumpwm:*run-or-raise-all-groups*  t
      *disk-usage-paths* '("/" "/home" "/usr" "/pacific"))
;;}}

;;{{{ Background
(defparameter *desktop-background-image-path* (concat *home-dir* "/.share/backgrounds"))

(defparameter wallpaper-image-command
  (car
   '("display -resize ~a -window root ~a"
     "xv -quit -root -rmode 5 -max ~a"))
  "wallpaper image command")

(defparameter bing-wallpaper-image-command
  (concat *home-dir* "/bin/bingwallpaper 2>&1 > /dev/null")
  "bing wallpaper command")

(defun get-screen-height (screen)
  (xlib:drawable-height (screen-root (current-screen))))

(defun get-screen-width (screen)
  (xlib:drawable-width (screen-root (current-screen))))

(defun get-screen-root-display-size (&optional (screen (current-screen)))
  (format nil "~dx~d"
          (get-screen-width screen)
          (get-screen-height screen)))

(defun head-display-size (&optional (head (current-head)))
  (format nil "~dx~d"
          (head-width head)
          (head-height head)))

(defun select-random-wallpaper-image-path ()
  "Select a random image"
  (let ((file-list (directory (concatenate 'string *desktop-background-image-path* "/*/*.jpg"))))
    (if file-list
        (let ((*random-state* (if file-list (make-random-state t))))
          (namestring (nth (random (length file-list)) file-list))))))

(defun setup-wallpaper-image (image-path)
  (run-shell-command
   (format nil wallpaper-image-command (get-screen-root-display-size (current-screen)) image-path)))

(defun setup-random-wallpaper-image ()
  (let ((image-path (select-random-wallpaper-image-path)))
    (if image-path
        (setup-wallpaper-image image-path)
        (message "No image present to setup random wallpaper."))))

(defcommand random-wallpaper () ()
  "Setup random wallpaper"
  (setup-random-wallpaper-image))

(defcommand bing-wallpaper () ()
  "Setup bing wallpaper"
  (run-shell-command bing-wallpaper-image-command))

;;}}} Background

;; Default layout
;;{{{ mode-line

(defmacro def-list-share ())

(progn

  (defun cleanup-str (str)
    (remove #\Newline str))

  (defun cleanup-str-nl (str)
    (substitute #\Space #\Newline str))

  ;; https://github.com/joelagnel/stumpwm-goodies/blob/master/mode-line/modeline-config.lisp
  (defun show-ip-address ()
    (let ((ip (run-shell-command "ip addr show dev enp0s31f6  | grep 'inet' | head -1 | cut -d' ' -f6 | cut -d/ -f1" t)))
      (cleanup-str ip)))

  (defun show-battery-charge ()
    (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
      (cleanup-str raw-battery)))

  (defun show-hostname ()
    (let ((host-name (run-shell-command "cat /etc/hostname" t)))
      (cleanup-str host-name)))

  (defun show-battery-state ()
    (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
      (cleanup-str raw-battery)))

  (defun show-kernel ()
    (let ((version (run-shell-command "uname -r" t)))
      (cleanup-str version)))

  (defun show-emacs-jabber-new-message ()
    (let ((new-message (run-shell-command "cat /home/joel/emacs-jabber.temp" t)))
;;;     (and (> (length new-message) 0) (stumpwm:message new-message))
      (cleanup-str new-message)))
;;;

  (defun show-emacs-jabber-new-mail ()
    (let ((new-mail (run-shell-command "cat /home/joel/emacs-jabber-mail.temp" t)))
      (if (not (eq (length new-mail) 0))
          (progn (stumpwm:message new-mail)
                 (run-shell-command "rm /home/joel/emacs-jabber-mail.temp" t)
                 (run-shell-command "touch /home/joel/emacs-jabber-mail.temp" t)))
      ""))

  (defun show-uptime ()
    (let ((uptime (run-shell-command "uptime | tr ', ' '\\n' | sed  '/^$/d' | sed -n -e 2,3p -e 4's/$/u/p' -e 8,9's/$/,/p' -e 10p | xargs echo | sed 's@,@@g'" t)))
      (concat "|" (cleanup-str uptime) "|"))))


(defvar *mode-line-fmts* '(("^[^B^7*%h^]" "/"
                            (:eval (show-ip-address)) " "
                            (:eval (format-expand *time-format-string-alist*
                                                  ;; "%a %b %e %Y - %k:%M:%S"
                                                  "%a %k:%M:%S %b %e %Y"))
                            " "
                            (:eval (show-kernel))     " "
                            (:eval (show-uptime))     " "
                            ;; "%c (%f) - %B - ^71%N^** [^B%n^71%u^**^b] %T %W - %m - %D - %I - %p"
                            "%c (%f) - %B - ^71%N^** [^B%n^71%u^**^b] %T %W - %m - %D - %I - %p")

                           ("^[^B^7*%h^] " (:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^01%N^** [^B%n^01%u^**^b] %T %W - %m - %D - %I ")
                           ("^[^B^7*%h^] " (:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^1*%N^** [^B%n^b ^B^1*%u^**^b ] %T %W - %m - %D - %I")
                           ("^[^B^7*%h^] " (:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S"))
                            (:eval (format "Name"))
                            " %p - %c (%f) - %B - ^1*%N^** [^B%n^b ^B^1*%u^**^b ] %T %W - %m - %D - %I")
                           ;; ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - %N [^B%n^b ^71^B%u^b^70^* ] %W - %m - %D - %I")
                           ;; (:eval (format "Name"))
                           " - %c (%f) - %b %B - %N [^B%n^b ^B^1%u^*^b ] %T %W - %m - %D - %I"

                           "%N [^B%n^b ^B^1%u^*^b ] %T %W - %I"))

;; (setf *mode-line-fmts* '(
;;                            ((:eval
;;                              (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S"))
;;                             ;; (:eval (format "Name"))
;;                             "~% - %c (%f) - %B - [^B%n^b ^B%u^b] %W - %m - %D")
;;                             " - %c (%f) - %b %B - [^B%n^b ^B%u^b ] %W - %m - %D"
;;                             "[^B%n^b ^B%u^b] %W"))

(setf stumpwm:*screen-mode-line-format* (car *mode-line-fmts*))

;; Stumpwm lets you have a mode-line that can be used to show
;; different things (e.g. - what windows/groups are open, date/time,
;; cpu usage, etc). I normally don't want to see the mode-line
;; (because I don't like to see mode-line info unless I want to see
;; it) but I do want to see it when I'm running a Stumpwm command (I
;; display window/group info in the mode-line; therefore, if I'm
;; switching to a different window/group, it's convenient to have the
;; mode-line display when I'm initiating something in Stumpwm). So, I
;; created a "hook" that runs whenever the Stumpwm "escape key" (by
;; default - "C-t") is pressed. Now, I only see the mode line when I
;; actually need to see it:

;;}}} mode-line end


;;Set X11 background image for all screens
(defun screen-initilize-decoration ()
  (when *desktop-background-image-path*
    (let ((start-screen (car *screen-list*)))
      (message "setting screen ~a" start-screen)
      (loop for screen in *screen-list*
            for fmt in *mode-line-fmts*
            do (progn
                 (switch-to-screen screen)
                 ;; Turn on the modeline
                 (message "setting screen ~a" screen)
                 (dolist (head (screen-heads screen))
                   (message "setting screen ~a head ~a" screen head)
                   ;; (sleep 1)
                   (unless (head-mode-line head)
                     (enable-mode-line screen head t fmt)))
                 (setup-random-wallpaper-image)))
      (switch-to-screen start-screen))))

(screen-initilize-decoration)
;; setup bing wall paper
(bing-wallpaper)

(defun new-head-function (head screen)
  (when (and head screen)
    (when (> (head-number head) 0)
      (unless (head-mode-line head)
        (enable-mode-line screen head t)
        (when (fboundp 'set-profile)
          (set-profile :myprofile))))))

(add-hook *new-head-hook* #'new-head-function)

;; "display -resize `xwininfo -root | awk '{ if ($1 == \"Width:\" ) { w=$2 } else if ($1 == \"Height:\" ) { h=$2 } } END { print w \"x\" h }'` -window root "

#+wifi
(setf wifi:*iwconfig-path*
      (let ((default-path (or wifi:*iwconfig-path* "/sbin/iwconfig"))
            (found-path   (some #'probe-file
                                '("/sbin/iwconfig"
                                  "/run/current-system/profile/sbin/iwconfig"))))
        (if found-path (namestring found-path) default)))

