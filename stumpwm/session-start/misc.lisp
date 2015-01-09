
(in-package :stumpwm)

;;{{ Pointer
(setq *grab-pointer-timeout* 4
      ;; change window numbers
      *frame-number-map* "1234567890"
      *group-number-map* "123456789"
      ;;Run-or-raise work through multiple screens
      *run-or-raise-all-screens* t
      *run-or-raise-all-groups*  t
      *disk-usage-paths* '("/" "/home" "/usr" "/pacific"))
;;}}

;;{{{ Background
(defparameter *desktop-background-image-path* (concat *home-dir* "/.share/backgrounds"))

(defparameter wallpaper-image-command
  (car
   '("display -resize ~a -window root ~a"
     "xv -quit -root -rmode 5 -max ~a"))
  "wallpaper image command")

(defun get-root-height ()
  (xlib:drawable-height (screen-root (current-screen))))

(defun get-root-width ()
  (xlib:drawable-width (screen-root (current-screen))))

(defun get-root-display-size ()
  (format nil "~dx~d"
          (get-root-width)
          (get-root-height)))

(defun select-random-wallpaper-image-path ()
  "Select a random image"
  (let ((file-list (directory (concatenate 'string *desktop-background-image-path* "/*/*.jpg"))))
    (if file-list
        (let ((*random-state* (if file-list (make-random-state t))))
          (namestring (nth (random (length file-list)) file-list))))))

(defun setup-wallpaper-image (image-path)
  (run-shell-command
   (format nil wallpaper-image-command (get-root-display-size) image-path)))

(defun setup-random-wallpaper-image ()
  (let ((image-path (select-random-wallpaper-image-path)))
    (if image-path
        (setup-wallpaper-image image-path)
        (message "No image present to setup random wallpaper."))))

(defcommand random-wallpaper () ()
  "Setup random wallpaper"
  (setup-random-wallpaper-image))

;;}}} Background

;;{{ Pointer
(defcommand show-pointer () ()
  "Show pointer"
  (warp-pointer (current-screen)
                (/ (get-root-width) 2)
                (/ (get-root-height) 2)))
;;}}

;; Default layout
;;{{{ mode-line
(defvar *mode-line-fmts* '(
                           ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^71%N^** [^B%n^71%u^**^b] %W - %m - %D")
                           ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^01%N^** [^B%n^01%u^**^b] %W - %m - %D")
                           ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^1*%N^** [^B%n^b ^B^1*%u^**^b ] %W - %m - %D")
                           ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S"))
                            (:eval (format "Name"))
                            " %p - %c (%f) - %B - ^1*%N^** [^B%n^b ^B^1*%u^**^b ] %W - %m - %D")
                           ;; ((:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - %N [^B%n^b ^71^B%u^b^70^* ] %W - %m - %D")
                           ;; (:eval (format "Name"))
                            " - %c (%f) - %b %B - %N [^B%n^b ^B^1%u^*^b ] %W - %m - %D"

                            "%N [^B%n^b ^B^1%u^*^b ] %W"))

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

;; (defun toggle-mode-line-hook (key key-seq cmd)
;;   (declare (ignore key key-seq cmd))
;;   (mode-line))

(defun toggle-mode-line-hook (key key-seq cmd)
  (declare (ignore key key-seq cmd))
  (toggle-mode-line (current-screen) (current-head) (car *mode-line-fmts*)))

;;(toggle-mode-line (current-screen) (current-head) '(eval: "(format \"Name\")"))

;; (add-hook *key-press-hook* 'toggle-mode-line-hook)
;;(add-hook *key-press-hook* 'toggle-mode-line)


;;}}} mode-line end

;;Set X11 background image for all screens
(when *desktop-background-image-path*
  (let ((start-screen (car *screen-list*)))
    (loop for i in *screen-list*
       for j in *mode-line-fmts*
       do (progn (switch-to-screen i)
                 ;; Turn on the modeline
                 (if (not (head-mode-line (current-head)))
                     (toggle-mode-line (current-screen) (current-head) j))
                 (setup-random-wallpaper-image)))
    (switch-to-screen start-screen)))
;; "display -resize `xwininfo -root | awk '{ if ($1 == \"Width:\" ) { w=$2 } else if ($1 == \"Height:\" ) { h=$2 } } END { print w \"x\" h }'` -window root "




(defun time-plist (&optional time)
  (multiple-value-bind (sec min hour dom mon year dow dstp tz)
      (or time   (decode-universal-time (+ (get-universal-time) (* 30 60))))
    (list :second sec :minute min :hour hour :dom dom :month mon
          :year year :dow dow :dlsavings-p dstp :tz tz)))




;; from: https://lists.gnu.org/archive/html/stumpwm-devel/2014-05/msg00001.html
(defcommand projector-toggle () ()
  "Toggle the projector (mirrorred display) on or off."
  (let ((xrandr-q (run-shell-command "xrandr -q" t)))
    (with-input-from-string (s xrandr-q)
      ;; If an output is on, xrandr shows its mode after the "NAME connected"
      ;; and before the parentheses.
      (let* ((displays (loop for line = (read-line s nil :EOF)
                             until (eql line :EOF)
                             for matches = (multiple-value-bind (str matches)
                                               (ppcre:scan-to-strings
                                                "^(.+) connected (.*)\\("
                                                line)
                                             (declare (ignore str))
                                             matches)
                             when matches
                             ;; matches[0] = output name
                             ;; matches[1] = current mode (if connected)
                             collect (cons (svref matches 0)
                                           (if (string= (svref matches 1) "")
                                               :OFF
                                               :ON))))
             (vga (find "VGA" displays :test #'ppcre:scan :key #'car))
             (lvds (find "LVDS" displays :test #'ppcre:scan :key #'car)))
        (when vga
          (if (eql (cdr vga) :off)
              (run-shell-command
               (format nil
                       "xrandr --output ~A --mode 1024x768 --same-as ~A
--output ~A --mode 1024x768"
                       (car vga)
                       (car lvds)
                       (car lvds)))
              (run-shell-command
               (format nil "xrandr --output ~A --off --output ~A --auto"
                       (car vga)
                       (car lvds)))))))))
