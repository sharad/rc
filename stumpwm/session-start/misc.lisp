
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

(defparameter bing-wallpaper-image-command
  (concat *home-dir* "/bin/bingwallpaper 2>&1 > /dev/null")
  "bing wallpaper command")

(defun get-screen-height (screen)
  (xlib:drawable-height (screen-root (current-screen))))

(defun get-screen-width (screen)
  (xlib:drawable-width (screen-root (current-screen))))

(defun get-screen-root-display-size (&optional (screen (current-screen)) )
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

;;{{ Pointer
(defcommand show-pointer () ()
  "Show pointer"
  (let ((head (current-head)))
    (let ((y (head-y head))
          (height (head-height head))
          (x (head-x head))
          (width (head-width head)))
      (let ((pointer-y (+ x (/ width 2)))
            (pointer-x (+ y (/ height 2))))
        (message (concat
                  "x[~a] + width[~a] / 2 = ~a"
                  "~%"
                  "y[~a] + height[~a] / 2 = ~a")
                 x width pointer-y
                 y height pointer-x)
        (warp-pointer (current-screen)
                      pointer-y
                      pointer-x)))))
;;}}

;; Default layout
;;{{{ mode-line
(defvar *mode-line-fmts* '(
                           ("^[^B^7*%h^] " (:eval (format-expand *time-format-string-alist* "%a %b %e %Y - %k:%M:%S")) " %p - %c (%f) - %B - ^71%N^** [^B%n^71%u^**^b] %T %W - %m - %D - %I ")
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

(defun toggle-mode-line-on-key-press (key key-seq cmd)
  (declare (ignore key key-seq cmd))
  (mode-line))

(defun toggle-mode-line-on-key-press (key key-seq cmd)
  (declare (ignore key key-seq cmd))
  (toggle-mode-line (current-screen) (current-head) (car *mode-line-fmts*)))

(let ()
  (defun mode-line-when-pointer-grabbed (key key-seq cmd)
    ;; (declare (ignore key key-seq cmd))
    (declare (ignore key key-seq))
    (enable-mode-line
     (current-screen)
     (current-head)
     (kmap-or-kmap-symbol-p cmd)))

  (defcommand toggle-mode-line-enable () ()
    (add-hook *key-press-hook* 'mode-line-when-pointer-grabbed))

  (defcommand toggle-mode-line-disable () ()
    (remove-hook *key-press-hook* 'mode-line-when-pointer-grabbed)))








(let ((deactivate-fullscreen-idle-timeout 10)
      (deactivate-fullscreen-timer nil)
      (toggle-fullscreen-on-ungrabbed-pointer-for-few-mins 7)
      (toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer nil))
  ;; TODO: disable fullscreen on inactivity

  (defun activate-fullscreen-if-not (window)
    (when window
      (unless (window-fullscreen window)
        ;; (activate-fullscreen window)
        (dformat 2 "client requests to go fullscreen~%")
        (add-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
        (setf (window-fullscreen window) t)
        (focus-window window)
        (update-mode-lines (current-screen)))))

  (defun deactivate-fullscreen-if-not (window)
    (when window
      (when (window-fullscreen window)
        (setf (window-fullscreen window) nil)
        (dformat 2 "client requests to leave fullscreen~%")
        (remove-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
        (update-decoration window)
        (update-mode-lines (current-screen)))))

  (defun fullscreen-pointer-not-grabbed (key key-seq cmd)
    (declare (ignore key key-seq))
    (if (kmap-or-kmap-symbol-p cmd)
        (progn
          (deactivate-fullscreen-if-not (current-window)))
        (progn
          (let ((win (other-hidden-window (current-group))))
            (unless win
              (activate-fullscreen-if-not (current-window)))))))
  ;; should not be here
  ;; is required when one window is present in frame.
  ;; but creates problem with conkeror.
  ;; (activate-fullscreen-if-not (current-window))


  (defun fullscreen-focus-window (cwin lwin)
    (activate-fullscreen-if-not cwin)
    (deactivate-fullscreen-if-not lwin))

  (defun fullscreen-curr-post-command (cmd)
    (activate-fullscreen-if-not (current-window)))

  (defun unfullscreen-curr-post-command (cmd)
    (deactivate-fullscreen-if-not (current-window)))



  (progn
    (defun deactivate-full-screen-on-idle-timeout ()
      (when (> deactivate-fullscreen-idle-timeout 2)
        (when (member 'fullscreen-focus-window *focus-window-hook*)
          ;; (message "deactivate fs")
          (if (> (stumpwm::idle-time (stumpwm::current-screen)) deactivate-fullscreen-idle-timeout)
              (deactivate-fullscreen-if-not (stumpwm::current-window))
              (activate-fullscreen-if-not (stumpwm::current-window))))))

    (defun deactivate-full-screen-on-idle-timer-stop ()
      "Stops the newmail timer."
      (ignore-errors
       (when deactivate-fullscreen-timer
         (stumpwm::cancel-timer newmail-timer)
         (setf deactivate-fullscreen-timer nil))))

    (defun deactivate-full-screen-on-idle-timer-start ()
      "Starts the newmail timer."
      (deactivate-full-screen-on-idle-timer-stop)
      (setf deactivate-fullscreen-timer
            (stumpwm::run-with-timer
             deactivate-fullscreen-idle-timeout
             deactivate-fullscreen-idle-timeout
             'deactivate-full-screen-on-idle-timeout)))

    (defcommand deactivate-full-screen-on-idle-start () ()
      "Starts the newmail timer."
      (deactivate-full-screen-on-idle-timer-start))

    (defcommand deactivate-full-screen-on-idle-stop () ()
      "Stops the newmail timer."
      (deactivate-full-screen-on-idle-timer-stop)))





  (defcommand fullscreen-on-ungrabbed-pointer-enable () ()
    (add-hook *key-press-hook* 'fullscreen-pointer-not-grabbed)
    (add-hook *focus-window-hook* 'fullscreen-focus-window)
    (deactivate-full-screen-on-idle-timer-start)
    (activate-fullscreen-if-not (current-window)))

  (defcommand fullscreen-on-ungrabbed-pointer-disable () ()
    (remove-hook *key-press-hook* 'fullscreen-pointer-not-grabbed)
    (remove-hook *focus-window-hook* 'fullscreen-focus-window)
    (deactivate-full-screen-on-idle-timer-stop)
    (deactivate-fullscreen-if-not (current-window)))

  (defcommand toggle-fullscreen-on-ungrabbed-pointer () ()
    (if (member 'fullscreen-focus-window *focus-window-hook*)
        (fullscreen-on-ungrabbed-pointer-disable)
        (fullscreen-on-ungrabbed-pointer-enable)))

  (defun toggle-fullscreen-on-ungrabbed-pointer-after-few-mins ()
    (when toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer
      (cancel-timer toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer)
      (setf toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer nil))
    (toggle-fullscreen-on-ungrabbed-pointer))

  (defcommand toggle-fullscreen-on-ungrabbed-pointer-for-few-mins () ()
    "run toggle-fullscreen-on-ungrabbed-pointer-for-few-mins"
    (when (> toggle-fullscreen-on-ungrabbed-pointer-for-few-mins 1)
      (let (mins
            (*
              (if toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer 2 1)
              toggle-fullscreen-on-ungrabbed-pointer-for-few-mins
              60))
        (toggle-fullscreen-on-ungrabbed-pointer-after-few-mins)
        (setf toggle-fullscreen-on-ungrabbed-pointer-for-few-mins-timer
              (stumpwm::run-with-timer mins nil #'toggle-fullscreen-on-ungrabbed-pointer-after-few-mins)))))

  ;; enable it.
  (fullscreen-on-ungrabbed-pointer-enable))


;;}}} mode-line end

;;Set X11 background image for all screens
(defun screen-initilize-decoration ()
  (when *desktop-background-image-path*
    (let ((start-screen (car *screen-list*)))
      (loop for i in *screen-list*
            for j in *mode-line-fmts*
            do (progn (switch-to-screen i)
                      ;; Turn on the modeline
                      (if (not (head-mode-line (current-head)))
                          (toggle-mode-line (current-screen) (current-head) j))
                      (setup-random-wallpaper-image)))
      (switch-to-screen start-screen))))

(screen-initilize-decoration)
;; setup bing wall paper
(bing-wallpaper)

;; "display -resize `xwininfo -root | awk '{ if ($1 == \"Width:\" ) { w=$2 } else if ($1 == \"Height:\" ) { h=$2 } } END { print w \"x\" h }'` -window root "

(defun time-plist (&optional time)
  (multiple-value-bind (sec min hour dom mon year dow dstp tz)
      ;; (or time   (decode-universal-time (+ (get-universal-time) (* 30 60))))
      (or time   (decode-universal-time (+ (get-universal-time) (* 0 60))))
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
                       "xrandr --output ~A --mode 1024x768 --same-as ~A --output ~A --mode 1024x768"
                       (car vga)
                       (car lvds)
                       (car lvds)))
              (run-shell-command
               (format nil "xrandr --output ~A --off --output ~A --auto"
                       (car vga)
                       (car lvds)))))))))

;;{{(find-package (symbol-value (intern "stumpwm" :keyword)))
;; (define-stumpwm-type :package (input prompt)
;;         (or
;;          (pa-fnstumpwm::choose-or-provide
;;           (mapcar #'package-name (list-all-packages))
;;           :dialog prompt
;;           :autoselect-if-only t
;;           :choice-time-out-seconds 100
;;           :extra-choices nil)
;;          (throw 'error "Abort.")))

(define-stumpwm-type :package (input prompt)
  (completing-read
   (current-screen)
   prompt
   (mapcar #'(lambda (p)
               (string-downcase
                (package-name p)))
           (list-all-packages))
   :initial-input (string-downcase (package-name *package*))
   :require-match nil))

(defcommand current-package (pkg) ((:package "package: "))
  (let ((pkg (find-package (string-upcase pkg))))
    (if pkg
        (progn
          (message "current package ~a" pkg)
          (setf *package* pkg))
        (message "current package ~a" *package*))))
;;}}

;;{{
(defun head-force-refresh (screen new-heads)
  (scale-screen screen new-heads)
  (mapc 'group-sync-all-heads (screen-groups screen))
  (update-mode-lines screen))

(defcommand refresh-heads (&optional (screen (current-screen))) ()
  "Refresh screens in case a monitor was connected, but a
  ConfigureNotify event was snarfed by another program."
  (head-force-refresh screen (make-screen-heads screen (screen-root screen))))
;;}}

;; ;;{{ Example
;; As an example, here’s a new type called :smart-direction. The
;; existing :direction type simply asks for one of the four directions
;; “left”, “right”, “up” or “down”, without checking to see if there’s
;; a frame in that direction. Our new type, :smart-direction, will
;; look around the current frame, and only allow the user to choose a
;; direction in which another frame lies. If only one direction is
;; possible it will return that automatically without troubling the
;; user. It signals an error for invalid directions; it could
;; alternately return a “nil” value in those cases, and let the
;; command handle that.


(define-stumpwm-type :smart-direction (input prompt)
  (let ((valid-dirs
         (loop  ; gather all the directions in which there's a neighbouring frame
            with values = '(("up" :up)
                            ("down" :down)
                            ("left" :left)
                            ("right" :right))
            with frame-set =
              (group-frames (window-group (current-window)))
            for dir in values
            for neighbour = (neighbour
                             (second dir)
                             (window-frame (current-window)) frame-set)
            if (and neighbour (frame-window neighbour))
            collect dir))
        (arg (argument-pop input)))  ; store a possible argument
    (cond ((null valid-dirs)  ; no directions, bail out
           (throw 'error "No valid directions"))
          (arg  ; an arg was bound, but is it valid?
           (or (second (assoc arg valid-dirs :test #'string=))
               (throw 'error "Not a valid direction")))
          ((= 1 (length valid-dirs))  ; only one valid direction
           (second (car valid-dirs)))
          (t  ; multiple possibilities, prompt for direction
           (second (assoc (completing-read input prompt valid-dirs
                                           :require-match t)
                          valid-dirs :test #'string=))))))

(defcommand smarty (dir) ((:smart-direction "Pick a direction: "))
  ;; `dir' is a keyword here
  (message "You're going ~a" (string-downcase dir)))

(define-key *root-map* (kbd "R") "smarty right")

;; ;;}}

(when nil
  ;;; DONE in stumpwm-contrib media/amixer/amixer.lisp
  ;;;{{ volume
  ;;; A command to create volume-control commands
  (defun def-volcontrol (channel amount)
    "Commands for controling the volume"
    (defcommand (intern (concat "amixer-" channel "-" (or amount "toggle"))) () ()
      (echo-string
       (current-screen)
       (concat channel " " (or amount "toggled") "
"
               (run-shell-command
                (concat "amixer sset " channel " " (or amount "toggle") "| grep '^[ ]*Front'") t)))))

  (defvar amixer-channels '("PCM" "Master" "Headphone"))
  (defvar amixer-options '(nil "1+" "1-"))

  (let ((channels amixer-channels))
    (loop while channels do
      (let ((options amixer-options))
        (loop while options do
          (def-volcontrol (car channels) (car options))
          (setq options (cdr options))))
      (setq channels (cdr channels))))

  (defcommand "amixer-sense-toggle" () ()
    (echo-string
     (current-screen)
     (concat "Headphone Jack Sense toggled
"
             (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t)))))
;;;}}


;;;{{{
(defun local-window-matches-properties-p (window &key class instance type role title)
  "Returns T if window matches all the given properties"
  (and
   (if class (string-match (window-class window) class) t)
   (if instance (string-match (window-res window) instance) t)
   (if type (string-match (window-type window) type) t)
   (if role (string-match (window-role window) role) t)
   (if title (string-match (window-title window) title) t)))

(progn                                  ;;option macro

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defmacro gen-binary-option-commands (name)
    (let* ((option  (symb name '-p))
           (enable-fun  (symb 'enable- name '-function))
           (disable-fun (symb 'disable- name '-function))
           (enable  (symb 'disable- name))
           (disable (symb 'disable- name))
           (toggle  (symb 'toggle- name)))
      `(progn                            ;autoselect-if-only-p
         (stumpwm::defcommand ,enable () ()
           (setf ,option t)
           (if ,option
               (if (fboundp ',enable-fun) (funcall #',enable-fun))
               (if (fboundp ',disable-fun) (funcall #',disable-fun))))

         (stumpwm::defcommand ,disable () ()
           (setf ,option nil)
           (if ,option
               (if (fboundp ',enable-fun) (funcall #',enable-fun))
               (if (fboundp ',disable-fun) (funcall #',disable-fun))))

         (stumpwm::defcommand ,toggle () ()
           (setf ,option (not ,option))
           (if ,option
               (if (fboundp ',enable-fun) (funcall #',enable-fun))
               (if (fboundp ',disable-fun) (funcall #',disable-fun))))))))


(progn
  (let ((focus-window-match-rules-p t)
        (focus-window-match-rules '()))

    (defun define-focus-window-match (name &rest rule)
      (push
       (cons name rule)
       focus-window-match-rules))

    (defun matche-window-on-rules (window)
      (let ((rules focus-window-match-rules))
        (some
         #'(lambda (rule)
             (apply #'window-matches-properties-p window (cdr rule)))
         rules)))

    (defun set-focus-on-matched-window (window &optional force)
      ;; TODO pending trying to add code to resolve case when wcli window not get focus
      ;; make it toggle-able.
      (when (or
             force
             (matche-window-on-rules window))
        ;; TODO: how to detect if window did not get focus
        (let ((frame (stumpwm::window-frame window)))
          (stumpwm::focus-frame (stumpwm::window-group window) frame))))

    (defun focus-matched-window (&optional (window (current-window)))
      (when focus-window-match-rules-p
        (set-focus-on-matched-window window nil)))

    (defcommand test-focus-matched-window (&optional (win (current-window))) ()
      (when win
        (message "match ~a" (matche-window-on-rules win))))

    (progn
      (gen-binary-option-commands focus-window-match-rules)

      (defun disable-focus-window-match-rules-function ()
        (remove-hook *new-window-hook* #'focus-matched-window))
      (defun enable-focus-window-match-rules-function ()
        (add-hook *new-window-hook* #'focus-matched-window)))

    (enable-focus-window-match-rules)))

  (define-focus-window-match
      "pinentry-gtk"
    :class "Gcr-prompter"
    :instance  "gcr-prompter")

  (define-focus-window-match
      "gnome-keyring"
    :class "Gcr-prompter"
    :instance  "gcr-prompter"
    :title "Unlock Login Keyring"))




(let ((show-win-prop t))
  (defun sh-win-prop (&optional (window (current-window)))
    (let ((w (or window (current-window))))
      (if (not w)
          (message "No active window!")
          (message-no-timeout "class: ~A~%instance: ~A~%type: :~A~%role: ~A~%title: ~A"
                              (window-class w)
                              (window-res w)
                              (string (window-type w))
                              (window-role w)
                              (window-title w)))))

  (progn
    (gen-binary-option-commands show-win-prop)

    (defun disable-show-win-prop-function ()
      (remove-hook *new-window-hook* #'sh-win-prop))
    (defun enable-show-win-prop-function ()
      (setf *new-window-hook*
            (append *new-window-hook* (list #'sh-win-prop)))))

  (disable-show-win-prop))

;;;}}}
