
(in-package :stumpwm)


(defcommand x-disable-capslock () ()
  (run-shell-command "if xset q | grep -q 'Caps Lock: *on' ; then xdotool key Caps_Lock ; fi"))


(progn
  ;; Profiles management
  (dolist (a '((#\p fmt-profile)))
    (pushnew a *screen-mode-line-formatters* :test 'equal))

  (let (current
        (profile-alist nil))

    (define-stumpwm-type :profile (input prompt)
      (or (find (intern
                 (string-upcase
                  (or (argument-pop input)
                      ;; Whitespace messes up find-symbol.
                      (string-trim " "
                                   (completing-read (current-screen)
                                                    prompt
                                                    ;; find all symbols in the
                                                    ;;  stumpwm package.
                                                    (let (acc)
                                                      (dolist (s (mapcar #'car profile-alist))
                                                        (push (string-downcase (symbol-name s)) acc))
                                                      acc)))
                      (throw 'error "Abort.")))
                 "KEYWORD")
                profile-alist :key #'car :test #'equal)
          (throw 'error "Symbol not in STUMPWM package")))

    (defun profile-add (name &rest alist)
      (if (push (cons name alist) profile-alist)
          (message "Added ~a profile" (setf current name))))

    (defun profile-apply (profile)
      (x-disable-capslock)
      (let ((maps (cdr (assoc :map profile)))
            (cmds (cdr (assoc :cmd profile))))
        (if maps
            (if (consp maps)
                (dolist (m maps)
                  (run-shell-command (concat "xmodmap " m)))
                (run-shell-command (concat "xmodmap " maps))))
        (if cmds
            (if (consp cmds)
                (dolist (c cmds)
                  (run-shell-command c))
                (run-shell-command cmds)))
        t))

    (defcommand toggle-profile () ()
      (let ((profile (car (or (cdr (member current profile-alist :key #'car)) profile-alist))))
        (set-profile profile)))

    (defcommand set-profile (profile) ((:profile "profile name: "))
      (let ((pr (if (symbolp profile)
                    (find profile profile-alist :key #'car :test #'equal )
                    profile)))
        (if (profile-apply (cdr pr))
            (message "applied ~a profile" (setf current (car pr)))
            (message "failed to apply ~a profile" (car pr)))))

    (defcommand show-profile (profile) ((:profile "profile name: "))
      (message "profile: ~a" profile))

    (defcommand show-current-profile () ()
      (show-profile (find current profile-alist :key #'car :test #'equal)))

    (defcommand show-fullprofile () ()
      (message "profile: ~a" profile-alist))

    (defun fmt-profile (mil)
      (declare (ignore ml))
      (format nil "~a" current)))

  (profile-add :cprofile
               '(:map "~/.Xmodmaps/base-map" "~/.Xmodmaps/xmodmaprc-normal-but-super")
               '(:cmd "python -c 'from ctypes import *; X11 = cdll.LoadLibrary(\"libX11.so.6\"); display = X11.XOpenDisplay(None); X11.XkbLockModifiers(display, c_uint(0x0100), c_uint(2), c_uint(0)); X11.XCloseDisplay(display)'" "synclient TouchpadOff=0"))
  (profile-add :myprofile
               '(:map "~/.Xmodmaps/base-map" "~/.Xmodmaps/xmodmaprc-swap-alt-ctrl-caps=alt")
               '(:cmd "python -c 'from ctypes import *; X11 = cdll.LoadLibrary(\"libX11.so.6\"); display = X11.XOpenDisplay(None); X11.XkbLockModifiers(display, c_uint(0x0100), c_uint(2), c_uint(0)); X11.XCloseDisplay(display)'" "synclient TouchpadOff=1")))





;;{{ When Other profile came in
;; (defstruct key
;;   keysym shift control meta alt hyper super)


(defun keysym->code-state (key)
  ;; (key (kbd keystring))
  (let* ((keysym (key-keysym key))
         (code (xlib:keysym->keycodes *display* keysym))
         (state (apply
                 'xlib:make-state-mask
                 (remove-if 'null
                            (mapcar 'car
                                    (list
                                     (if (key-shift key) '(:shift))
                                     (if (key-control key) '(:control))
                                     (if (key-meta key) (modifiers-meta *modifiers*))
                                     (if (key-alt key) (modifiers-alt *modifiers*))
                                     (if (key-hyper key) (modifiers-hype *modifiers*))
                                     (if (key-super key) (modifiers-super *modifiers*))))))))
    (cons code state)))

;; (keysym->code-state (kbd "C-`"))

(defun send-escape-key ()
  (let* ((code-state (keysym->code-state *escape-key* #|(kbd "C-`")|#))
         (code (car code-state))
         (state (cdr code-state)))
    ;; (funcall (gethash :key-press *event-fn-table*) :code code :state state)
    (handle-event :event-key :key-press :code code :state state)))

(defcommand gobackmyp () ()
  (set-profile :myprofile)
  (message "enabled your profile")
  ;;(sleep 2)
  (send-escape-key))

;;}}
