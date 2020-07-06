
(in-package :stumpwm)


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

;; (defun head-force-refresh (screen new-heads)
;;   (scale-screen screen new-heads)
;;   (mapc 'group-sync-all-heads (screen-groups screen))
;;   (update-mode-lines screen))

;; (defcommand refresh-heads (&optional (screen (current-screen))) ()
;;   "Refresh screens in case a monitor was connected, but a
;;   ConfigureNotify event was snarfed by another program."
;;   (head-force-refresh screen (make-screen-heads screen (screen-root screen))))


;; (defun head-force-refresh (screen new-heads)
;;   (scale-screen screen new-heads)
;;   (mapc 'group-sync-all-heads (screen-groups screen))
;;   (loop for new-head in new-heads
;;         do (run-hook-with-args *new-head-hook* new-head screen)))

;; (defcommand refresh-heads (&optional (screen (current-screen))) ()
;;   "Refresh screens in case a monitor was connected, but a
;;   ConfigureNotify event was snarfed by another program."
;;   (head-force-refresh screen (make-screen-heads screen (screen-root screen))))
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


(stumpwm:define-stumpwm-type :smart-direction (input prompt)
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

;;;}}


;;;{{{
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


(progn                                  ;;option macro

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defmacro gen-binary-option-body (name &rest body)
    (let* ((option      (symb name '-p))
           (enable-fun  (symb 'enable- name '-function))
           (disable-fun (symb 'disable- name '-function))
           (enable      (symb 'enable- name))
           (disable     (symb 'disable- name))
           (toggle      (symb 'toggle- name))
           (get-map     (symb 'get- name '-map))
           (map         (symb '* name '-map*)))
      `(let* ((,option nil)
              (impl #'(lambda ()
                        (if ,option
                            (if (fboundp ',enable-fun) (funcall #',enable-fun))
                            (if (fboundp ',disable-fun) (funcall #',disable-fun))))))
         (stumpwm::defcommand ,enable () ()
           (setf ,option t)
           (funcall impl))

         (stumpwm::defcommand ,disable () ()
           (setf ,option nil)
           (funcall impl))

         (stumpwm::defcommand ,toggle () ()
           (setf ,option (not ,option))
           (funcall impl))

         (defun ,get-map ()
           (let ((m (make-sparse-keymap)))
             (define-key m (kbd "e") (string-downcase (symbol-name ',enable)))
             (define-key m (kbd "d") (string-downcase (symbol-name ',disable)))
             (define-key m (kbd "t") (string-downcase (symbol-name ',toggle)))
             m))

         (set ',map (,get-map))

         (progn
           ,@body))))

  (defmacro gen-binary-option-commands (name)
    `(gen-binary-option-body ,name)))


;;;{{{
(progn
  (gen-binary-option-commands show-key-binding)

  (defun show-key-binding-if-incomplete (key key-seq cmd)
    (declare (ignore key))
    (when (and
           (not (eq cmd '*root-map*))
           (kmap-or-kmap-symbol-p cmd))
      (display-bindings-for-keymaps key-seq (symbol-value cmd))))

  (defun disable-show-key-binding-function ()
    (remove-hook *key-press-hook* #'show-key-binding-if-incomplete))

  (defun enable-show-key-binding-function ()
    (add-hook *key-press-hook* #'show-key-binding-if-incomplete))

  (enable-show-key-binding))
;;;}}}


;;;{{{ mode-line-on-key-press
(progn
  (gen-binary-option-commands mode-line-on-key-press)

  (defun mode-line-when-pointer-grabbed (key key-seq cmd)
    ;; (declare (ignore key key-seq cmd))
    (declare (ignore key key-seq))
    (enable-mode-line
     (current-screen)
     (current-head)
     (kmap-or-kmap-symbol-p cmd)))

  (defun disable-mode-line-on-key-press-function ()
    (remove-hook *key-press-hook* #'mode-line-when-pointer-grabbed))

  (defun enable-mode-line-on-key-press-function ()
    (add-hook *key-press-hook* #'mode-line-when-pointer-grabbed))

  (disable-mode-line-on-key-press))
;;;}}


;;;{{{ focus-window-match-rules
(progn

  (gen-binary-option-commands focus-window-match-rules)

  (defun local-window-matches-properties-p (window &key class instance type role title)
    "Returns T if window matches all the given properties"
    (and
     (if class (string-match (window-class window) class) t)
     (if instance (string-match (window-res window) instance) t)
     (if type (string-match (window-type window) type) t)
     (if role (string-match (window-role window) role) t)
     (if title (string-match (window-title window) title) t)))

  (let ((focus-window-match-rules-p t)
        (focus-window-match-rules '()))
    (defun define-focus-window-match-rule (name &rest rule)
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

    (defun disable-focus-window-match-rules-function ()
      (remove-hook *new-window-hook* #'focus-matched-window))
    (defun enable-focus-window-match-rules-function ()
      (add-hook *new-window-hook* #'focus-matched-window))

    (enable-focus-window-match-rules))

  (define-focus-window-match-rule
      "pinentry-gtk"
    :class "Gcr-prompter"
    :instance  "gcr-prompter")

  (define-focus-window-match-rule
      "gnome-keyring"
    :class "Gcr-prompter"
    :instance  "gcr-prompter"
    :title "Unlock Login Keyring")

  (define-focus-window-match-rule
      "ssh"
    :class "Gcr-prompter"
    :instance "gcr-prompter"
    :title "pinentry-gnome3"))
;;;}}}


;;;{{{ show-win-prop
(let ((show-win-prop-p t))

  (gen-binary-option-commands show-win-prop)

  (defun show-win-prop (&optional (window (current-window)))
    (let ((w (or window (current-window))))
      (if (not w)
          (message "No active window!")
          (message-no-timeout "class: ~A~%instance: ~A~%type: :~A~%role: ~A~%title: ~A"
                              (window-class w)
                              (window-res w)
                              (string (window-type w))
                              (window-role w)
                              (window-title w)))))

  (defun disable-show-win-prop-function ()
    (remove-hook *new-window-hook* #'show-win-prop))
  (defun enable-show-win-prop-function ()
    (setf *new-window-hook*
          (append *new-window-hook* (list #'show-win-prop))))

  (enable-show-win-prop))
;;;}}}


;;;{{{ fullscreen-on-ungrabbed-pointer
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
        ;; why it should be already in this state
        ;; (focus-window window)
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

  ;; sharad
  (defun fullscreen-focus-frame (cframe lframe)
    (activate-fullscreen-if-not   (frame-window cframe))
    (deactivate-fullscreen-if-not (frame-window lframe)))

  (defun fullscreen-focus-window (cwin lwin)
    (activate-fullscreen-if-not   cwin)
    (deactivate-fullscreen-if-not lwin))

  (defun fullscreen-curr-post-command (cmd)
    (activate-fullscreen-if-not (current-window)))

  (defun unfullscreen-curr-post-command (cmd)
    (deactivate-fullscreen-if-not (current-window)))

  (progn
    (defun deactivate-full-screen-on-idle-timeout ()
      (when (> deactivate-fullscreen-idle-timeout 2)
        (when (member 'fullscreen-focus-frame *focus-frame-hook*)
          ;; (message "deactivate fs")
          (if (> (stumpwm::idle-time (stumpwm::current-screen)) deactivate-fullscreen-idle-timeout)
              (deactivate-fullscreen-if-not (stumpwm::current-window))
              (activate-fullscreen-if-not   (stumpwm::current-window))))))

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


  ;; BUG1: when focus move from window frame to empty frame window not get unfullscreen
  ;; BUG2: when color or eval command read input that time window again become full screen.

  (when  nil
    (progn
      (defun test-focus-window (cwin lwin)
        (message "*focus-frame-hook*: cwin: ~a, lwin: ~a" cwin lwin))
      (add-hook *focus-frame-hook* 'test-focus-window)
      (defun test-focus-frame (cframe lframe)
        (message "*focus-frame-hook*: ~a ~a" cframe lframe))
      (add-hook *focus-frame-hook* 'test-focus-frame)
      (defun test-command-mode-start ()
        (message "*command-mode-start-hook*:"))
      (add-hook *command-mode-start-hook* 'test-command-mode-start)
      (defun test-command-mode-end ()
        (message "*command-mode-end-hook*:"))
      (add-hook *command-mode-end-hook* 'test-command-mode-end)
      (defun test-split-frame ()
        (message "*split-frame-hook*:"))
      (add-hook *split-frame-hook* 'test-split-frame)
      (defun test-focus-group ()
        (message "*focus-group-hook*:"))
      (add-hook *focus-group-hook* 'test-focus-group)))

  (defcommand enable-fullscreen-on-ungrabbed-pointer () ()
    (add-hook *key-press-hook* 'fullscreen-pointer-not-grabbed)
    (add-hook *focus-frame-hook* 'fullscreen-focus-frame)
    (add-hook *focus-window-hook* 'fullscreen-focus-window)
    (deactivate-full-screen-on-idle-timer-start)
    (activate-fullscreen-if-not (current-window)))

  (defcommand disable-fullscreen-on-ungrabbed-pointer () ()
    (remove-hook *key-press-hook* 'fullscreen-pointer-not-grabbed)
    (remove-hook *focus-frame-hook* 'fullscreen-focus-frame)
    (remove-hook *focus-window-hook* 'fullscreen-focus-window)
    (deactivate-full-screen-on-idle-timer-stop)
    (deactivate-fullscreen-if-not (current-window)))

  (defcommand toggle-fullscreen-on-ungrabbed-pointer () ()
    (if (member 'fullscreen-focus-frame *focus-frame-hook*)
        (disable-fullscreen-on-ungrabbed-pointer)
        (enable-fullscreen-on-ungrabbed-pointer)))

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

  (defun get-fullscreen-on-ungrabbed-pointer-map ()
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "e") "enable-fullscreen-on-ungrabbed-pointer")
      (define-key m (kbd "d") "disable-fullscreen-on-ungrabbed-pointer")
      (define-key m (kbd "t") "toggle-fullscreen-on-ungrabbed-pointer")
      m))

  (setf *fullscreen-on-ungrabbed-pointer-map* (get-fullscreen-on-ungrabbed-pointer-map))

  ;; enable it.
  (enable-fullscreen-on-ungrabbed-pointer))
;;;}}

