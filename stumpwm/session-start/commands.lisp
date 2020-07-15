;; -*-lisp-*-
;;
;; Window managment ------------------------------------------------------------

(in-package :stumpwm)



;;{{{ Notification system

(defun msg-notify (fmt args)
  (let ((*executing-stumpwm-command* nil)
        (*message-window-gravity* :center))
    (stumpwm::message-no-timeout fmt args)))

(stumpwm:defcommand notify (msg) ((:rest "Notify: "))
  (msg-notify "~a" msg))
;;}}}


(stumpwm:defcommand fnext () ()
  (stumpwm::focus-next-frame (stumpwm:current-group)))
(stumpwm:defcommand fprev () ()
  (stumpwm::focus-prev-frame (stumpwm:current-group)))


;; Aquasole
;; OSD text
(stumpwm:defcommand osd-echo () ()
  (let* ((screen (stumpwm:current-screen))
         (w (window-xwin (screen-focus screen))))
    (stumpwm::echo-in-window w
                             (stumpwm::screen-font screen)
                             (stumpwm::screen-fg-color screen)
                             (stumpwm::screen-bg-color screen)
                             "Test de l'osd"))
  (xlib:display-finish-output *display*))


(stumpwm:defcommand hsbalance-frames () ()
  "hsbalance-frames"
  (let ((gwin-count (1- (length (group-windows (current-group))))))
    (only)
    (dotimes (c gwin-count (balance-frames))
      (hsplit))))

(stumpwm:defcommand vsbalance-frames () ()
  "vsbalance-frames"
  (let ((gwin-count (1- (length (group-windows (current-group))))))
    (only)
    (dotimes (c gwin-count (balance-frames))
      (vsplit))))


;; menu test
(stumpwm:defcommand test-menu () ()
  (select-from-menu (current-screen)
                    '("a b qsdfùksdfqsdf sf"
                      "qsdf qsdf grfghd dwxfg"
                      "dfgdfg dsfgsd fg dfg df"
                      "dsfg dsfgds fdfhrttg"
                      "lpmk f*qzlfsdùflgsdfùl"
                      "dfhdf"
                      "dsfg dfsghlmkgfhl"
                      "kvfbùsdlfkg"
                      "sdflmgksd"
                      "fg")))


(defun create-backup (filename)
  #+ignore (class utility)
  "Create backup"
  (if (and filename (probe-file filename))
                                        ;(translate-pathname filename filename (concatenate 'string filename "~"))))
      (rename-file filename (concatenate 'string filename "~"))))

;; (create-backup "/tmp/out.mpg")


(let (video-pid
      filename)
  (stumpwm:defcommand grab-desktop-info () ()
    "Get video grabbing process pid"
    (let* ((pid (if (and video-pid (sb-ext:process-alive-p video-pid))
                    (remember-win:process-pid video-pid)))
           (file (if (and filename (probe-file filename))
                     filename))
           filesize)
      (if pid
          (stumpwm:message "Desktop grabbing process working with pid: ~a.~@[~&Outputting in ~a~]" pid file)
          (stumpwm:message "No desktop grabbing is going on in my knowledge.~@[~&But previous recording could be found in ~a~]" file))))

  (stumpwm:defcommand grab-desktop () ();(&optional filearg) ((:rest "Filename: "))
    (if (and video-pid (sb-ext:process-alive-p video-pid))
        (grab-desktop-stop)
        (grab-desktop-start (read-one-line (current-screen) "Filename: " :initial-input "/tmp/out.flv"))))

  (stumpwm:defcommand grab-desktop-start (&optional filearg) ((:rest "Filename: "))
    (if (and video-pid (sb-ext:process-alive-p video-pid))
        (stumpwm:message
         "Already desktop grabber is running with pid: ~a~&outputting into ~a"
         (remember-win:process-pid video-pid) filename)
        (let* ((filearg (or filearg "/tmp/out.mpeg"))
                                        ;(width (stumpwm:run-shell-command "xrandr | grep 'Screen 0' | awk '{ printf \"%s\", $8 }'" t))
                                        ;(hight (stumpwm:run-shell-command "xrandr | grep 'Screen 0' | awk '{ printf \"%s\", $10 }' | sed 's/,.*//'" t))
               (geometry (stumpwm:run-shell-command "xwininfo -root | grep 'geometry' | awk '{printf \"%s\", $2;}'" t))
               (depth    (stumpwm:run-shell-command "xwininfo -root | grep -i 'Depth' | awk '{printf \"%s\", $2;}'" t))
               (capture-cmd
                 (concatenate 'string
                                        ;"ffmpeg -y -f x11grab -s xga -r 24 -i "
                                        ;"ffmpeg -f x11grab -s " width "x" hight " -r 24 -i "
                              "ffmpeg -f x11grab -s " geometry " -r " depth " -i "
                              (getenv "DISPLAY")
                              ".0 -sameq "
                              filearg)))
          (if (and filearg (probe-file filearg))
              (create-backup filearg))
          (when (setf video-pid (remember-win:run-cli-command capture-cmd))
            (setf filename filearg)
            (stumpwm:message "Your pid is ~a with cmd ~a" (remember-win:process-pid video-pid) capture-cmd)))))

  ;;(stumpwm:message-no-timeout capture-cmd))))

  (stumpwm:defcommand grab-desktop-stop () ()
                                        ; shuld offer to play the last recordind, control it by user variable.
    (if (and video-pid
             (sb-ext:process-alive-p video-pid))
                                        ;(signal 'INT)))
        (let ((pid (remember-win:process-pid video-pid)))
          (when (sb-ext:process-kill video-pid 2)
            (stumpwm:message (concatenate 'string
                                  "Stopped the desktop grabbing from the pid ~a~%"
                                  "See the video in ~a file")
                     pid filename)
            (setf video-pid nil)))
        (stumpwm:message "No desktop grabbing happenning to stop.")))

  (stumpwm:defcommand grab-desktop-play () ()
    "Play video in last file"           ;I think it may be better to
                                        ;disable video playing while
                                        ;video grabbing is on.
    (if (and filename (probe-file filename))
        (remember-win:run-wcli-command (format nil "vlc --play-and-exit ~a" filename))
        (progn
          (stumpwm:message "No output file present to play.")
          (setf filename nil))))

  (stumpwm:defcommand grab-desktop-reset-pid () ()
    "Only for debugging purpose, when we are not able to reset video pid"
    (setf video-pid nil))

  (let ((youtubecat '(("Autos & Vehicles" "Autos")
                      ("Comedy" "Comedy")
                      ("Education" "Education")
                      ("Entertainment" "Entertainment")
                      ("Film & Animation" "Film")
                      ("Gaming" "Games")
                      ("Howto & Style" "Howto")
                      ("Music"	"Music")
                      ("News & Politics" "News")
                      ("Nonprofits & Activism" "Nonprofit")
                      ("People & Blogs" "People")
                      ("Pets & Animals" "Animals")
                      ("Science & Technology" "Tech")
                      ("Sports" "Sports")
                      ("Travel & Events" "Travel"))))
    (stumpwm:defcommand grab-desktop-post () ()
      "Post video to YouTube hosting site."
      (if (and filename (probe-file filename))
          (stumpwm:run-shell-command
           (concatenate 'string "google youtube post --category Education ~a" filename)))))

  (setf *desktop-grab-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "\\") "grab-desktop")
          (define-key m (kbd "i")  "grab-desktop-info")
          (define-key m (kbd "p")  "grab-desktop-play")
          (define-key m (kbd "o")  "grab-desktop-post")
          m))

  (define-key stumpwm:*root-map* (kbd "\\") '*desktop-grab-map*))


;;"ffmpeg -f x11grab -s " width "x" hight " -r 24 -i " (getenv "DISPLAY") ".0 -sameq " filename)))

(stumpwm:defcommand display-groups () ()
  (stumpwm:message "~a" (screen-groups (current-screen))))

(stumpwm:defcommand display-urgent-windows () ()
  (stumpwm:message "~a" (screen-urgent-windows (current-screen))))

(stumpwm:defcommand display-screen-info () ()
  (notify (current-screen)))

(stumpwm:defcommand display-screens-info () ()
  (notify (sort-screens)))

(stumpwm:defcommand display-head-info () ()
  (notify (current-head)))

(stumpwm:defcommand display-all-windows () ()
            (stumpwm:message "~a" (all-windows)))

(stumpwm:defcommand display-current-frames () ()
  (let ((group (screen-current-group (current-screen))))
    (notify (tile-group-frame-tree group))))

(stumpwm:defcommand display-current-frame-tree () ()
  (let ((group (screen-current-group (current-screen))))
    (notify (tile-group-frame-tree group))))

(stumpwm:defcommand display-frame-preferences () ()
  (notify *window-placement-rules*))


;; Misc commands --------------------------------------------------------------
(stumpwm:defcommand cd (path) ((:rest "Dir: "))
  (change-dir path))

(stumpwm:defcommand pwd () ()
  (get-current-directory))


(defun emacs-server-running-p ()
  (or (probe-file (concat *home-dir* ".emacs_server"))
      (probe-file (concat "/tmp/" (getenv "UID") "/server"))))

(defun wait-for-program (pgm)
  (dotimes (v 10 nil)
    (let ((c (read-from-string (stumpwm:run-shell-command (concat "pgrep " pgm " | wc -l") t))))
      (if (< c 1)
          (return t)
          (progn
            (stumpwm:message "~a ~a ~a" c pgm v)
            (sleep 2))))))

(defun wait-for-nwprogram (pgm)
  (or (member pgm (get-all-clis)
              :test #'equal
              :key #'(lambda (s)
                       (subseq (string-left-trim " " s) 0
                               (min (length pgm) (length s)))))
      (wait-for-program pgm)))

(progn

  ;; https://stackoverflow.com/questions/8830888/whats-the-canonical-way-to-join-strings-in-a-list
  ;; (defun string-join (separator &rest strings)
  ;;   " "
  ;;   (let ((separator (replace-all "~" "~~" separator)))
  ;;     (format nil
  ;;             (concatenate 'string "~{~a~^" separator "~}")
  ;;             strings)))

  (defun string-join (separator &rest strings)
    (format nil
            (concatenate 'string "~{~a~^" separator "~}")
            strings))

  (defun build-emacslcient-cmd (&rest args)
    (let ((home         (getenv "HOME"))
          (display      (or (getenv "DISPLAY") ":0.0"))
          (emacs-server (or (getenv "EMACS_SERVER_NAME") "general")))
      (let* ((emacs-server-file (concat home "/.emacs.d/server/" emacs-server))
             (emacsclient-cmd   (string-join " " "emacsclient -d " display  "-f" emacs-server-file)))
        (if args
            (apply #'string-join " " emacsclient-cmd args)
            emacsclient-cmd))))

  (stumpwm:defcommand editor () ()
    ;;(if (wait-for-nwprogram "emacsclient")
    (remember-win:run-wcli-command
     (build-emacslcient-cmd "-nc"
                            "-e" "'(setq spec-id \"main\")" )))

  (defun printable-group-name ()
    (prin1-to-string (format nil "~a"
                             (substitute #\_ #\Space (stumpwm::group-name (stumpwm::current-group))))))

  (stumpwm:defcommand emacsclient () ()
    (let ((server-winmgr-req-id (prin1-to-string
                                 (concat "(serve-window-manager-request " (printable-group-name) ")"))))
      (remember-win:run-wcli-command
       (build-emacslcient-cmd "-nc" "-e" server-winmgr-req-id))))

  (stumpwm:defcommand xeditor () ()
    (emacsclient))

  (stumpwm:defcommand mail-reader () ()
    (remember-win:run-wcli-command (build-emacslcient-cmd "-n" "-e" "'(make-mail-chat-frame)'")))

  (stumpwm:defcommand new-mail () ()
    (if (wait-for-program "emacsclient")
        (remember-win:run-wcli-command (build-emacslcient-cmd "-n" "-e" "'(gnus-group-mail)'")))))


(stumpwm:defcommand emacs-gnus () ()
  (if (wait-for-nwprogram "emacsclient")
      (remember-win:run-wcli-command
       (remember-win:run-wcli-command (build-emacslcient-cmd  "-e" "'(gnus)'")))))

(stumpwm:defcommand gnusclient () ()
  (if (wait-for-program "emacsclient")
      (emacs-gnus)))

(stumpwm:defcommand gnusclient () ()
  (if (wait-for-program "emacsclient")
      (emacs-gnus)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from window.lisp
;; (defun xwin-class (win)
;;   (multiple-value-bind (res class) (xlib:get-wm-class win)
;;     (declare (ignore res))
;;     class))
;;
;; (defun xwin-res-name (win)
;;   (multiple-value-bind (res class) (xlib:get-wm-class win)
;;     (declare (ignore class))
;;     res))
;;
;; Implement (defun (setf xwin-class)
;; take help of (xlib:set-wm-class (window-xwin (current-window)) "emacs" "Semacs")
;; could be found in /usr/share/common-lisp/source/clx
;; manager.lisp:68:(defun set-wm-class (window resource-name resource-class)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(stumpwm:defcommand gnome-quit () ()
  (stumpwm:run-shell-command "gconftool-2 --type boolean --set /apps/nautilus/preferences/show_desktop true")
  (remember-win:run-wcli-command
   (concat "gnome-session-save --gui --logout-dialog")))

(stumpwm:defcommand mutt () ()
  (stumpwm:run-or-raise
   "xterm -title mutt -e mutt -y"
   '(:title "mutt")))

(stumpwm:defcommand ebib () ()
  (stumpwm:run-or-raise
   "xterm -title ebib -e emacs -nw -f ebib"
   '(:title "ebib")))


(stumpwm:defcommand pavucontrol () ()
  (remember-win:run-wcli-command (concat "pavucontrol")))


(stumpwm:defcommand chromium () ()
  (remember-win:run-wcli-command "chromium"))

(stumpwm:defcommand epiphany () ()
  (remember-win:run-wcli-command
   (concat "epiphany --profile=" (getenv "HOME") "/.config/epiphany-profiles/" (getenv "XBPROFILE"))))

(stumpwm:defcommand firefox () ()
  (remember-win:run-wcli-command
   (concat "firefox -P " (getenv "XBPROFILE"))))

(stumpwm:defcommand firefox-tor () ()
   (remember-win:run-wcli-command
    (concat "firefox -P tor")))

(stumpwm:defcommand xbrowser () ()
  (remember-win:run-wcli-command
   (concat (or (getenv "XBROWSER")
               (concat (getenv "HOME")
                       "/bin/conkeror-redirected"
                       " -P "
                       (getenv "XBPROFILE"))))))

(stumpwm:defcommand xbrowser-tor () ()
  (remember-win:run-wcli-command "conkeror -P tor"))

(stumpwm:defcommand seamonkey () ()
  (remember-win:run-wcli-command (concat "seamonkey -P "
                                         (getenv "XBPROFILE"))))

(stumpwm:defcommand mozilla () ()
  (remember-win:run-wcli-command (concat "mozilla -P "
                                         (getenv "XBPROFILE"))))

(stumpwm:defcommand conkeror () ()
  (remember-win:run-wcli-command (concat "firefox -P "
                                         (getenv "XBPROFILE"))))

(stumpwm:defcommand virt-manager () ()
  "virt-manager"
  (remember-win:run-wcli-command "virt-manager"))

(stumpwm:defcommand w3m () ()
   (stumpwm:run-or-raise
    "xterm -title w3m -tn xterm -e w3m -cookie -config /home/m0rg/.w3m/config -N http://www.google.fr"
    '(:title "w3m")))

;; (stumpwm:defcommand xbrowser () ()
;;    (stumpwm:run-or-raise
;;     (getenv "XBROWSER")
;;     '(:class "Mozilla")))

(stumpwm:defcommand slrn () ()
   (stumpwm:run-or-raise
    "xterm -title slrn -e slrn -f /home/m0rg/.newsrc"
    '(:title "slrn")))

(stumpwm:defcommand cmus () ()
   (stumpwm:run-or-raise
    "xterm -title cmus -e cmus"
    '(:title "cmus 2.2.0")))

(stumpwm:defcommand ssh-to-intranet () ()
   (stumpwm:run-or-raise
    "xterm -title Intranet -e ssh $INTRANETSERVER"
    '(:title "Intranet")))

(stumpwm:defcommand inkscape () ()
   (stumpwm:run-or-raise
    "inkscape"
    '(:class "Inkscape")))

(stumpwm:defcommand gimp () ()
   (stumpwm:run-or-raise
    "gimp"
    '(:class "Gimp")))

;;fectchmail
(stumpwm:defcommand fetchmail () ()
  (stumpwm:run-shell-command "fetchmail -d0")
  (stumpwm:message "Checking mails..."))
(stumpwm:defcommand fetchmail-daemon () ()
  (stumpwm:run-shell-command "fetchmail"))
(stumpwm:defcommand fetchmail-kill-daemon () ()
  (stumpwm:run-shell-command "fetchmail -q"))


;;unison synchronization
(stumpwm:defcommand unison-synchronization (host) ((:rest "Synchro host: "))
  (stumpwm:run-shell-command (concat *home-dir*
                             "/bin/synchro "
                             host)))


(stumpwm:defcommand paste () ()
  (let ((text (get-x-selection 1)))
    (and text (window-send-string text))))


;;screenshot-to-website
(stumpwm:defcommand screenshot-to-website (filename) ((:rest "Filename: "))
  (stumpwm:run-shell-command
   (format nil
           "import -window root /home/s/hell/code/html/patzy/screenshots/~a"
           filename)))

;;screenshot-to-file
(stumpwm:defcommand screenshot-to-file (filename) ((:rest "Filename: "))
  (stumpwm:run-shell-command
   (format nil "import -window root ~a" filename)))


(stumpwm:defcommand restore-group-dump (filename) ((:rest "Dump name: "))
  (let ((group (add-group (current-screen) filename)))
    (switch-to-group group)
    (restore-from-file (data-dir-file filename))))

(stumpwm:defcommand save-group-dump (filename) ((:rest "Dump name: "))
  (dump-group-to-file (data-dir-file filename)))


;;Termsn
(defun lotus-group-name-string ()
  ;; (substitute #\_ #\Space (prin1-to-string (group-name (current-group))))
  (prin1-to-string (group-name (current-group))))

(defun lotus-group-name-string-title ()
  (format nil " -T ~a" (lotus-group-name-string)))

(defun lotus-terminal-command-with-group-name-title (term-cmd)
  (concatenate 'string
               term-cmd
               (format nil " -T ~a" (lotus-group-name-string))))

(stumpwm:defcommand kitty () ()
  (remember-win:run-wcli-command (lotus-terminal-command-with-group-name-title "kitty")))

(stumpwm:defcommand sakura () ()
  (remember-win:run-wcli-command (lotus-terminal-command-with-group-name-title "sakura")))

(stumpwm:defcommand urxvt () ()
  (remember-win:run-wcli-command (lotus-terminal-command-with-group-name-title "urxvt")))

(stumpwm:defcommand xterm () ()
  (remember-win:run-wcli-command (lotus-terminal-command-with-group-name-title "xterm")))

(stumpwm:defcommand mrxvt (&optional title) ((:rest "title: "))
  (remember-win:run-wcli-command (lotus-terminal-command-with-group-name-title "mrxvt")))

(stumpwm:defcommand xscreen () ()
  (remember-win:run-wcli-command "xterm -e screen"))


;;Google calendar
(stumpwm:defcommand gcal-week () ()
  (stumpwm:message "~a"
           (stumpwm:run-shell-command (concat "gcalcli calw 1  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))
(stumpwm:defcommand gcal-month () ()
  (stumpwm:message "~a"
           (stumpwm:run-shell-command (concat "gcalcli calm  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))


(stumpwm:defcommand gcal-search (search-string) ((:rest "Search gcal: "))
  (stumpwm:message "~a"
           (stumpwm:run-shell-command (concat "gcalcli "
                                      "--ignore-started --details search \""
                                      search-string
                                      "\""
                                      " | "
                                      "sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))

(stumpwm:defcommand gcal-add-event (evt) ((:rest "Event:"))
  (stumpwm:run-shell-command (format nil (concat "gcalcli quick '~a'~%")
                             evt))
  (stumpwm:message "Added event: ~a" evt))


;; cmus-remote control
(stumpwm:defcommand cmus-play () ()
  (stumpwm:run-shell-command "cmus-remote -p"))
(stumpwm:defcommand cmus-stop () ()
  (stumpwm:run-shell-command "cmus-remote -s"))
(stumpwm:defcommand cmus-next () ()
  (stumpwm:run-shell-command "cmus-remote -n"))
(stumpwm:defcommand cmus-prev () ()
  (stumpwm:run-shell-command "cmus-remote -r"))


(stumpwm:defcommand restart-conky () ()
  (stumpwm:run-shell-command "killall conky ; conky -d -c ~/.conkyrc/main/conkyrc 2>&1 >/dev/null"))

(stumpwm:defcommand conky () ()
  (stumpwm:run-shell-command "conky -d -c ~/.conkyrc/main/conkyrc"))


(stumpwm:defcommand file-manager () ()
  ;; (format "nautilus --no-default-window --no-desktop %s" dir)
  ;; (remember-win:run-wcli-command "nautilus --no-default-window --no-desktop")
  (remember-win:run-wcli-command "nautilus --no-default-window"))

(stumpwm:defcommand file-manager-quit () ()
  (stumpwm:run-shell-command "nautilus -q"))


(stumpwm:defcommand toggle-touchpad () ()
  "Toggle the laptop touchpad on/off.
   Need to have set 'Option SHMConfig' for Synaptics Touchpad
   device in xorg.conf."
  (let ((state (stumpwm:run-shell-command "synclient -l | grep TouchpadOff | awk '{ print $3 }'" t)))
    (if (string= (subseq state 0 1) "1")
        (stumpwm:run-shell-command "synclient TouchpadOff=0")
        (progn
          (stumpwm:run-shell-command "synclient TouchpadOff=1")
          (banish-pointer)))))

(stumpwm:defcommand refocus-conkeror () ()
  ;; from: http://bc.tech.coop/ubuntu-config/.stumpwmrc
  "Re-focus the conkeror buffer.
   Useful when you want to escape Flash without a mouse."
  (shell-command "conkeror -batch -e 'if (w=window_watcher.activeWindow) {
                     unfocus(w.buffers.current);
                     w.minibuffer.message(\"focus regained\");
                   }'"))

(define-key stumpwm:*root-map* (kbd "X") "refocus-conkeror")


;; (debug-sleep)


(stumpwm:defcommand lock-stumpwm () ()
  (eval-command "exec xautolock -locknow" t))

(stumpwm:defcommand bye () ()
  #+pa
  (in.net.sharad.pa-backend-emacs-planner::emacs-eval-nooutput "(close-all-frames)")
  (sleep 1)
  (stumpwm:run-shell-command (concat (getenv "HOME") "/.rsetup/wmlogout/run"))
  (quit))

(stumpwm:defcommand bye-with-cleanup () ()
            (bye))

(stumpwm:defcommand bye-with-confirmation () ()
  (let ((*message-window-gravity* :center))
    (fclear)
    (unwind-protect
         (if (y-or-n-p "^5^BLogout from stumpwm:^b ^2")
             (bye-with-cleanup))
      (progn
        (pull-hidden-other)))))

(stumpwm:defcommand sys-halt () ()
  (cond ((= 1 (length (complete-program "systemctl")))
         (stumpwm:run-shell-command "systemctl poweroff"))
        ((= 1 (length (complete-program "herd")))
         (stumpwm:run-shell-command "pkexec herd power-off shepherd"))))

(stumpwm:defcommand sys-poweroff () ()
  (sys-halt))

(stumpwm:defcommand sys-suspend () ()
  (cond ((= 1 (length (complete-program "systemctl")))
         (stumpwm:run-shell-command "systemctl suspend"))
        ((= 1 (length (complete-program "herd")))
         (stumpwm:run-shell-command "pkexec herd suspend shepherd"))))

(stumpwm:defcommand sys-suspend-then-hibernate () ()
  (cond ((= 1 (length (complete-program "systemctl")))
         (stumpwm:run-shell-command "systemctl suspend-then-hibernate"))
        ((= 1 (length (complete-program "herd")))
         (stumpwm:run-shell-command "pkexec herd suspend-then-hibernate shepherd"))))

(stumpwm:defcommand sys-hibernate () ()
  (cond ((= 1 (length (complete-program "systemctl")))
         (stumpwm:run-shell-command "systemctl hibernate"))
        ((= 1 (length (complete-program "herd")))
         (stumpwm:run-shell-command "pkexec herd hibernate shepherd"))))

(stumpwm:defcommand sys-reboot () ()
  ;; (stumpwm:run-shell-command "reb00t")
  (cond ((= 1 (length (complete-program "systemctl")))
         (stumpwm:run-shell-command "systemctl reboot"))
        ((= 1 (length (complete-program "herd")))
         (stumpwm:run-shell-command "pkexec herd reboot shepherd"))))


(defun generate-menu (menu &key (fclear nil) (gravity :center))
  (labels ((pick (options)
             (let ((selection (stumpwm::select-from-menu (stumpwm::current-screen)
                                                         options
                                                         "Exit Menu:")))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 ((stringp (second selection))
                  (second selection))
                 (t
                  (pick (cdr selection)))))))
    (let ((*message-window-gravity* gravity))
      (when fclear
        (fclear))
      (unwind-protect
           (let* ((lmenu (mapcar #'(lambda (item)
                                     (let* ((item    (if (consp item) item (cons item item)))
                                            (heading (car  item))
                                            (value   (cadr item)))
                                       (list (concat "^5^B" heading "^b ^2") value)))
                                 menu))
                  (cmd   (pick lmenu)))
             (when (plusp (length cmd))
               (eval-command cmd t)))
        (when fclear
          (pull-hidden-other))))))


(stumpwm:defcommand ctr-alt-del () ()
  (generate-menu '(("Lock"                   "lock-stumpwm")
                   ("Logout"                 "bye-with-confirmation")
                   ("Logout Now"             "bye")
                   ;; ("Halt"                   "sys-halt")
                   ("Poweroff"               "sys-poweroff")
                   ("Suspend"                "sys-suspend")
                   ("Suspend then hibernate" "sys-suspend-then-hibernate")
                   ("Hibernate"              "sys-hibernate")
                   ("Reboot"                 "sys-reboot"))
                 :fclear  t
                 :gravity :center))


(stumpwm:defcommand start-wm-components () ()
  (stumpwm:message "started start-wm-components")
  (prog1
      ;; (stumpwm:run-shell-command
      ;;  (concat (getenv "HOME") "/.rsetup/x/run"))
      (stumpwm:run-shell-command
       (concat (getenv "HOME") "/.rsetup/wmlogin/run"))
    (stumpwm:message "done start-wm-components")))

(stumpwm:defcommand start-wm-test-components () ()
  (stumpwm:message "started start-wm-test-components")
  (prog1
      (stumpwm:run-shell-command
       (concat (getenv "HOME") "/tmp/test.sh"))
    (stumpwm:message "done start-wm-test-components")))


(stumpwm:defcommand display-top-map () ()
  (display-bindings-for-keymaps nil *top-map*))

(defun xwin-kill (window)
  "Kill the client associated with window."
  (dformat 3 "Kill client~%")
  (xlib:kill-client *display* (xlib:window-id window)))


#+ql
(progn
  ;; http://theatticlight.net/posts/Stumpwm-shutdown-nag/
  (ql:quickload 'cl-cron)
  (require 'cl-cron)
  (use-package 'cl-cron))

;; (no-error
;;     (require 'cl-xyzfail)
;;  )
(when nil
  (progn
    ;; http://www.cliki.net/CPS
    ;; http://dunsmor.com/lisp/onlisp/onlisp_24.html
    ;;
    (require 'cl-cont)
    (use-package 'cl-cont)))


;; (debug-sleep)


(stumpwm:defcommand gnext-nonempty () ()
  )

(stumpwm:defcommand gprev-nonempty () ()
  )


(stumpwm:defcommand env (&optional (var t)) ((:string "env var: "))
  (let ((var (if (string-equal "" var) t var)))
    (if (eq var t)
        (let ((env-vars (sb-ext:posix-environ)))
          (stumpwm:message "Environment[ ~a ]:~%~:{~a: ~a~%~}"
                   (length env-vars)
                   (let ((i 0))
                     (mapcar #'(lambda (e)
                                 (incf i)
                                 (list i e))
                             env-vars))))
        (let* ((var (format nil "~a" var))
               (value (getenv var))
               (changed-value
                 (read-one-line (current-screen)
                                (format nil "env[~a]: " var)
                                :initial-input value)))
          (if (and value
                   (stringp value)
                   (apply #'string-equal
                          (mapcar #'(lambda (str) (string-trim '(#\Space #\Tab #\Newline) str))
                                  (list value changed-value))))
              (stumpwm:message "env[~a] unchanged" var)
              (if (setf (getenv var) changed-value)
                  (stumpwm:message "env[~a]: ~a" var changed-value)))))))


(stumpwm:defcommand find-cousor () ()
  (stumpwm:run-shell-command "~/bin/find-cursor --color black")
  (stumpwm:run-shell-command "~/bin/find-cursor --color white"))


;; no window command
(stumpwm:defcommand cpy-pass () ()
  (let ((sec-cmd "secret-tool lookup server exch-cas.fortinet.com user 'fortinet-us\\spratap' protocol imap")
        (clip-cmd "xclip"))
    (stumpwm:run-shell-command (concat sec-cmd " | " clip-cmd))
    (sleep 1)
    (paste)))


(stumpwm:defcommand abort-recursive-edit () ()
  (remember-win:run-cli-command (build-emacslcient-cmd  "-e" "'(abort-recursive-edit)'")))


;; https://www.reddit.com/r/qtools/comments/aeu2qn/rofi_does_not_open_terminal_in_i3wm/
;; https://wiki.archlinux.org/index.php/Dmenu
;; https://bbs.archlinux.org/viewtopic.php?id=80145
;; https://github.com/cdown/clipmenu/blob/develop/clipmenud
;; https://wiki.archlinux.org/index.php/Rofi

(stumpwm:defcommand run-rofi-win () ()
  (stumpwm:run-shell-command "rofi -show run"))

(stumpwm:defcommand run-rofi-desktop () ()
  (stumpwm:run-shell-command "rofi -show drun"))

(stumpwm:defcommand run-rofi-terminal-wait () ()
  (stumpwm:run-shell-command "rofi -show run -terminal xterm -kb-accept-entry '' -kb-accept-alt Return -run-shell-command '{terminal} -e \"{cmd}; bash -c \\\"read -n 1 \\\" \" ' "))

(stumpwm:defcommand run-rofi-terminal-nowait () ()
  (stumpwm:run-shell-command "rofi -show run -terminal xterm -kb-accept-entry \"\" -kb-accept-alt Return -run-shell-command '{terminal} -e \"{cmd}\"'"))

(stumpwm:defcommand run-rofi-xscreen-session () ()
  (stumpwm:run-shell-command "rofi -modi xscreen:~/.setup/binx/rofi/screenssh.sh -show xscreen"))

;; TODO: universal menu for all KEYMAPS

