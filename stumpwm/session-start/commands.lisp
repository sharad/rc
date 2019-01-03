;; -*-lisp-*-
;;
;; Window managment ------------------------------------------------------------

(in-package :stumpwm)



;;{{{ Notification system

(defun msg-notify (fmt args)
  (let ((*executing-stumpwm-command* nil)
        (*message-window-gravity* :center))
    (message-no-timeout fmt args)))

(defcommand notify (msg) ((:rest "Notify: "))
  (msg-notify "~a" msg))
;;}}}



;; #+pb (fboundp 'stumpwm::run-cli-command)

(defcommand fnext () ()
  (focus-next-frame (current-group)))
(defcommand fprev () ()
  (focus-prev-frame (current-group)))

;; Aquasole
;; OSD text
(defcommand osd-echo () ()
  (let* ((screen (current-screen))
         (w (window-xwin (screen-focus screen))))
    (echo-in-window w (screen-font screen)
                    (screen-fg-color screen)
                    (screen-bg-color screen)
                    "Test de l'osd"))
    (xlib:display-finish-output *display*))

(defcommand hsbalance-frames () ()
            "sdfdsf"
            (only)
            (dotimes (c (1- (length (group-windows (current-group))))
                      (balance-frames))
              (hsplit)))

(defcommand vsbalance-frames () ()
            "sdfdsf"
            (only)
            (dotimes (c (1- (length (group-windows (current-group))))
                      (balance-frames))
              (vsplit)))

;; menu test
(defcommand test-menu () ()
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
  (defcommand grab-desktop-info () ()
    "Get video grabbing process pid"
    (let* ((pid (if (and video-pid (sb-ext:process-alive-p video-pid))
                    (process-pid video-pid)))
           (file (if (and filename (probe-file filename))
                     filename))
           filesize)
      (if pid
          (message "Desktop grabbing process working with pid: ~a.~@[~&Outputting in ~a~]" pid file)
          (message "No desktop grabbing is going on in my knowledge.~@[~&But previous recording could be found in ~a~]" file))))

  (defcommand grab-desktop () ();(&optional filearg) ((:rest "Filename: "))
      (if (and video-pid (sb-ext:process-alive-p video-pid))
          (grab-desktop-stop)
          (grab-desktop-start (read-one-line (current-screen) "Filename: " :initial-input "/tmp/out.flv"))))

  (defcommand grab-desktop-start (&optional filearg) ((:rest "Filename: "))
    (if (and video-pid (sb-ext:process-alive-p video-pid))
        (message
         "Already desktop grabber is running with pid: ~a~&outputting into ~a"
         (process-pid video-pid) filename)
        (let* ((filearg (or filearg "/tmp/out.mpeg"))
                                        ;(width (run-shell-command "xrandr | grep 'Screen 0' | awk '{ printf \"%s\", $8 }'" t))
                                        ;(hight (run-shell-command "xrandr | grep 'Screen 0' | awk '{ printf \"%s\", $10 }' | sed 's/,.*//'" t))
               (geometry (run-shell-command "xwininfo -root | grep 'geometry' | awk '{printf \"%s\", $2;}'" t))
               (depth    (run-shell-command "xwininfo -root | grep -i 'Depth' | awk '{printf \"%s\", $2;}'" t))
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
          (when (setf video-pid (run-cli-command capture-cmd))
            (setf filename filearg)
            (message "Your pid is ~a with cmd ~a" (process-pid video-pid) capture-cmd)))))

  ;;(message-no-timeout capture-cmd))))

  (defcommand grab-desktop-stop () ()
    ; shuld offer to play the last recordind, control it by user variable.
    (if (and video-pid
             (sb-ext:process-alive-p video-pid))
                                        ;(signal 'INT)))
        (let ((pid (process-pid video-pid)))
          (when (sb-ext:process-kill video-pid 2)
            (message (concatenate 'string
                                  "Stopped the desktop grabbing from the pid ~a~%"
                                  "See the video in ~a file")
                     pid filename)
            (setf video-pid nil)))
        (message "No desktop grabbing happenning to stop.")))

  (defcommand grab-desktop-play () ()
    "Play video in last file"           ;I think it may be better to
                                        ;disable video playing while
                                        ;video grabbing is on.
    (if (and filename (probe-file filename))
        (run-wcli-command (format nil "vlc --play-and-exit ~a" filename))
        (progn
          (message "No output file present to play.")
          (setf filename nil))))

  (defcommand grab-desktop-reset-pid () ()
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
    (defcommand grab-desktop-post () ()
      "Post video to YouTube hosting site."
      (if (and filename (probe-file filename))
          (run-shell-command
           (concatenate 'string "google youtube post --category Education ~a" filename)))))

  (setf *desktop-grab-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "\\") "grab-desktop")
          (define-key m (kbd "i")  "grab-desktop-info")
          (define-key m (kbd "p")  "grab-desktop-play")
          (define-key m (kbd "o")  "grab-desktop-post")
          m))

  (define-key *root-map* (kbd "\\") '*desktop-grab-map*))

                                 ;;"ffmpeg -f x11grab -s " width "x" hight " -r 24 -i " (getenv "DISPLAY") ".0 -sameq " filename)))

(defcommand display-groups () ()
  (message "~a" (screen-groups (current-screen))))

(defcommand display-urgent-windows () ()
  (message "~a" (screen-urgent-windows (current-screen))))

(defcommand display-screen-info () ()
  (notify (current-screen)))

(defcommand display-screens-info () ()
  (notify (sort-screens)))

(defcommand display-head-info () ()
  (notify (current-head)))

(defcommand display-all-windows () ()
            (message "~a" (all-windows)))

(defcommand display-current-frames () ()
  (notify (tile-group-frame-tree (screen-current-group (current-screen)))))

(defcommand display-current-frame-tree () ()
  (notify (tile-group-frame-tree (screen-current-group (current-screen)))))

(defcommand display-frame-preferences () ()
  (notify *window-placement-rules*))

;; Misc commands --------------------------------------------------------------

;; (defun change-dir (path)
;;   (setf *default-pathname-defaults* path)
;;   #+(and clisp linux) (linux:|chdir| (truename path))
;;   #+(and sbcl sb-posix) (sb-posix:|chdir| (truename path)))
;;   ;; #+(and clisp linux) (linux:|chdir| (namestring (truename path)))
;;   ;; #+(and sbcl sb-posix) (sb-posix:|chdir| (namestring (truename path))))

;; (defcommand cd (path) ((:rest "Dir: "))
;;             (change-dir path))
  ;; #+(and clisp linux) (linux:|chdir| (namestring (truename path)))
  ;; #+(and sbcl sb-posix) (sb-posix:|chdir| (namestring (truename path))))

(defcommand cd (path) ((:rest "Dir: "))
  (change-dir path))

(defcommand pwd () ()
  (get-current-directory))

(defun emacs-server-running-p ()
  (or (probe-file (concat *home-dir*
                          ".emacs_server"))
      (probe-file (concat "/tmp/" (getenv "UID") "/server"))))


;; (defcommand emacsclient () ()
;;   (run-or-raise
;;    (concat "emacsclient -d " (getenv "DISPLAY") " -c " "-f " (concat (getenv "DISPLAY") "/emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")))
;;    '(:class "Emacs")))

(defun wait-for-program (pgm)
    (dotimes (v 10 nil)
      (let ((c (read-from-string (run-shell-command (concat "pgrep " pgm " | wc -l") t))))
        (if (< c 1)
            (return t)
            (progn
              (message "~a ~a ~a" c pgm v)
              (sleep 2))))))

(defun wait-for-nwprogram (pgm)
  (or (member pgm (get-all-clis)
                  :test #'equal
                  :key #'(lambda (s)
                           (subseq (string-left-trim " " s) 0 (min (length pgm) (length s)))))
      (wait-for-program pgm)))

#-pa
(progn
  (defcommand editor () ()
              ;;(if (wait-for-nwprogram "emacsclient")
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")) " -e '(setq spec-id \"main\")")
               ;; '(:class "Emacs")
               ))

  (defcommand xeditor () ()
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general"))
                       " -e "
                       (prin1-to-string
                        (concat "(serve-window-manager-request "
                                (prin1-to-string (format nil "~a" (substitute #\_ #\Space (stumpwm::group-name (stumpwm::current-group)))))
                                ")")))
               ;; '(:class "Emacs")
               ))

  (defcommand emacsclient () ()
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general"))
                       " -e "
                       (prin1-to-string
                        (concat "(serve-window-manager-request "
                                (prin1-to-string (format nil "~a" (substitute #\_ #\Space (stumpwm::group-name (stumpwm::current-group)))))
                                ")")))
               ;; '(:class "Emacs")
               ))

  (defcommand mail-reader () ()
              (run-wcli-command
               (concat "emacsclient -n -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e (make-mail-chat-frame)"))))

  (defcommand new-mail () ()
              (if (wait-for-program "emacsclient")
                  (run-wcli-command
                   (concat "emacsclient -n -c -d " (getenv "DISPLAY")  " -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus-group-mail)'"))))))

(defcommand gnus () ()
  (if (wait-for-nwprogram "emacsclient")
      (run-wcli-command
       (concat "emacsclient -d " (getenv "DISPLAY")  "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus)'"))
       '(:class "EmacsGNU"))))


;; (defcommand mail-reader () ()
;;   (if (wait-for-program "emacsclient")
;;   (run-wcli-command
;;    (concat "emacsclient -n -c -d " (getenv "DISPLAY")  " -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus)'")))))


(defcommand gnusclient () ()
  (if (wait-for-program "emacsclient")
  (run-or-pull
   (concat "emacsclient -d " (getenv "DISPLAY") " -c " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")) " -e '(gnus)'")
   '(:class "EmacsGNUS"))))

;; run-wcli-command

(defcommand gnusclient () ()
  (if (wait-for-program "emacsclient")
  (run-wcli-command
   (concat "emacsclient -d " (getenv "DISPLAY") " -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")) " -e '(gnus)'"))))

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

(defcommand gnome-quit () ()
  (run-shell-command "gconftool-2 --type boolean --set /apps/nautilus/preferences/show_desktop true")
  (run-wcli-command
   (concat "gnome-session-save --gui --logout-dialog")))

;; no use.
;; (defcommand emacsclient-cli () ()
;;   (run-wcli-command
;;    (concat "emacsclient -d " (getenv "DISPLAY") " -c ")))


;; (defcommand manuscrit () ()
;;    (run-shell-command "gv /home/m0rg/these/manuscrit/these.ps"))

(defcommand mutt () ()
   (run-or-raise
    "xterm -title mutt -e mutt -y"
    '(:title "mutt")))

(defcommand ebib () ()
  (run-or-raise
   "xterm -title ebib -e emacs -nw -f ebib"
    '(:title "ebib")))

;; (defcommand ebib () ()
;;   (run-or-raise-cli
;;    (if (emacs-server-running-p)
;;        "emacsclient -d ${DISPLAY} -e \"(open-ebib-in-new-frame)\""
;;        "emacs -d ${DISPLAY} -f \"(let ((ebib-layout 'full))
;;                                      (ebib))\"")
;;     '(:title "ebib")))

(defcommand firefox () ()
   (run-wcli-command
    (concat "firefox -P " (getenv "XBPROFILE"))))

(defcommand firefox-tor () ()
   (run-wcli-command
    (concat "firefox -P tor")))

(defcommand xbrowser () ()
            (run-wcli-command
             (concat
              (or (getenv "XBROWSER")
                  (concat
                   (getenv "HOME") "/bin/conkeror-redirected" " -P " (getenv "XBPROFILE"))))))

(defcommand xbrowser-tor () ()
            (run-wcli-command "conkeror -P tor"))

(defcommand seamonkey () ()
   (run-wcli-command
    (concat "seamonkey -P " (getenv "XBPROFILE"))))

(defcommand mozilla () ()
   (run-wcli-command
    (concat "mozilla -P " (getenv "XBPROFILE"))))

(defcommand conkeror () ()
   (run-wcli-command
    (concat "firefox -P " (getenv "XBPROFILE"))))

(defcommand virt-manager () ()
            "virt-manager"
  (run-wcli-command "virt-manager"))

(defcommand w3m () ()
   (run-or-raise
    "xterm -title w3m -tn xterm -e w3m -cookie -config /home/m0rg/.w3m/config -N http://www.google.fr"
    '(:title "w3m")))

;; (defcommand xbrowser () ()
;;    (run-or-raise
;;     (getenv "XBROWSER")
;;     '(:class "Mozilla")))

(defcommand slrn () ()
   (run-or-raise
    "xterm -title slrn -e slrn -f /home/m0rg/.newsrc"
    '(:title "slrn")))

(defcommand cmus () ()
   (run-or-raise
    "xterm -title cmus -e cmus"
    '(:title "cmus 2.2.0")))

(defcommand ssh-to-intranet () ()
   (run-or-raise
    "xterm -title Intranet -e ssh $INTRANETSERVER"
    '(:title "Intranet")))

(defcommand inkscape () ()
   (run-or-raise
    "inkscape"
    '(:class "Inkscape")))

(defcommand gimp () ()
   (run-or-raise
    "gimp"
    '(:class "Gimp")))

;;fectchmail
(defcommand fetchmail () ()
  (run-shell-command "fetchmail -d0")
  (message "Checking mails..."))
(defcommand fetchmail-daemon () ()
  (run-shell-command "fetchmail"))
(defcommand fetchmail-kill-daemon () ()
  (run-shell-command "fetchmail -q"))

;;unison synchronization
(defcommand unison-synchronization (host) ((:rest "Synchro host: "))
   (run-shell-command (concat *home-dir*
                              "/bin/synchro "
                              host)))

(defcommand paste () ()
            (let ((text (get-x-selection 1)))
              (and text (window-send-string text))))


;;screenshot-to-website
(defcommand screenshot-to-website (filename) ((:rest "Filename: "))
  (run-shell-command
   (format nil
           "import -window root /home/s/hell/code/html/patzy/screenshots/~a"
           filename)))

;;screenshot-to-file
(defcommand screenshot-to-file (filename) ((:rest "Filename: "))
  (run-shell-command
   (format nil "import -window root ~a" filename)))

(defcommand restore-group-dump (filename) ((:rest "Dump name: "))
 (let ((group (add-group (current-screen) filename)))
   (switch-to-group group)
   (restore-from-file (data-dir-file filename))))

(defcommand save-group-dump (filename) ((:rest "Dump name: "))
 (dump-group-to-file (data-dir-file filename)))

;;Termsn
;; (dolist '(term (xterm urxvt mrxvt))
;;         (defcommand term (&optional title) ((:rest "title: "))
;;                     (run-wcli-command (concatenate 'string (symbole-name term)
;;                                                   (if title (format nil " -T ~a" title))))))

;;(run-shell-command "xterm"))

;; (defcommand xterm () ()
;;             (run-wcli-command "xterm"))

;;Termsn
;;Termsn
(defcommand xterm (&optional title) ((:rest "title: "))
            (run-wcli-command (concatenate 'string "xterm"
                                          (if title (format nil " -T ~a" title)))))

;; ;;Termsn *emacs-planner-tasks*
;; (defcommand urxvt (&optional title) ((:rest "title: "))
;;             (run-wcli-command (concatenate 'string "urxvtc"
;;                                           (if title (format nil " -T ~a" title)))))
(defcommand urxvt () ()
  (run-wcli-command (concatenate 'string "urxvt" ;; "urxvtc"
                                 (format nil " -T ~a"
                                         (substitute #\_ #\Space (prin1-to-string (group-name (current-group))))))))

(defcommand urxvt () ()
  (run-wcli-command (concatenate 'string "urxvt" ;; "urxvtc"
                                 ;; (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
                                 ;;   (if (probe-file paradise)
                                 ;;       (concatenate 'string " -cd " paradise " ")
                                 ;;       ""))
                                 (format nil " ~a-T ~a"
                                         (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
                                           (if (probe-file paradise)
                                               (concatenate 'string " -cd " paradise " ")
                                               " "))
                                         (substitute #\_ #\Space (prin1-to-string (group-name (current-group))))))))

(defcommand xterm () ()
  (run-wcli-command (concatenate 'string "xterm"
                                 (format nil " -T ~a"
                                         (substitute #\_ #\Space (prin1-to-string (group-name (current-group))))))))

;; (testing
;;   (cl-ppcre:split " " (concatenate 'string "urxvtc"
;;                                    ;; (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
;;                                    ;;   (if (probe-file paradise)
;;                                    ;;       (concatenate 'string " -cd " paradise " ")
;;                                    ;;       ""))
;;                                    (format nil "~a-T ~a"
;;                                            (let ((paradise (concatenate 'string (getenv "HOME") "/../paradise/")))
;;                                              (if (probe-file paradise)
;;                                                  (concatenate 'string " -cd " paradise " ")
;;                                                  " "))
;;                                            (substitute #\_ #\Space (group-name (current-group)))))))

(defcommand mrxvt (&optional title) ((:rest "title: "))
            (run-wcli-command (concatenate 'string "mrxvt"
                                          (if title (format nil " -title ~a" title)))))

(defcommand xscreen () ()
  (run-wcli-command "xterm -e screen"))
;;  (run-shell-command "xterm -e screen"))


;;Google calendar
(defcommand gcal-week () ()
  (message "~a"
           (run-shell-command (concat "gcalcli calw 1  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))
(defcommand gcal-month () ()
  (message "~a"
           (run-shell-command (concat "gcalcli calm  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))


(defcommand gcal-search (search-string) ((:rest "Search gcal: "))
  (message "~a"
           (run-shell-command (concat "gcalcli "
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

(defcommand gcal-add-event (evt) ((:rest "Event:"))
  (run-shell-command (format nil (concat "gcalcli quick '~a'~%")
                             evt))
  (message "Added event: ~a" evt))


;; cmus-remote control
(defcommand cmus-play () ()
  (run-shell-command "cmus-remote -p"))
(defcommand cmus-stop () ()
  (run-shell-command "cmus-remote -s"))
(defcommand cmus-next () ()
  (run-shell-command "cmus-remote -n"))
(defcommand cmus-prev () ()
  (run-shell-command "cmus-remote -r"))


(defcommand restart-conky () ()
   (run-shell-command
    "killall conky ; conky -d -c ~/.conkyrc/main/conkyrc 2>&1 >/dev/null"))

(defcommand conky () ()
   (run-shell-command
    "conky -d -c ~/.conkyrc/main/conkyrc"))

(defcommand toggle-touchpad () ()
  "Toggle the laptop touchpad on/off.
   Need to have set 'Option SHMConfig' for Synaptics Touchpad
   device in xorg.conf."
  (let ((state (run-shell-command
		"synclient -l | grep TouchpadOff | awk '{ print $3 }'" t)))
    (if (string= (subseq state 0 1) "1")
      (run-shell-command "synclient TouchpadOff=0")
      (progn
        (run-shell-command "synclient TouchpadOff=1")
        (banish-pointer)))))

(defcommand refocus-conkeror () ()
  ;; from: http://bc.tech.coop/ubuntu-config/.stumpwmrc
  "Re-focus the conkeror buffer.
   Useful when you want to escape Flash without a mouse."
  (shell-command "conkeror -batch -e 'if (w=window_watcher.activeWindow) {
                     unfocus(w.buffers.current);
                     w.minibuffer.message(\"focus regained\");
                   }'"))

(define-key *root-map* (kbd "X") "refocus-conkeror")

(defcommand sys-halt () ()
  (run-shell-command "poweroff"))

(defcommand sys-suspend () ()
  (run-shell-command "pmi action suspend"))

(defcommand sys-hibernate () ()
  (run-shell-command "pmi action hibernate"))

(defcommand sys-reboot () ()
  (run-shell-command "reb00t"))

(defcommand start-wm-components () ()
 (message "started start-wm-components")
 (prog1
     (run-shell-command
      (concat
       (getenv "HOME")
       "/.rsetup/wmlogin/run"))
   (message "done start-wm-components")))


(defcommand start-wm-test-components () ()
  (message "started start-wm-test-components")
  (prog1
      (run-shell-command
       (concat
        (getenv "HOME")
        "/tmp/test.sh"))
    (message "done start-wm-test-components")))

(defcommand bye () ()
#+pa
  (in.net.sharad.pa-backend-emacs-planner::emacs-eval-nooutput "(close-all-frames)")
  (sleep 1)
  (run-shell-command
   (concat
    (getenv "HOME")
    "/.rsetup/wmlogout/run"))
  (quit))

(defcommand bye-with-cleanup () ()
            (bye))

(defcommand bye-with-confirmation () ()
            (let ((*message-window-gravity* :center))
              (fclear)
              (if (y-or-n-p "^5^BLogout from stumpwm:^b ^2")
                  (bye-with-cleanup)
                  (pull-hidden-other))))

(defcommand display-top-map () ()
    (display-bindings-for-keymaps  nil *top-map*))

(defun xwin-kill (window)
  "Kill the client associated with window."
  (dformat 3 "Kill client~%")
  (xlib:kill-client *display* (xlib:window-id window)))



(progn
  ;; http://theatticlight.net/posts/Stumpwm-shutdown-nag/
  (ql:quickload 'cl-cron)
  (require 'cl-cron)
  (use-package 'cl-cron)
  )

;; (no-error
;;     (require 'cl-xyzfail)
;;  )
(when nil
(progn
 ;; http://www.cliki.net/CPS
 ;; http://dunsmor.com/lisp/onlisp/onlisp_24.html
 ;;
 (require 'cl-cont)
 (use-package 'cl-cont))
)

(defcommand gnext-nonempty () ()
            )

(defcommand gprev-nonempty () ()
            )

(defcommand env (&optional (var t)) ((:string "env var: "))
  (if (eq var t)
      (message "all env")
      (let* ((var (format nil "~a" var))
             (value (getenv var))
             (changed-value
              (read-one-line (current-screen)
                             (format nil "env[~a]: " var)
                             :initial-input value)))
        (if (and
             value
             (stringp value)
             (apply
              #'string-equal
              (mapcar
               #'(lambda (str) (string-trim '(#\Space #\Tab #\Newline) str))
               (list value changed-value))))
            (message "env[~a] unchanged" var)
            (if (setf (getenv var) changed-value)
                (message "env[~a]: ~a" var changed-value))))))
