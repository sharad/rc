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



;; #+pb (fboundp 'stumpwm::run-cli-command)

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
            "sdfdsf"
            (only)
            (dotimes (c (1- (length (group-windows (current-group))))
                      (balance-frames))
              (hsplit)))

(stumpwm:defcommand vsbalance-frames () ()
            "sdfdsf"
            (only)
            (dotimes (c (1- (length (group-windows (current-group))))
                      (balance-frames))
              (vsplit)))

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
                    (process-pid video-pid)))
           (file (if (and filename (probe-file filename))
                     filename))
           filesize)
      (if pid
          (message "Desktop grabbing process working with pid: ~a.~@[~&Outputting in ~a~]" pid file)
          (message "No desktop grabbing is going on in my knowledge.~@[~&But previous recording could be found in ~a~]" file))))

  (stumpwm:defcommand grab-desktop () ();(&optional filearg) ((:rest "Filename: "))
      (if (and video-pid (sb-ext:process-alive-p video-pid))
          (grab-desktop-stop)
          (grab-desktop-start (read-one-line (current-screen) "Filename: " :initial-input "/tmp/out.flv"))))

  (stumpwm:defcommand grab-desktop-start (&optional filearg) ((:rest "Filename: "))
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

  (stumpwm:defcommand grab-desktop-stop () ()
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

  (stumpwm:defcommand grab-desktop-play () ()
    "Play video in last file"           ;I think it may be better to
                                        ;disable video playing while
                                        ;video grabbing is on.
    (if (and filename (probe-file filename))
        (run-wcli-command (format nil "vlc --play-and-exit ~a" filename))
        (progn
          (message "No output file present to play.")
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

(stumpwm:defcommand display-groups () ()
  (message "~a" (screen-groups (current-screen))))

(stumpwm:defcommand display-urgent-windows () ()
  (message "~a" (screen-urgent-windows (current-screen))))

(stumpwm:defcommand display-screen-info () ()
  (notify (current-screen)))

(stumpwm:defcommand display-screens-info () ()
  (notify (sort-screens)))

(stumpwm:defcommand display-head-info () ()
  (notify (current-head)))

(stumpwm:defcommand display-all-windows () ()
            (message "~a" (all-windows)))

(stumpwm:defcommand display-current-frames () ()
  (notify (tile-group-frame-tree (screen-current-group (current-screen)))))

(stumpwm:defcommand display-current-frame-tree () ()
  (notify (tile-group-frame-tree (screen-current-group (current-screen)))))

(stumpwm:defcommand display-frame-preferences () ()
  (notify *window-placement-rules*))

;; Misc commands --------------------------------------------------------------

;; (defun change-dir (path)
;;   (setf *default-pathname-defaults* path)
;;   #+(and clisp linux) (linux:|chdir| (truename path))
;;   #+(and sbcl sb-posix) (sb-posix:|chdir| (truename path)))
;;   ;; #+(and clisp linux) (linux:|chdir| (namestring (truename path)))
;;   ;; #+(and sbcl sb-posix) (sb-posix:|chdir| (namestring (truename path))))

;; (stumpwm:defcommand cd (path) ((:rest "Dir: "))
;;             (change-dir path))
  ;; #+(and clisp linux) (linux:|chdir| (namestring (truename path)))
  ;; #+(and sbcl sb-posix) (sb-posix:|chdir| (namestring (truename path))))

(stumpwm:defcommand cd (path) ((:rest "Dir: "))
  (change-dir path))

(stumpwm:defcommand pwd () ()
  (get-current-directory))

(defun emacs-server-running-p ()
  (or (probe-file (concat *home-dir*
                          ".emacs_server"))
      (probe-file (concat "/tmp/" (getenv "UID") "/server"))))


;; (stumpwm:defcommand emacsclient () ()
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
  (stumpwm:defcommand editor () ()
              ;;(if (wait-for-nwprogram "emacsclient")
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")) " -e '(setq spec-id \"main\")")
               ;; '(:class "Emacs")
               ))

  (stumpwm:defcommand xeditor () ()
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general"))
                       " -e "
                       (prin1-to-string
                        (concat "(serve-window-manager-request "
                                (prin1-to-string (format nil "~a" (substitute #\_ #\Space (stumpwm::group-name (stumpwm::current-group)))))
                                ")")))
               ;; '(:class "Emacs")
               ))

  (stumpwm:defcommand emacsclient () ()
              (run-wcli-command
               (concat "emacsclient -d " (getenv "DISPLAY") " -nc " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general"))
                       " -e "
                       (prin1-to-string
                        (concat "(serve-window-manager-request "
                                (prin1-to-string (format nil "~a" (substitute #\_ #\Space (stumpwm::group-name (stumpwm::current-group)))))
                                ")")))
               ;; '(:class "Emacs")
               ))

  (stumpwm:defcommand mail-reader () ()
              (run-wcli-command
               (concat "emacsclient -n -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e (make-mail-chat-frame)"))))

  (stumpwm:defcommand new-mail () ()
              (if (wait-for-program "emacsclient")
                  (run-wcli-command
                   (concat "emacsclient -n -c -d " (getenv "DISPLAY")  " -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus-group-mail)'"))))))

(stumpwm:defcommand gnus () ()
  (if (wait-for-nwprogram "emacsclient")
      (run-wcli-command
       (concat "emacsclient -d " (getenv "DISPLAY")  "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus)'"))
       '(:class "EmacsGNU"))))


;; (stumpwm:defcommand mail-reader () ()
;;   (if (wait-for-program "emacsclient")
;;   (run-wcli-command
;;    (concat "emacsclient -n -c -d " (getenv "DISPLAY")  " -f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general") " -e '(gnus)'")))))


(stumpwm:defcommand gnusclient () ()
  (if (wait-for-program "emacsclient")
  (run-or-pull
   (concat "emacsclient -d " (getenv "DISPLAY") " -c " "-f " (concat (getenv "HOME") "/.emacs.d/server/" (or (getenv "EMACS_SERVER_NAME") "general")) " -e '(gnus)'")
   '(:class "EmacsGNUS"))))

;; run-wcli-command

(stumpwm:defcommand gnusclient () ()
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

(stumpwm:defcommand gnome-quit () ()
  (run-shell-command "gconftool-2 --type boolean --set /apps/nautilus/preferences/show_desktop true")
  (run-wcli-command
   (concat "gnome-session-save --gui --logout-dialog")))

;; no use.
;; (stumpwm:defcommand emacsclient-cli () ()
;;   (run-wcli-command
;;    (concat "emacsclient -d " (getenv "DISPLAY") " -c ")))


;; (stumpwm:defcommand manuscrit () ()
;;    (run-shell-command "gv /home/m0rg/these/manuscrit/these.ps"))

(stumpwm:defcommand mutt () ()
   (run-or-raise
    "xterm -title mutt -e mutt -y"
    '(:title "mutt")))

(stumpwm:defcommand ebib () ()
  (run-or-raise
   "xterm -title ebib -e emacs -nw -f ebib"
    '(:title "ebib")))

;; (stumpwm:defcommand ebib () ()
;;   (run-or-raise-cli
;;    (if (emacs-server-running-p)
;;        "emacsclient -d ${DISPLAY} -e \"(open-ebib-in-new-frame)\""
;;        "emacs -d ${DISPLAY} -f \"(let ((ebib-layout 'full))
;;                                      (ebib))\"")
;;     '(:title "ebib")))

(stumpwm:defcommand firefox () ()
   (run-wcli-command
    (concat "firefox -P " (getenv "XBPROFILE"))))

(stumpwm:defcommand firefox-tor () ()
   (run-wcli-command
    (concat "firefox -P tor")))

(stumpwm:defcommand xbrowser () ()
            (run-wcli-command
             (concat
              (or (getenv "XBROWSER")
                  (concat
                   (getenv "HOME") "/bin/conkeror-redirected" " -P " (getenv "XBPROFILE"))))))

(stumpwm:defcommand xbrowser-tor () ()
            (run-wcli-command "conkeror -P tor"))

(stumpwm:defcommand seamonkey () ()
   (run-wcli-command
    (concat "seamonkey -P " (getenv "XBPROFILE"))))

(stumpwm:defcommand mozilla () ()
   (run-wcli-command
    (concat "mozilla -P " (getenv "XBPROFILE"))))

(stumpwm:defcommand conkeror () ()
   (run-wcli-command
    (concat "firefox -P " (getenv "XBPROFILE"))))

(stumpwm:defcommand virt-manager () ()
            "virt-manager"
  (run-wcli-command "virt-manager"))

(stumpwm:defcommand w3m () ()
   (run-or-raise
    "xterm -title w3m -tn xterm -e w3m -cookie -config /home/m0rg/.w3m/config -N http://www.google.fr"
    '(:title "w3m")))

;; (stumpwm:defcommand xbrowser () ()
;;    (run-or-raise
;;     (getenv "XBROWSER")
;;     '(:class "Mozilla")))

(stumpwm:defcommand slrn () ()
   (run-or-raise
    "xterm -title slrn -e slrn -f /home/m0rg/.newsrc"
    '(:title "slrn")))

(stumpwm:defcommand cmus () ()
   (run-or-raise
    "xterm -title cmus -e cmus"
    '(:title "cmus 2.2.0")))

(stumpwm:defcommand ssh-to-intranet () ()
   (run-or-raise
    "xterm -title Intranet -e ssh $INTRANETSERVER"
    '(:title "Intranet")))

(stumpwm:defcommand inkscape () ()
   (run-or-raise
    "inkscape"
    '(:class "Inkscape")))

(stumpwm:defcommand gimp () ()
   (run-or-raise
    "gimp"
    '(:class "Gimp")))

;;fectchmail
(stumpwm:defcommand fetchmail () ()
  (run-shell-command "fetchmail -d0")
  (message "Checking mails..."))
(stumpwm:defcommand fetchmail-daemon () ()
  (run-shell-command "fetchmail"))
(stumpwm:defcommand fetchmail-kill-daemon () ()
  (run-shell-command "fetchmail -q"))

;;unison synchronization
(stumpwm:defcommand unison-synchronization (host) ((:rest "Synchro host: "))
   (run-shell-command (concat *home-dir*
                              "/bin/synchro "
                              host)))

(stumpwm:defcommand paste () ()
            (let ((text (get-x-selection 1)))
              (and text (window-send-string text))))


;;screenshot-to-website
(stumpwm:defcommand screenshot-to-website (filename) ((:rest "Filename: "))
  (run-shell-command
   (format nil
           "import -window root /home/s/hell/code/html/patzy/screenshots/~a"
           filename)))

;;screenshot-to-file
(stumpwm:defcommand screenshot-to-file (filename) ((:rest "Filename: "))
  (run-shell-command
   (format nil "import -window root ~a" filename)))

(stumpwm:defcommand restore-group-dump (filename) ((:rest "Dump name: "))
 (let ((group (add-group (current-screen) filename)))
   (switch-to-group group)
   (restore-from-file (data-dir-file filename))))

(stumpwm:defcommand save-group-dump (filename) ((:rest "Dump name: "))
 (dump-group-to-file (data-dir-file filename)))

;;Termsn
;; (dolist '(term (xterm urxvt mrxvt))
;;         (stumpwm:defcommand term (&optional title) ((:rest "title: "))
;;                     (run-wcli-command (concatenate 'string (symbole-name term)
;;                                                   (if title (format nil " -T ~a" title))))))

;;(run-shell-command "xterm"))

;; (stumpwm:defcommand xterm () ()
;;             (run-wcli-command "xterm"))

;;Termsn
;;Termsn
(stumpwm:defcommand xterm (&optional title) ((:rest "title: "))
            (run-wcli-command (concatenate 'string "xterm"
                                          (if title (format nil " -T ~a" title)))))

;; ;;Termsn *emacs-planner-tasks*
;; (stumpwm:defcommand urxvt (&optional title) ((:rest "title: "))
;;             (run-wcli-command (concatenate 'string "urxvtc"
;;                                           (if title (format nil " -T ~a" title)))))
(stumpwm:defcommand urxvt () ()
  (run-wcli-command (concatenate 'string "urxvt" ;; "urxvtc"
                                 (format nil " -T ~a"
                                         (substitute #\_ #\Space (prin1-to-string (group-name (current-group))))))))

(stumpwm:defcommand urxvt () ()
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

(stumpwm:defcommand xterm () ()
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

(stumpwm:defcommand mrxvt (&optional title) ((:rest "title: "))
            (run-wcli-command (concatenate 'string "mrxvt"
                                          (if title (format nil " -title ~a" title)))))

(stumpwm:defcommand xscreen () ()
  (run-wcli-command "xterm -e screen"))
;;  (run-shell-command "xterm -e screen"))


;;Google calendar
(stumpwm:defcommand gcal-week () ()
  (message "~a"
           (run-shell-command (concat "gcalcli calw 1  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))
(stumpwm:defcommand gcal-month () ()
  (message "~a"
           (run-shell-command (concat "gcalcli calm  |"
                                      " sed 's/\\[0;3\\([0-7]\\)"
                                      "m/\\^\\1\\*/g' |"
                                      " sed 's/\\[[0-9;]*m//g'"
                                      " | "
                                      *stumpish*
                                      " -e notify")
                              nil)))


(stumpwm:defcommand gcal-search (search-string) ((:rest "Search gcal: "))
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

(stumpwm:defcommand gcal-add-event (evt) ((:rest "Event:"))
  (run-shell-command (format nil (concat "gcalcli quick '~a'~%")
                             evt))
  (message "Added event: ~a" evt))


;; cmus-remote control
(stumpwm:defcommand cmus-play () ()
  (run-shell-command "cmus-remote -p"))
(stumpwm:defcommand cmus-stop () ()
  (run-shell-command "cmus-remote -s"))
(stumpwm:defcommand cmus-next () ()
  (run-shell-command "cmus-remote -n"))
(stumpwm:defcommand cmus-prev () ()
  (run-shell-command "cmus-remote -r"))


(stumpwm:defcommand restart-conky () ()
   (run-shell-command
    "killall conky ; conky -d -c ~/.conkyrc/main/conkyrc 2>&1 >/dev/null"))

(stumpwm:defcommand conky () ()
  (run-shell-command
   "conky -d -c ~/.conkyrc/main/conkyrc"))

(stumpwm:defcommand file-manager () ()
  ;; (format "nautilus --no-default-window --no-desktop %s" dir)
  (stumpwm::run-wcli-command
   "nautilus --no-default-window --no-desktop"))

(stumpwm:defcommand file-manager-quit () ()
  (stumpwm:run-shell-command
   "nautilus -q"))

(stumpwm:defcommand toggle-touchpad () ()
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

(stumpwm:defcommand refocus-conkeror () ()
  ;; from: http://bc.tech.coop/ubuntu-config/.stumpwmrc
  "Re-focus the conkeror buffer.
   Useful when you want to escape Flash without a mouse."
  (shell-command "conkeror -batch -e 'if (w=window_watcher.activeWindow) {
                     unfocus(w.buffers.current);
                     w.minibuffer.message(\"focus regained\");
                   }'"))

(define-key *root-map* (kbd "X") "refocus-conkeror")

(stumpwm:defcommand sys-halt () ()
  (run-shell-command "poweroff"))

(stumpwm:defcommand sys-suspend () ()
  (run-shell-command "pmi action suspend"))

(stumpwm:defcommand sys-hibernate () ()
  (run-shell-command "pmi action hibernate"))

(stumpwm:defcommand sys-reboot () ()
  (run-shell-command "reb00t"))

(stumpwm:defcommand start-wm-components () ()
 (message "started start-wm-components")
 (prog1
     (run-shell-command
      (concat
       (getenv "HOME")
       "/.rsetup/wmlogin/run"))
   (message "done start-wm-components")))


(stumpwm:defcommand start-wm-test-components () ()
  (message "started start-wm-test-components")
  (prog1
      (run-shell-command
       (concat
        (getenv "HOME")
        "/tmp/test.sh"))
    (message "done start-wm-test-components")))

(stumpwm:defcommand bye () ()
#+pa
  (in.net.sharad.pa-backend-emacs-planner::emacs-eval-nooutput "(close-all-frames)")
  (sleep 1)
  (run-shell-command
   (concat
    (getenv "HOME")
    "/.rsetup/wmlogout/run"))
  (quit))

(stumpwm:defcommand bye-with-cleanup () ()
            (bye))

(stumpwm:defcommand bye-with-confirmation () ()
            (let ((*message-window-gravity* :center))
              (fclear)
              (if (y-or-n-p "^5^BLogout from stumpwm:^b ^2")
                  (bye-with-cleanup)
                  (pull-hidden-other))))

(stumpwm:defcommand display-top-map () ()
  (display-bindings-for-keymaps nil *top-map*))

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

(stumpwm:defcommand gnext-nonempty () ()
            )

(stumpwm:defcommand gprev-nonempty () ()
            )

(stumpwm:defcommand env (&optional (var t)) ((:string "env var: "))
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
