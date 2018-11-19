
(when nil

  ;; https://gist.github.com/martialboniou/1665090

  ;; MY STUMPWM

  (in-package :stumpwm)
  (setq *mouse-focus-policy* :click)
  (setf *startup-message*
        "everything seems to be just fine")

  (set-contrib-dir "/home/sleeze/.config/stumpwm/contrib")
  (setf *suppress-frame-indicator* t)
  ;; Directories
  (setf *home-dir* (make-pathname :directory (format nil "~A/stumpwm" (getenv "XDG_CONFIG_HOME")))
        *undo-data-dir* (make-pathname :directory "/home/sleeze/.config/stumpwm/undo")
        *scratchpad-group-name* ".scratchpad"
        *data-dir* (make-pathname :directory (format nil "~A/stumpwm/conf" (getenv "XDG_CONFIG_HOME"))))

  ;; Can I haz?
  (load-module "window-tags")
  (load-module "frame-tags")

  ;; Startup shit ------------------------------------------>

  ;; restore data from previous exit (state StumpWM was last using),
  (clear-window-placement-rules)
  (setf (group-name (first (screen-groups (current-screen)))) "Main")
  (gnewbg "Web")
  (if (probe-file (data-dir-file "desktop.lisp"))
      (restore-from-file (data-dir-file "desktop.lisp")))
  (restore-window-placement-rules (data-dir-file "tile-rules.lisp"))

  (run-shell-command "nitrogen --set-centered /home/sleeze/photo/pattern/cross.png")

  ;(setf (group-name (first (screen-groups (current-screen)))) "Main")
  ;(run-commands "gnewbg Web")

  ;; Appearance stuff

  (set-frame-outline-width 1)
  (set-font "-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-iso8859-16")
  ;(set-font "-jmk-neep-medium-r-semicondensed--11-100-75-75-c-50-iso8859-15")
  ;(set-font "-jmk-Sleeze-Medium-R-SemiCondensed--11-100-75-75-C-50-ISO8859-1")

  (set-fg-color     (make-color-hex "#D7D0C7"))
  (set-bg-color     (make-color-hex "#101010"))
  (set-border-color (make-color-hex "#E84F4F"))

  (setf *mode-line-foreground-color* (make-color-hex "#E84F4F")
        *mode-line-background-color* (make-color-hex "#101010")
        *mode-line-border-color*     (make-color-hex "#953331"))

  ;(set-focus-color   (make-color-hex "#EE3B3B"))
  (set-focus-color   (make-color-hex "#E84F4F"))
  (set-unfocus-color (make-color-hex "#111111"))
  (set-win-bg-color  (make-color-hex "#151515"))

  (setf *message-window-padding* 5)
  (setf *window-border-style* :thin)
  (setq *input-window-gravity* :bottom-left)
  (setf *maxsize-border-width*   2
        *transient-border-width* 2
        *float-window-title-height* 2
        *float-window-border* 2
        *normal-border-width*    2)

  ;; Variables

  (define-frame-preference "Web"
                           (0 t t :class "Firefox"))

  (define-frame-preference "Deluge"
                           (0 t t :class "Deluge"))

  (define-frame-preference "Media"
                           (0 t t :class "MPlayer"))

  (defvar *terminal* "urxvt"
      "Command to start a terminal."

   (defvar *small-font-terminal* "urxvt -fn -jmk-neep-medium-r-normal--10-80-75-75-c-50-iso8859-15"
      "Command to start a terminal with a small font."))

  ;; Functions

  (defun shift-windows-forward (frames win))
  "Exchange windows through cycling frames."
    (when frames
            (let ((frame (car frames)
                    (shift-windows-forward (cdr frames)
                                           (frame-window frame))
                    (when win
                             (pull-window win frame))))))

  ;; Macros

  ;; create given groups while keeping focus on current.
  (defmacro make-groups-bg (&rest names)
    (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
      `(run-commands ,@ns)))

  ;; Commands

  ;; Loadrc -- remember current state
  (defcommand loadrc-forget () () "Reload the @file{~/.stumpwmrc} file without remember current state."
    (handler-case
     (progn
       (with-restarts-menu (load-rc-file nil)))
     (error (c)
            (message "^B^1*Error loading rc file:^n ~A" c))
     (:no-error (&rest args)
                (declare (ignore args))
                (message "rc file loaded successfully."))))

  (defcommand loadrc () () "Reload the @file{~/.stumpwmrc} file while remembering current state."
    (remember-all) (loadrc-forget))

  (defcommand gmove-next () ())
  "Move focused window to next group without switching to it. Unlike behavior in gnext-with-window."
    (move-window-to-next-group (current-group) (sort-groups (current-screen)))
  (defcommand gmove-prev () ())
  "Move focused window to previous group without switching to it. Unlike behavior in gprev-with-window."
    (move-window-to-next-group (current-group) (reverse (sort-groups (current-screen))))

  ;; toggle hor/vert split (only with 2 windows)
  (defcommand toggle-split () ()
    (let* ((group (current-group))
           (cur-frame (tile-group-current-frame group))
           (frames (group-frames group)))
      (if (eq (length frames) 2)
          (progn (if (or (neighbour :left cur-frame frames)
                         (neighbour :right cur-frame frames))
                     (progn
                       (only)
                       (vsplit))
                   (progn
                     (only)
                     (hsplit))))
        (message "Works only with 2 frames"))))

  ;;rotate windows....
  (defcommand rotate-windows () ()
    (let* ((frames (group-frames (current-group))
              (win (frame-window (car (last frames))))
            (shift-windows-forward frames win)))))

  ;; layouts
  (defcommand work () () (restore-from-file "~/.config/stumpwm/layouts/work"))
  (defcommand restore-main () () (restore-from-file "~/.config/stumpwm/layouts/main-layout"))
  (defcommand restore-main2 () () (restore-from-file "~/.config/stumpwm/layouts/main2-layout"))
  (defcommand restore-mylayout () () (restore-from-file "~/.config/stumpwm/layouts/my-layout"))

  (defcommand firefox () ()
      "Run firefox"
      (run-or-raise "firefox" '(:class "Firefox")))


  (defcommand urxvt () ()
      "Run urxvt"
      (run-or-raise "urxvt" '(:class "Urxvt")))

  (defcommand gvim () ()
      "Run gvim"
      (run-or-raise "gvim" '(:class "Gvim")))

  (defcommand thunar () ()
      "Run thunar"
      (run-or-raise "thunar" '(:class "Thunar")))

  ;; Keys

  (set-prefix-key (kbd "s-x"))

  ;; Move through frames with s-[hjkl]
  (loop for (vi-key name) in '(("k" "up")
                               ("j" "down")
                               ("h" "left")
                               ("l" "right"))
      do (let ((key-combo (format nil "s-~A" vi-key))
               (shifted-key-combo (format nil "s-~A" (string-upcase vi-key))))
          (define-key *top-map* (kbd key-combo)
                                (format nil "move-focus ~A" name))
          (define-key *top-map* (kbd shifted-key-combo)
                                (format nil "move-window ~A" name))))

  ;; Groups
  (dotimes (i 9)
    (define-key *top-map* (kbd (format nil "s-~A" (1+ i)))
      (format nil "gselect ~A" (1+ i)))
    (define-key *top-map* (kbd (format nil "s-M-~A" (1+ i)))
      (format nil "gmove ~A" (1+ i))))


  ;; Paste X selection
  (defcommand paste-x-selection () (:rest)
    "Universal rat-less X paste."
    (let ((cmd (concatenate 'string "insert " (get-x-selection))))
       (eval-command cmd)))
  (define-key *top-map* (kbd "Insert")     "paste-x-selection")

  ;; Top-map keys
  (define-key *top-map* (kbd "s-RET")      (format nil "exec ~A" *terminal*))
  (define-key *top-map* (kbd "s-M-RET")    (format nil "exec ~A" *small-font-terminal*))
  (define-key *top-map* (kbd "s-TAB") "pull-hidden-next")
  (define-key *top-map* (kbd "s-r")   "loadrc")
  (define-key *top-map* (kbd "s-q")   "quit")

  ;; Screenshots and Wallpaper
  ;(define-key *top-map* (kbd "Print") "exec ~/bin/scrupload.sh")
  (define-key *top-map* (kbd "Print") "exec ~/bin/scrupload.sh")
  (define-key *top-map* (kbd "s-Print") "exec ~/bin/scrupload.sh -s")
  (define-key *top-map* (kbd "XF86HomePage") "exec ruby ~/bin/scripts/gempaper set")

  ;; Multimedia keys
  ;; Multimedia Keys
  (define-key *top-map* (kbd "XF86AudioPlay")        "exec mpc toggle")
  (define-key *top-map* (kbd "XF86AudioNext")        "exec mpc next")
  (define-key *top-map* (kbd "XF86AudioPrev")        "exec mpc prev")
  (define-key *top-map* (kbd "XF86Tools")       (format nil "exec ~A/bin/mpcm.sh"         (getenv "HOME")))
  (define-key *top-map* (kbd "s-XF86Tools")       (format nil "exec ~A/bin/mpcm2.sh"         (getenv "HOME")))

  ;; stuff to groups
  (defvar *group-map* nil
    "Keymap for doing stuffs to groups")
  (setf *group-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "n")   "gnew")
          (define-key m (kbd "f")   "gnew-float")
          (define-key m (kbd "N")   "gnewbg")
          (define-key m (kbd "r")   "grename")
          (define-key m (kbd "F")   "gnewbg-float")
          (define-key m (kbd "k")   "gkill")
          (define-key m (kbd "m")   "gmove")
          m))
  (define-key *top-map* (kbd "s-g") '*group-map*)

  ;; stuff to frames
  (defvar *window-map* nil
    "Keymap for doing stuffs to windows")
  (setf *window-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "c")   "delete-window")
          (define-key m (kbd "d")   "vsplit")
          (define-key m (kbd "f")   "hsplit")
          (define-key m (kbd "z")   "remove-split")
          (define-key m (kbd "r")   "remove")
          (define-key m (kbd "s")   "iresize")
          (define-key m (kbd "y")   "toggle-split")
          (define-key m (kbd "o")   "gprev-with-window")
          (define-key m (kbd "p")   "gnext-with-window")
          m))
  (define-key *top-map* (kbd "s-s") '*window-map*)

  ;; Application Launchers
  (defvar *launch-map* nil
    "Keymap for launching stuffs")
  (setf *launch-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "t")   "thunar")
          (define-key m (kbd "v")   "gvim")
          (define-key m (kbd "f")   "firefox")
          (define-key m (kbd "g")   "gimp")
          (define-key m (kbd "d")   "deluge")
          (define-key m (kbd "i")   "mirage")
          (define-key m (kbd "z")   "zathura")
          (define-key m (kbd "m")   (format nil "exec ~A -e pimpd2 -sh"     *small-font-terminal*))
          (define-key m (kbd "x")   (format nil "exec ~A -e stumpish"     *small-font-terminal*))
          (define-key m (kbd "n")   (format nil "exec ~A -e ncmpcpp"     *terminal*))
          (define-key m (kbd "SPC") "exec ~/bin/dmenu_run")
          m))
  (define-key *top-map* (kbd "s-SPC") '*launch-map*)


  ;; Web search

  (defmacro make-web-jump (name url-prefix)
    `(defcommand ,name (search)
       ((:rest ,(concatenate 'string (symbol-name name) ": ")))
       (run-shell-command (format nil "firefox '~A'"
                                  (concat ,url-prefix (substitute #\+ #\Space search))))))

  (make-web-jump tenstar          "http://10starmovies.com/video_search.php?q=")
  (make-web-jump series           "http://watchseries.eu/search/")
  (make-web-jump google           "http://www.google.com/search?q=")
  (make-web-jump wikipedia        "http://en.wikipedia.org/wiki/Special:Search?fulltext=Search&search=")
  (make-web-jump youtube          "http://youtube.com/results?search_query=")
  (make-web-jump awiki            "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=")
  (make-web-jump pkgs             "http://www.archlinux.org/packages/?q=")
  (make-web-jump aur              "http://aur.archlinux.org/packages.php?K=")
  (make-web-jump kickass          "http://www.kat.ph/search/")
  (make-web-jump dictionary       "http://dictionary.reference.com/browse/")


  (defvar *query-map* nil
    "Keymap for searching the webs")
  (setf *query-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "i") "tenstar")
          (define-key m (kbd "s") "series")
          (define-key m (kbd "g") "google")
          (define-key m (kbd "w") "wikipedia")
          (define-key m (kbd "y") "youtube")
          (define-key m (kbd "a") "awiki")
          (define-key m (kbd "p") "pkgs")
          (define-key m (kbd "u") "aur")
          (define-key m (kbd "k") "kickass")
          (define-key m (kbd "d") "dictionary")
          m))
  (define-key *top-map* (kbd "s-w") '*query-map*)

  ;; dirs
  ;;
  ;; Open urxvt in a given directory.

  (defmacro dirs-search (name url-prefix)
    `(defcommand ,name (search)
       ((:rest ,(concatenate 'string (symbol-name name) ": ")))
       (run-shell-command (format nil "urxvt -cd '~A'"
                                  (concat ,url-prefix (substitute #\/ #\Space search))))))

  (dirs-search location   "/")

  (define-key *top-map* (kbd "s-u")   "location")

  ;; Volume keys
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")
  (define-key *top-map* (kbd "XF86AudioMute")        "exec amixer set Master toggle")

  ;; Window with layout
  (defmacro program-with-layout (name &key (command (string-downcase (string name))))
                                (props `'(:class ,(string-capitalize command)))
      `(defcommand ,name () ()
          (gnew ,command)
          (restore-from-file ,(concat "/home/sleeze/.config/stumpwm/layouts/"
                               command "-layout"))
          (restore-window-placement-rules ,(concat "/home/sleeze/.config/stumpwm/layouts/"
                                            command "-rules"))
          (run-or-raise ,command ,props)
          (place-existing-windows))) ; needed if the command has already been run

  ;; Gimp
  (program-with-layout gimp)
  (program-with-layout deluge)
  (program-with-layout zathura)

  ;; Group shit

  (defun remember-all () ())
  "Similiar to remember-group except all information is dumped, useful
  for next startup or recalling all undo actions."
    (dump-to-datadir "rules") (dump-to-datadir "desktop")


  (defcommand dump-to-datadir (expr) (:rest)
    "Dump group (from current-screen), screen (current-screen), desktop or rules to file in data-dir.
  Just specify what you want to dump and this will dynamically create and name file accordingly."
    (cond ((string-equal expr 'group)
           (let* ((o (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
                                                  (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
                                    :type "lisp" :defaults *data-dir*)))
             (dump-group-to-file o) (message "~A dumped" expr)))
          ((string-equal expr 'screen)
           (let* ((o (make-pathname :name (format nil "screen_~{~A~}" (list (char (getenv "DISPLAY") 1)))
                                    :type "lisp" :defaults *data-dir*)))
             (dump-screen-to-file o) (message "~A dumped" expr)))
          ((string-equal expr 'rules)
           (let* ((o (make-pathname :name "tile-rules" :type "lisp" :defaults *data-dir*)))
             (dump-window-placement-rules o) (message "~A dumped" expr)))
          ((string-equal expr 'desktop)
           (let* ((o (make-pathname :name "desktop" :type "lisp" :defaults *data-dir*)))
             (dump-desktop-to-file o) (message "~A dumped" expr)))
          (t (message "dont know how to dump ~a" expr))))

  ;; restore [current]-group (for current-screen), [current]-screen, desktop or window-placement-rules
  ;; from a previously created file (more correctly from DUMP-TO-DATADIR) in user defined *data-dir*.
  (defcommand restore-from-datadir (expr) (:rest))
  "Restore file from data-dir, previously created by 'dump-to-datadir', according to what you specify.
  You may restore group (for current-screen), screen (for current-screen), desktop or rules. This will
  restore file dynamically by pattern patching, according to what you're restoring, to file name by
  looking at what you're currently using.

  E.g. if you're in group 2 on screen 0 and you enter 'restore-from-datadir group' it will look for a
  file named 'screen_0_group_2.lisp' (created by accompanying 'dump-to-datadir') in your data-dir and
  restore it. If no matching file is found it will skip loading of any files and print an error message.

  Note: if restoring a group file was successful then an undo state is created so you can technically
  undo the loading of that file. There are no undo states when loading 'screen', 'desktop' or 'rules'."
    (cond ((string-equal expr 'group
            (let* ((i (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"))
                      (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group)))
                      :type "lisp" :defaults *data-dir*))
              (if (probe-file i)
                  (progn (restore-from-file i) (remember-group) (message "~A restored" expr))
                (message "unable to find valid ~A file in data dir" expr)))))
          ((string-equal expr 'screen
            (let* ((i (make-pathname :name (format nil "screen_~{~A~}" (list (char (getenv "DISPLAY") 1))))
                      :type "lisp" :defaults *data-dir*))
              (if (probe-file i)
                  (progn (restore-from-file i) (message "~A restored" expr))
                (message "unable to find valid ~A file in data dir" expr)))))
          ((string-equal expr 'rules
            (if (probe-file (data-dir-file "tile-rules.lisp"))
                (progn (restore-window-placement-rules (data-dir-file "tile-rules.lisp"))
                       (message "~A restored" expr))
              (message "unable to find valid ~A file in data dir" expr))))
          ((string-equal expr 'desktop
            (if (probe-file (data-dir-file "desktop.lisp"))
                (progn (restore-from-file (data-dir-file "desktop.lisp")) (message "~A restored" expr))
              (message "unable to find valid ~A file in data dir" expr))))
          (t (message "dont know how to restore ~a" expr)))

  ;; dump to file, which is silent, but with more informative prompts.
  (defcommand dump-group-to-file (file) ((:rest "group to file: "))
    "Dumps the frames of the current group of the current screen to the named file."
    (dump-to-file (dump-group (current-group)) file))
  (defcommand dump-screen-to-file (file) ((:rest "screen to file: "))
    "Dumps the frames of all groups of the current screen to the named file."
    (dump-to-file (dump-screen (current-screen)) file))
  (defcommand dump-desktop-to-file (file) ((:rest "desktop to file: "))
    "Dumps the frames of all groups of all screens to the named file."
    (dump-to-file (dump-desktop) file))

  (defun remember-group (&optional (group (current-group))) ())
  "Remember current group information before calling another command or
  function. Combined with 'undo' command this allows for toggling between
  the two undo states."
    (if (ensure-directories-exist *undo-data-dir*)
      (when group
        (dump-group-to-file
          (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"))
          (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group)))
          :type "lisp" :defaults *undo-data-dir*)))

  ;; designate master window/frame (should probably use current frame number, but less dynamic?)
  (defcommand (master-make tile-group) () () "Designate current window as Master."
    (renumber 0) (repack-window-numbers) (remember-group))
  (defcommand (master-focus tile-group) () () "Focus on designated Master window." (select-window-by-number 0))

  ;; swap current window with master (should be 0 (from master-make)) and desginate it as the new master.
  (defcommand (master-swap tile-group) (num &optional (group (current-group))) ((:window-number t))
    "If current window is not Master and Master exists, swap current
              window with Master and designate this as the new Master."
    (labels ((match (win)
                    (= (window-number win) num)))
      (let* ((win (find-if #'match (group-windows group))))
        (when (and win group) (exchange-windows (current-window) win) (master-make)))))

  (defcommand rotate-windows () ()
    (let* ((frames (group-frames (current-group)))
           (win (frame-window (car (last frames)))))
      (shift-windows-forward frames win))

  ;; Sounds
  ;; I don't know yet.

  ;; (defun place-window-sound (w g f)
  ;;   (declare (ignore w f))
  ;;   (unless (eq (current-group) g)
  ;;     (run-shell-command "exec mplayer /home/sleeze/.config/stumpwm/sounds/beep.ogg")))
  ;;
  ;; (defun new-window-sound (w)
  ;;   (run-shell-command "exec mplayer /home/sleeze/.config/stumpwm/sounds/beep.ogg"))

   (defun destroy-window-sound (w)
     ;(run-shell-command "exec mplayer /home/sleeze/.config/stumpwm/sounds/beep.ogg"))
     (run-shell-command "exec mplayer /home/sleeze/.config/stumpwm/sounds/beep-7.mp3"))

   (defmacro replace-hook (hook fn)
     `(remove-hook, hook, fn)
     `(add-hook, hook, fn))

  ;; (replace-hook *new-window-hook* 'new-window-sound)
   (replace-hook *destroy-window-hook* 'destroy-window-sound)))
  ;; (add-hook *place-window-hook* 'place-window-sound)
