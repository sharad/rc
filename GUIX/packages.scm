
(define %lotus-other-packages
  (list "emacs-geiser"
        "emacs-sesman"
        "guile-studio"
        "emacs-guix"
        "emacs-math-symbol-lists"
        "emacs-pretty-mode"
        "emacs-el-mock"
        "emacs-flyspell-correct"
        "jupyter"

        "rcs"
        "darcs"

        "dovecot"
        ;; "postfix"
        ;; "myconkeror"
        ;; "next"

        "font-adobe-source-code-pro"
        "font-terminus"
        "font-dejavu"
        "font-hack"
        "font-awesome"
        "fribidi"
        ;; "bicon"

        ;; "p4"
        "pavucontrol"
        "pulsemixer"

        "patchelf"
        "gdb"
        "tree"

        "time"

        "xmodmap"
        ;; at
        "curl"
        "perl"
        "python"
        "ruby"
        "autocutsel"
        "xcompmgr"
        "wget"
        "xmlstarlet"
        "xwininfo"
        "xmlstarlet"
        "imagemagick"

	"twm"
	"herbstluftwm"
	"wmctrl"
	;; https://sourceforge.net/p/cdesktopenv/wiki/LinuxBuild/
	;; "cdesktopenv"

        ;; "bsdmainutils"
	"rdup"
        "git-annex"
        "git-remote-gcrypt"

	"ledger"
	"emacs-ledger-mode"
	"sbcl-cl-ledger"

        "lsh"

	"enscript"

        "gnupg"
        "gpgme"
        "qgpgme"
        "pinentry"
        "pinentry-tty"
        "pinentry-gtk2"
	"emacs-pinentry"
	"signing-party"

        ))

(define %lotus-system-selected-package-names
  (list
   "m4"
   "binutils"
   ;; "coreutils"
   ;; "diffutils"
   ;; "findutils"
   ;; "gnu-make"
   ;; "patch"
   "libxdg-basedir"
   "xdg-user-dirs"
   "xdg-utils"
   "shroud"
   "gdm"
   "gpm"
   "git"
   "git-remote-gcrypt"
   "guile-colorized"
   "file"
   "font-lohit"
   "screen"
   "tmux"
   "kitty"
   "lxqt-openssh-askpass"
   "gettext"
   "ecryptfs-utils"
   "zsh"
   "zsh-autosuggestions"
   "hstr"
   "shflags"
   "vim"
   "the-silver-searcher"
   "emacs-ag"
   "emacs-helm-ag"
   "emacs"
   "gparted"
   "parted"
   "ncurses-with-gpm"
   "ncurses"

   "polkit"
   "polkit-gnome"
   "redshift"

   "stapler"
   "gcc-toolchain"
   "strace"
   "guile-readline"

   "sbcl"
   "cl-fad"
   "cl-slime-swank"

   "glibc-utf8-locales"))

(define %lotus-mail-packages
  (list "mailutils"
        "offlineimap"
        "notmuch"
        "mu"))

(define %lotus-font-packages
  (list "font-lohit"
        ;; "font-indic"
	))

(define %lotus-media-packages
  (list "libva"
        "libva-utils"
        "gstreamer"
        "gst-libav"
        "gst123"
        "libvdpau"
        "mpg123"
        "mpg321"))

(define %lotus-gui-packages
  (list "i3status"
        "dmenu"
        "st"
        "xrdb"
        "xterm"
        "xdotool"
        "xrandr"
        "arandr"
        "autorandr"
        "xrandr-invert-colors"
        "rxvt-unicode"
        "sakura"
        "seahorse"
        "libxft"
        "scsh"
        "openbox"
        "awesome"
        "i3-wm"
        "windowmaker"
        "wmnd"
        "menumaker"
        "stumpwm"
        "guile-wm"
        ;; "stumpwm-with-slynk"
        ;; "cl-stumpwm"
        "emacs-stumpwm-mode"
        "keynav"
        "conky"
        ;; "surf"
        "xprop"
        "xwininfo"
        "xautolock"
        "slock"
        "xset"
        "xsetroot"
        "pidgin"
        "pidgin-otr"
        "telegram-purple"))

(define %lotus-text-packages
  (list "aspell"
        "aspell-dict-en"
        "aspell-dict-hi"
        "fortune-mod"
        "xmlstarlet"
        "libxslt"
        "tidy"))

(define %lotus-notification-packages
  (list "guile-xosd" 
        ;; "osdsh"
        "xosd"
        "libnotify"
        "dunst"))

(define %lotus-package-names-for-installation 
  (append %lotus-system-selected-package-names
          %lotus-other-packages
          %lotus-mail-packages
          %lotus-font-packages
          %lotus-media-packages
          %lotus-gui-packages
          %lotus-text-packages
          %lotus-notification-packages))

(define %lotus-system-desktop-packages
  (list lvm2
        ;; for HTTPS access
        nss-certs
        ;; for user mounts
        gvfs))

(define %lotus-system-selected-packages
  (map specification->package
       %lotus-package-names-for-installation))

(define %lotus-system-packages (append %lotus-system-desktop-packages
                                       %lotus-system-selected-packages))

(define %lotus-packages (append %lotus-system-packages
                                %base-packages))
