;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison m4 gawk xorg onc-rpc commencement fonts dns)


(use-modules (gnu packages linux))

(use-modules (guix utils))
(use-modules (guix packages))
(use-modules (gnu services networking))
(use-modules (gnu) (gnu system nss))

(use-service-modules networking ssh)
(use-package-modules bootloaders certs suckless wm)

(use-service-modules desktop networking ssh xorg)
(use-package-modules certs gnome)

(use-modules (gnu packages shells))

(use-modules (gnu))
(use-package-modules screen)




;; other guix
(use-modules (gnu system locale))


(define %lotus-disputed-packages (list "sbcl"
                                       "cl-fad"
                                       "sbcl-clx-xembed"
                                       "cl-slime-swank"
                                       "stumpish"
                                       "sbcl-stumpwm-wifi"
                                       "sbcl-stumpwm-ttf-fonts"
                                       "sbcl-stumpwm-swm-gaps"
                                       "sbcl-stumpwm-stumptray"
                                       "sbcl-stumpwm-pass"
                                       "sbcl-stumpwm-net"
                                       "sbcl-stumpwm-kbd-layouts"
                                       "sbcl-stumpwm-globalwindows"))
;; "notification-daemon" "trezord-udev-rules" "trezord"

(define %lotus-other-packages (list "vim"
                                    "gparted" ;; required for situations
                                    "gnu-pw-mgr"
                                    "mpd-mpc"
                                    "cava"
                                    ;; "openfortivpn"
                                    "ghostscript"
                                    "bidiv"
                                    "dos2unix"
                                    "cpulimit"
                                    "units"

                                    "iproute2"

                                    "perl-yaml"
                                    "perl-xml-compile"              ;xml2yaml
                                    "perl-xml-libxslt"
                                    "python-lxml"

                                    "man-pages"

                                    ;; "deb-forticlient-sslvpn"
                                    "cups-minimal" ;; for lp lpr command

                                    "unzip"
                                    "cpio"

                                    "poppler"
                                    "whois"
                                    "pwgen"
                                    "synergy"
                                    ;; "gettext"

                                    ;; https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks#mounting-storage-devices
                                    ;; "udisks"
                                    ;; "udisks-glue"
                                    "udevil"

                                    ;; "blueman"
                                    ;; "visidata"

                                    "baobab"
                                    "ncdu"
                                    "catdoc"
                                    ;; xdiskusage

                                    "bc"
                                    "gnome-calculator"
                                    "rlwrap"



                                    "emacs-bbdb"
                                    "emacs-ebdb"
                                    "emacs-gnus-harvest"
                                    "guile"
                                    "emacs-geiser"
                                    "emacs-sesman"
                                    "guile-studio"
                                    "emacs-guix"
                                    "emacs-math-symbol-lists"
                                    "emacs-pretty-mode"
                                    "emacs-el-mock"
                                    "emacs-flyspell-correct"
                                    "emacs-powerline"
                                    "emacs-spaceline"
                                    "emacs-emojify"
                                    "emacs-org"
                                    "emacs-org-noter" ;; https://github.com/weirdNox/org-noter
                                    "emacs-closql"
                                    "emacs-diminish"
                                    ;; "emacs-pass"
                                    "emacs-direnv"
                                    "emacs-editorconfig"
                                    "emacs-develock"
                                    "emacs-el-x"
                                    "emacs-default-encrypt"
                                    "emacs-default-text-scale"
                                    "emacs-deft"
                                    "emacs-dashboard"
                                    "emacs-typo"
                                    ;; https://develop.spacemacs.org/layers/+lang/c-c++/README.html
                                    "emacs-lsp-ui"
                                    "emacs-lsp-ivy"
                                    "emacs-helm-lsp"
                                    "emacs-company-lsp"
                                    ;; "clangd"
                                    ;; "cquery"
                                    "ccls"
                                    "emacs-ccls"
                                    "emacs-slack"
                                    "emacs-pdf-tools"
                                    ;; "emacs-org-roam" ;; need roam internet service login
                                    "emacs-standard-dirs"
                                    "emacs-helm-switch-to-repl"
                                    "emacs-org-pretty-table"
                                    "emacs-chess"
                                    "emacs-company-emoji"
                                    "emacs-dash-docs"
                                    "emacs-lsp-ivy"
                                    "emacs-smart-hungry-delete"
                                    "emacs-webpaste"
                                    "emacs-vterm"
                                    "emacs-ediprolog"
                                    "emacs-hyperbole"

                                    "xhost"
                                    "xauth"
                                    "xkill"
                                    ;; "xprintidle"
                                    ;; X gl
                                    "compton"
                                    "compton-conf"
                                    "xcompmgr"
                                    "xdpyinfo"
                                    "rofi"
                                    "python-rofi"
                                    "python-rofi-menu"
                                    "python-rofi-tmux"
                                    "python-paramiko"
                                    "python-scp"
                                    "python-attnmgr"
                                    ;; "rofi-master"

                                    "enscript"

                                    ;; "jupyter"
                                    "python-git-review"

                                    "wget"
                                    "xmlstarlet"
                                    "libxml2"
                                    "libxslt"
                                    "qtxmlpatterns"                 ;xquery

                                    "atool"
                                    "sshpass"

                                    ;; ;; qemu
                                    ;; "virt-manager"
                                    ;; "virt-viewer"
                                    ;; "libvirt"

                                    ;; "lesspipe"
                                    "python-organize-tool"

                                    "evince"
                                    "gv"

                                    "mosh"
                                    "eternalterminal"

                                    "eog"
                                    "feh"

                                    "openldap"

                                    ;; "glib"
                                    ;; "dconf"
                                    ;; "gsettings-desktop-schemas"

                                    "rcs"
                                    "darcs"

                                    ;; "bind:utils"

                                    "recutils"
                                    "emacs-recutils"

                                    ;; https://unix.stackexchange.com/questions/20784/how-can-i-resolve-a-hostname-to-an-ip-address-in-a-bash-script
                                    ;; getent hosts localhost



                                    "xlsfonts"


                                    "global"


                                    "alsa-utils"
                                    "aumix"
                                    "pavucontrol"
                                    "pulsemixer"

                                    "tree"

                                    "time"

                                    "xclip"
                                    "xsel"
                                    "xmodmap"
                                    "xinput"
                                    "ibus"
                                    "m17n-lib"
                                    "m17n-db"

                                    ;; at
                                    "curl"
                                    "perl"
                                    "python"
                                    "ruby"
                                    "autocutsel"
                                    "xcompmgr"
                                    "xfd"
                                    "xwininfo"
                                    "xmlstarlet"
                                    "imagemagick"
                                    "setxkbmap"
                                    "kbd"                           ;kbdinfo gkbled
                                    "xkeyboard-config"

                                    "fasd"

                                    "wmctrl"
                                    ;; https://sourceforge.net/p/motif/code/ci/master/tree/INSTALL.configure
                                    ;; https://sourceforge.net/p/cdesktopenv/wiki/LinuxBuild/
                                    ;; "cdesktopenv"

                                    ;; "bsdmainutils"
                                    "rdup"

                                    "ledger"
                                    "emacs-ledger-mode"
                                    "sbcl-cl-ledger"

                                    "lsh"

                                    "esmtp"

                                    "enscript"

                                    ;; "agda"
                                    "emacs-agda2-mode"

                                    "gnupg"
                                    "paperkey"
                                    "qrencode"
                                    "gpgme"
                                    "signing-party"
                                    ;; "scdaemon"
                                    ;; pcscd
                                    ;; "ccid"
                                    "gpgme"
                                    "qgpgme"
                                    "pinentry"
                                    "pinentry-tty"
                                    "pinentry-gtk2"
                                    "emacs-pinentry"
                                    "signing-party"
                                    "pius"
                                    "gpa"
                                    "jetring"

                                    "lm-sensors"

                                    "beep"))

(define %lotus-lang-packages (list)) ;; "ocaml"
;; ;; "ocaml-merlin"
;; "opam"

(define %lotus-audio-packages (list "sox"
                                    "alsa-utils"))

(define %lotus-hardware-packages (list "ddcutil"))

(define %lotus-user-selected-package-names (list "m4"
                                                 "binutils"
                                                 "libxdg-basedir"
                                                 "xdg-user-dirs"
                                                 "xdg-utils"
                                                 ;; https://faq.i3wm.org/question/2155/how-can-i-use-autostart-desktop-files-in-i3.1.html
                                                 ;; https://github.com/jceb/dex
                                                 ;; http://e-jc.de/dex/
                                                 ;; dex
                                                 "shroud"
                                                 "git"
                                                 "git-remote-gcrypt"
                                                 "guile-colorized"
                                                 "file"

                                                 "python"
                                                 "jq"
                                                 "python-xq"
                                                 "python-yq"
                                                 "python-tinydb"
                                                 "csvkit"

                                                 ;; "macchanger"
                                                 ;; "font-lohit"
                                                 "screen"
                                                 "tmux"
                                                 "byobu"
                                                 "kitty"
                                                 ;; "lxqt-openssh-askpass"
                                                 "gettext"
                                                 ;; "ecryptfs-utils"
                                                 "zsh"
                                                 "dash"
                                                 ;; "zsh-autosuggestions"
                                                 "hstr"
                                                 "shflags"
                                                 "the-silver-searcher"
                                                 "emacs-ag"
                                                 "emacs-helm-ag"
                                                 "emacs"

                                                 ;; "cdesktopenv"

                                                 ;; "gparted"
                                                 ;; "parted"
                                                 "ncurses-with-gpm"
                                                 ;; "ncurses"

                                                 ;; "polkit"
                                                 ;; "polkit-gnome"

                                                 "redshift"
                                                 "xcursor-themes"
                                                 "unclutter"


                                                 "glibc-utf8-locales" ;; guix guile showing some error even it is part of system configuration

                                                 "stapler"
                                                 "qpdf"
                                                 "strace"
                                                 "guile-readline"))

(define %lotus-network-packages (list "netcat"
                                      "nmap"
                                      "net-snmp"
                                      "net-tools"
                                      "nethogs"))

(define %lotus-mail-packages (list "mailutils"
                                   "offlineimap"
                                   "notmuch"
                                   "notmuch-addrlookup-c"
                                   "notifymuch"
                                   "mu"))

(define %lotus-font-packages (list "freetype"
                                   ;; ftview
                                   ;; https://wiki.debian.org/Fonts#Infinality_for_Debian
                                   "ttfautohint"
                                   "gs-fonts"
                                   "font-ibm-plex"
                                   "font-inconsolata"
                                   "font-ubuntu"
                                   "font-dejavu"
                                   "font-bitstream-vera"
                                   "font-abattis-cantarell"
                                   "font-lato"
                                   ;; "font-gnu-freefont-ttf"
                                   "font-gnu-freefont"
                                   "font-liberation"
                                   "font-linuxlibertine"
                                   "font-terminus"

                                   ;; "font-adobe-source-han-sans"
                                   ;; "font-cns11643"

                                   "font-cns11643-swjz"
                                   "font-wqy-zenhei"
                                   "font-wqy-microhei"
                                   "font-rachana"
                                   "font-tex-gyre"
                                   "font-anonymous-pro"
                                   "font-anonymous-pro-minus"
                                   "font-gnu-unifont"
                                   ;; "font-google-noto"
                                   "font-google-roboto"
                                   "font-un"
                                   "font-fantasque-sans"
                                   "font-hack"
                                   "font-adobe-source-code-pro"
                                   "font-adobe-source-sans-pro"
                                   "font-adobe-source-serif-pro"
                                   "font-fira-mono"
                                   "font-fira-sans"
                                   "font-fira-code"
                                   "font-awesome"
                                   "font-tamzen"
                                   "font-comic-neue"
                                   ;; "font-iosevka"
                                   ;; "font-iosevka-slab"
                                   "font-go"
                                   "font-google-material-design-icons"
                                   "font-opendyslexic"
                                   "font-dosis"
                                   "font-culmus"
                                   ;; "font-indic"
                                   "font-lohit"
                                   "font-blackfoundry-inria"
                                   "font-sil-gentium"
                                   "font-sil-andika"
                                   "font-sil-charis"
                                   "font-mononoki"
                                   "font-public-sans"
                                   "font-hermit"
                                   "font-dseg"))

(define %lotus-media-packages (list "libva"
                                    "libva-utils"
                                    "gstreamer"
                                    "gst-libav"
                                    "gst123"
                                    "libvdpau"
                                    "mpg123"
                                    "mpg321"))

(define %lotus-gui-packages (list "xinit"
                                  "i3status"
                                  ;; "xvkbd" ;; https://unix.stackexchange.com/a/11890
                                  "libwm"
                                  ;; "wmutils-core"
                                  ;; "wmutils-opt"
                                  "xautomation"
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
                                  "nautilus"
                                  "tracker"
                                  "emacs-pass"
                                  ;; "keychain"
                                  "gnome-keyring"
                                  "gcr"
                                  "seahorse"
                                  "libsecret"
                                  "libxft"
                                  "scsh"
                                  "wmnd"
                                  "menumaker"
                                  "emacs-stumpwm-mode"

                                  "keynav"
                                  "conky"
                                  ;; "surf"
                                  "xprop"
                                  "xwininfo"
                                  "xautolock"
                                  ;; "slock" -- need suid
                                  "xset"
                                  "xsetroot"
                                  "python-dbus"))

(define %lotus-text-packages (list "aspell"
                                   "aspell-dict-en"
                                   "aspell-dict-hi"
                                   "fortune-mod"
                                   "xmlstarlet"
                                   "libxslt"
                                   "tidy"))

(define %lotus-notification-packages (list "guile-xosd" 
                                           ;; "osdsh"
                                           "xosd"
                                           "libnotify"
                                           "notification-daemon"
                                           "dunst"))

(define %lotus-test-packages (list "vlc"
                                   "ffmpeg"
                                   "libvorbis"
                                   "libvpx"
                                   "pulseaudio"
                                   "alsa-lib"
                                   "libogg"
                                   "ffmpeg"
                                   "gst-plugins-base"
                                   "gst-plugins-good"
                                   "gst-plugins-bad"
                                   "gst-plugins-ugly"
                                   "gst-libav"
                                   "gst123"
                                   "gstreamer"
                                   "openh264"
                                   "libsmpeg"
                                   "libmpeg2"))
;; "icecat"

(define %lotus-file-packages (list "inotify-tools"))

(define %lotus-misc-packages (list "bluez"
                                   "blueman"))

(define %lotus-package-names-for-installation (append %lotus-user-selected-package-names
                                                      %lotus-lang-packages
                                                      %lotus-audio-packages
                                                      ;; %lotus-hardware-packages
                                                      %lotus-disputed-packages
                                                      %lotus-other-packages
                                                      %lotus-network-packages
                                                      %lotus-mail-packages
                                                      %lotus-font-packages
                                                      %lotus-media-packages
                                                      %lotus-gui-packages
                                                      %lotus-text-packages
                                                      %lotus-notification-packages
                                                      %lotus-test-packages
                                                      %lotus-file-packages
                                                      %lotus-misc-packages))

(define %lotus-user-desktop-packages (list (list glib     "bin")
                                           (list isc-bind "utils")))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-user-selected-packages (map specification->package
                                           %lotus-package-names-for-installation))

(define %lotus-user-packages (append %lotus-user-desktop-packages
                                     %lotus-user-selected-packages))

(define %lotus-packages (append %lotus-user-packages))

(packages->manifest %lotus-packages)

