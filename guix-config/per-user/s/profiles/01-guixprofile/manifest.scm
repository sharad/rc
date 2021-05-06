
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


(define %lotus-guixprofile-disputed-packages (list ;; "sbcl"
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

(define %lotus-guixprofile-device-hardware-packages (list "lm-sensors"
                                                          "alsa-lib"
                                                          "alsa-utils"
                                                          "bluez"
                                                          "bluez-alsa"
                                                          "bluez-alsa"
                                                          "blueman"
                                                          "ddcutil"
                                                          ;; https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks#mounting-storage-devices
                                                          ;; "udisks"
                                                          ;; "udisks-glue"
                                                          "udevil"))

(define %lotus-guixprofile-other-packages (list "vim"
                                                "gnu-pw-mgr"
                                                "mpd-mpc"
                                                "cava"
                                                "openfortivpn"
                                                "ghostscript"
                                                "bidiv"
                                                "dos2unix"
                                                "cpulimit"
                                                "units"

                                                "nomad"
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

                                                ;; "visidata"

                                                "baobab"
                                                "ncdu"
                                                "catdoc"
                                                ;; xdiskusage

                                                "bc"
                                                "gnome-calculator"
                                                "rlwrap"


                                                "guile"


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
                                                "python-paramiko"
                                                "python-scp"
                                                ;; "rofi-master"
                                                "python-attnmgr"

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
                                                ;; "xinput"
                                                ;; "ibus"
                                                ;; "m17n-lib"
                                                ;; "m17n-db"

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

                                                "sbcl-cl-ledger"

                                                "lsh"

                                                "esmtp"

                                                "enscript"

                                                ;; "agda"


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

                                                "signing-party"
                                                "pius"
                                                "gpa"
                                                "jetring"


                                                "beep"))

(define %lotus-guixprofile-lang-packages (list)) ;; "ocaml"
;; ;; "ocaml-merlin"
;; "opam"

(define %lotus-guixprofile-audio-packages (list "sox"
                                                "alsa-utils"))

(define %lotus-guixprofile-user-selected-package-names (list "m4"
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

(define %lotus-guixprofile-network-packages (list "netcat"
                                                  "nmap"
                                                  "net-snmp"
                                                  "net-tools"
                                                  "nethogs"))

(define %lotus-guixprofile-mail-packages (list "mailutils"
                                               "offlineimap"
                                               ;; "notmuch"
                                               ;; "mu"
                                               "notmuch-addrlookup-c"
                                               "notifymuch"))


(define %lotus-guixprofile-font-packages (list "freetype"
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

(define %lotus-guixprofile-media-packages (list "libva"
                                                "libva-utils"
                                                "gstreamer"
                                                "gst-libav"
                                                "gst123"
                                                "libvdpau"
                                                "mpg123"
                                                "mpg321"))

(define %lotus-guixprofile-gui-packages (list))

(define %lotus-guixprofile-text-packages (list "aspell"
                                               "aspell-dict-en"
                                               "aspell-dict-hi"
                                               "fortune-mod"
                                               "xmlstarlet"
                                               "libxslt"
                                               "tidy"))

(define %lotus-guixprofile-notification-packages (list "guile-xosd"
                                                       ;; "osdsh"
                                                       "xosd"
                                                       "libnotify"
                                                       "notification-daemon"
                                                       "dunst"))

(define %lotus-guixprofile-test-packages (list "vlc"
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

(define %lotus-guixprofile-file-packages (list "inotify-tools"))

(define %lotus-guixprofile-misc-packages (list "bluez"
                                               "blueman"
                                               "simple-scan"
                                               "xsane"
                                               "sane-backends"
                                               "sane-backends-minimal"))


(define %lotus-guixprofile-package-names-for-installation (append %lotus-guixprofile-user-selected-package-names
                                                                  %lotus-guixprofile-lang-packages
                                                                  %lotus-guixprofile-audio-packages
                                                                  %lotus-guixprofile-disputed-packages
                                                                  %lotus-guixprofile-device-hardware-packages
                                                                  %lotus-guixprofile-other-packages
                                                                  %lotus-guixprofile-network-packages
                                                                  %lotus-guixprofile-mail-packages
                                                                  %lotus-guixprofile-font-packages
                                                                  %lotus-guixprofile-media-packages
                                                                  %lotus-guixprofile-gui-packages
                                                                  %lotus-guixprofile-text-packages
                                                                  %lotus-guixprofile-notification-packages
                                                                  %lotus-guixprofile-test-packages
                                                                  %lotus-guixprofile-file-packages
                                                                  %lotus-guixprofile-misc-packages))

(define %lotus-guixprofile-user-desktop-packages (list (list glib     "bin")
                                                       (list isc-bind "utils")))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-guixprofile-user-selected-packages (map specification->package
                                                       %lotus-guixprofile-package-names-for-installation))

(define %lotus-guixprofile-user-packages (append %lotus-guixprofile-user-desktop-packages
                                                 %lotus-guixprofile-user-selected-packages))

(define %lotus-guixprofile-packages (append %lotus-guixprofile-user-packages))

(packages->manifest %lotus-guixprofile-packages)
