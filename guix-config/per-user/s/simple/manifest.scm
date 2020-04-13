
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
                                       "cl-slime-swank"))

(define %lotus-other-packages
  (list "vim"

        "ghostscript"
        "bidiv"
        "dos2unix"
        "cpulimit"

        "iproute2"

        "perl-yaml"
        "perl-xml-compile"              ;xml2yaml
        "perl-xml-libxslt"
        "python-lxml"

        "man-pages"

        "deb-forticlient-sslvpn"
        "cups-minimal" ;; for lp lpr command

        "unzip"

        "poppler"
        "whois"
        "pwgen"
        "synergy"
        ;; "gettext"

        "net-snmp"

        ;; https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks#mounting-storage-devices
        ;; "udisks"
        ;; "udisks-glue"

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
        ;; "emacs-diminish"
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

        "enscript"

        "jupyter"
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
        "xmodmap"
        "xinput"
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

        "beep"))

(define %lotus-lang-packages (list ;; "ocaml"
                              ;; ;; "ocaml-merlin"
                              ;; "opam"
                              ))

(define %lotus-user-selected-package-names
  (list   "m4"
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
          "csvkit"

          ;; "macchanger"
          ;; "font-lohit"
          "screen"
          "tmux"
          "byobu"
          "kitty"
          "lxqt-openssh-askpass"
          "gettext"
          ;; "ecryptfs-utils"
          "zsh"
          "zsh-autosuggestions"
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

(define %lotus-mail-packages
  (list "mailutils"
        "offlineimap"
        "notmuch"
        "notmuch-addrlookup-c"
        "notifymuch"
        "mu"))

(define %lotus-font-packages
  (list "freetype"
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
        "font-gnu-freefont-ttf"
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
  (list "xinit"
        "i3status"
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
        ;; "stumpwm"
        ;; "guile-wm"
        ;; "stumpwm-with-slynk"
        ;; "cl-stumpwm"
        ;; "openbox"
        ;; "awesome"
        ;; "i3-wm"
        ;; "windowmaker"
        ;; "twm"
        ;; "herbstluftwm"
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
        "pidgin"
        "python-dbus"
        "pidgin-otr"
        ;; "telegram-purple"
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

(define %lotus-test-packages
  (list "vlc"
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
        "libmpeg2"
        ;; "icecat"
        ))

(define %lotus-package-names-for-installation 
  (append %lotus-user-selected-package-names
          %lotus-lang-packages
          %lotus-disputed-packages
          %lotus-other-packages
          %lotus-mail-packages
          %lotus-font-packages
          %lotus-media-packages
          %lotus-gui-packages
          %lotus-text-packages
          %lotus-notification-packages
          %lotus-test-packages))

(define %lotus-user-desktop-packages
  (list (list glib     "bin")
        (list isc-bind "utils")))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-user-selected-packages
  (map specification->package
       %lotus-package-names-for-installation))

(define %lotus-user-packages (append %lotus-user-desktop-packages
                                     %lotus-user-selected-packages))

(define %lotus-packages (append %lotus-user-packages))

(packages->manifest %lotus-packages)

