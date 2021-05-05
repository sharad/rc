
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-x-package-names (list "pidgin"
                                     "skype4pidgin"
                                     "pidgin-otr"
                                     "geeqie"
                                     "telegram-purple"
                                     "telegram-purple"))

(define %lotus-x-gui-packages (list "xinit"
                                    "i3status"
                                    ;; "xvkbd" ;; https://unix.stackexchange.com/a/11890
                                    "libwm"
                                    "wmutils-core"
                                    "wmutils-opt"
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

(define %lotus-x-package-names-for-installation (append %lotus-x-package-names
                                                        %lotus-x-gui-packages))

(define %lotus-x-packages
  (map specification->package
       %lotus-x-package-names-for-installation))

(packages->manifest %lotus-x-packages)
