
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-x-package-names (list "pidgin"
                                     ;; "skype4pidgin@1.6"
                                     "skype4pidgin"
                                     "pidgin-otr"
                                     "telegram-purple"
                                     "telegram-purple"))

(define %lotus-x-packages
  (append (map specification->package
               %lotus-x-package-names)))

(packages->manifest %lotus-x-packages)
