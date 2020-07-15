
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dev-package-developement-names
  (list "glibc"
        "glibc-locales"
        "binutils"
        "pkg-config"
        "coreutils"
        "diffutils"
        "findutils"
        "tar"
        "patch"
        "sed"
        "grep"
        "gawk"
        "moreutils"
        "flex"
        "bison"
        "make"
        "autoconf"
        "automake"
        "libtool"
        "patchelf"
        "gcc-toolchain"
        "gdb"
        "gprolog"
        "swi-prolog"
        "emacs-ediprolog"))
        ;; "linux-libre-headers@4.19.56"
        ;; "fribidi"
        ;; "bicon"

(define %lotus-dev-package-experimental-names (list "screen-message" ;; "rofi-master"
                                                    ;; "zssh"
                                                    "gnutls"
                                                    "p11-kit"
                                                    "emacs-hyperbole"))

(define %lotus-dev-package-names (append %lotus-dev-package-developement-names
                                         %lotus-dev-package-experimental-names))

(define %lotus-dev-packages
  (append
   ;; (list '(gcc "lib"))
   (map specification->package
        %lotus-dev-package-names)))

(packages->manifest %lotus-dev-packages)
