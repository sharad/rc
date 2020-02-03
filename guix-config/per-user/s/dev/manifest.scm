
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dev-cdesktop-package-names
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
        "flex"
        "bison"
        "make"
        "autoconf"
        "automake"
        "libtool"))

(define %lotus-dev-cdesktop-packages
  (append
   ;; (list '(gcc "lib"))
   (map specification->package
        %lotus-dev-cdesktop-package-names)))

(packages->manifest %lotus-dev-cdesktop-packages)
