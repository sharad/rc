
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
;; (use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

;; (use-modules (lotus packages cdesktopenv))

(define %lotus-java-package-names
  (list "adb"
        "android-make-stub"))

(define %lotus-java-packages
  (append
   ;; (list '(gcc "lib"))
   (map specification->package
        %lotus-java-package-names)))

(packages->manifest %lotus-java-packages)
