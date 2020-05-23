
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dynamic-hash-package-names (list "firefox"
                                                "conkeror-firefox"
                                                "p4"
                                                "lesspipe" 
                                                "emacs-develock"))

(define %lotus-dynamic-hash-packages
  (append
   (map specification->package
        %lotus-dynamic-hash-package-names)))

;; set LD_LIBRARY_PATH as some command built and used
;; (packages->manifest %lotus-dynamic-hash-packages)

(packages->manifest %lotus-dynamic-hash-packages)

