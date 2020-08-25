
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-lengthy-package-names (list "retro-firefox-80.0"
                                           "git-annex"
                                           "agda"
                                           "python-pypdf2"
                                           "python-numpy"
                                           "python-pandas"
                                           "python-pycryptodome"
                                           "python-pdfminer"))
                                                ;; "opencv" ;; -- put it in input depedency

(define %lotus-lengthy-packages
  (append
   (map specification->package
        %lotus-lengthy-package-names)))

;; set LD_LIBRARY_PATH as some command built and used
;; (packages->manifest %lotus-lengthy-packages)

(packages->manifest %lotus-lengthy-packages)
