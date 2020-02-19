
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dev-cdesktop-package-names (list "firefox" "p4" "lesspipe"))
  

(define %lotus-dev-cdesktop-packages
  (append
   (list (list gcc "lib"))
   (map specification->package
        %lotus-dev-cdesktop-package-names)))

;; set LD_LIBRARY_PATH as some command built and used
;; (packages->manifest %lotus-dev-cdesktop-packages)

(packages->manifest %lotus-dev-cdesktop-packages)



;; https://sourceforge.net/p/cdesktopenv/wiki/LinuxBuild/#debian

;; git (for downloading from source repository)
;; build-essentials or build-essential
;; libxp-dev (not available on latest linuxes, skip)
;; libxt-dev
;; libxmu-dev
;; libxft-dev
;; libxinerama-dev
;; libxpm-dev
;; libmotif or libmotif3 or libmotif4 or libxm4 (Openmotif, in non-free or restricted)
;; libmotif-dev (Openmotif, in non-free or restricted)
;; libxaw7-dev (used by dtinfo)
;; libx11-dev
;; libXSs-dev or libxss-dev
;; libtirpc-dev
;; x11-xserver-utils (for xset)
;; libjpeg62-turbo-dev or libjpeg62-dev
;; libfreetype6-dev
;; libssl-dev
;; tcl-dev
;; ksh (required for database to any script, and probably dtksh building)
;; m4 (required for nsgmls building)
;; ncompress (old style unix 'compress' needed when building help files)
;; xfonts-100dpi (for nicer looking fonts)
;; xfonts-100dpi-transcode or xfonts-100dpi-transcoded
;; rpcbind (or portmap) may require running in insecure mode (-i) see section 1.5 below
;; bison
;; xbitmaps
;; x11proto-fonts-dev


;; https://sourceforge.net/p/motif/code/ci/master/tree/INSTALL.configure
