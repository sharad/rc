

(define %lotus-servers-package-names (list "bc"))

(define %lotus-servers-packages (append
                                      (map specification->package
                                           %lotus-servers-package-names)))


;; set LD_LIBRARY_PATH as some command built and used
;; (packages->manifest %lotus-servers-packages)

(packages->manifest %lotus-servers-packages)

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
