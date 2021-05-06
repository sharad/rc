
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


(define %lotus-simple-disputed-packages (list))
;; "notification-daemon" "trezord-udev-rules" "trezord"

(define %lotus-simple-device-hardware-packages (list))

(define %lotus-simple-other-packages (list))

(define %lotus-simple-lang-packages (list)) ;; "ocaml"
;; ;; "ocaml-merlin"
;; "opam"

(define %lotus-simple-audio-packages (list))

(define %lotus-simple-user-selected-package-names (list))

(define %lotus-simple-network-packages (list))

(define %lotus-simple-mail-packages (list))


(define %lotus-simple-font-packages (list))

(define %lotus-simple-media-packages (list))

(define %lotus-simple-gui-packages (list))

(define %lotus-simple-text-packages (list))

(define %lotus-simple-notification-packages (list))

(define %lotus-simple-test-packages (list))
;; "icecat"

(define %lotus-simple-file-packages (list))

(define %lotus-simple-misc-packages (list))


(define %lotus-simple-package-names-for-installation (append %lotus-simple-user-selected-package-names
                                                             %lotus-simple-lang-packages
                                                             %lotus-simple-audio-packages
                                                             %lotus-simple-disputed-packages
                                                             %lotus-simple-device-hardware-packages
                                                             %lotus-simple-other-packages
                                                             %lotus-simple-network-packages
                                                             %lotus-simple-mail-packages
                                                             %lotus-simple-font-packages
                                                             %lotus-simple-media-packages
                                                             %lotus-simple-gui-packages
                                                             %lotus-simple-text-packages
                                                             %lotus-simple-notification-packages
                                                             %lotus-simple-test-packages
                                                             %lotus-simple-file-packages
                                                             %lotus-simple-misc-packages))

(define %lotus-simple-user-desktop-packages (list))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-simple-user-selected-packages (map specification->package
                                                  %lotus-simple-package-names-for-installation))

(define %lotus-simple-user-packages (append %lotus-simple-user-desktop-packages
                                            %lotus-simple-user-selected-packages))

(define %lotus-simple-packages (append %lotus-simple-user-packages))

(packages->manifest %lotus-simple-packages)
