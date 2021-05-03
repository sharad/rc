
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


(define %lotus-essential-disputed-packages (list "sbcl"))
;; "notification-daemon" "trezord-udev-rules" "trezord"

(define %lotus-essential-device-hardware-packages (list))

(define %lotus-essential-other-packages (list "vim"))

(define %lotus-essential-lang-packages (list)) ;; "ocaml"
;; ;; "ocaml-merlin"
;; "opam"

(define %lotus-essential-audio-packages (list))

(define %lotus-essential-user-selected-package-names (list))

(define %lotus-essential-network-packages (list))

(define %lotus-essential-mail-packages (list))

(define %lotus-essential-font-packages (list))

(define %lotus-essential-media-packages (list))

(define %lotus-essential-gui-packages (list))

(define %lotus-essential-text-packages (list))

(define %lotus-essential-notification-packages (list))

(define %lotus-essential-test-packages (list))
;; "icecat"

(define %lotus-essential-file-packages (list))

(define %lotus-essential-misc-packages (list))


(define %lotus-essential-package-names-for-installation (append %lotus-essential-user-selected-package-names
                                                                %lotus-essential-lang-packages
                                                                %lotus-essential-audio-packages
                                                                %lotus-essential-disputed-packages
                                                                %lotus-essential-device-hardware-packages
                                                                %lotus-essential-other-packages
                                                                %lotus-essential-network-packages
                                                                %lotus-essential-mail-packages
                                                                %lotus-essential-font-packages
                                                                %lotus-essential-media-packages
                                                                %lotus-essential-gui-packages
                                                                %lotus-essential-text-packages
                                                                %lotus-essential-notification-packages
                                                                %lotus-essential-test-packages
                                                                %lotus-essential-file-packages
                                                                %lotus-essential-misc-packages))

(define %lotus-essential-user-desktop-packages (list))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-essential-user-selected-packages (map specification->package
                                                     %lotus-essential-package-names-for-installation))

(define %lotus-essential-user-packages (append %lotus-essential-user-desktop-packages
                                               %lotus-essential-user-selected-packages))

(define %lotus-essential-packages (append %lotus-essential-user-packages))

(packages->manifest %lotus-essential-packages)

