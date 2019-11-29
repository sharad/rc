;; This is an operating system configuration generated
;; by the graphical installer.


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

(define this-config-file
  (local-file (basename (assoc-ref (current-source-location)
                                   'filename))
              "config.scm"))

;; non-guix

;; Import nonfree linux module.
(use-modules (nongnu packages linux))

;; my packages

(use-modules (lotus packages perforce))


(use-modules (gnu packages linux))
(use-modules (guix modules))

(define (open-lvm-device source target)
  "Return a gexp that maps SOURCES to TARGETS as a LVM device, using
'lvm'."
  (with-imported-modules (source-module-closure
                          '((gnu build file-systems)))
                         #~(let ((source #$source))
                             ;; Use 'lvm2-static', not 'lvm2', to avoid pulling the
                             ;; whole world inside the initrd (for when we're in an initrd).
                             (begin
                               (system* #$(file-append lvm2-static "/sbin/lvm")
                                        "vgscan" "--mknodes")
                               (sleep 1)
                               (system* #$(file-append lvm2-static "/sbin/lvm")
                                        "vgscan" "--mknodes")
                               (sleep 1)
                               (system* #$(file-append lvm2-static "/sbin/lvm")
                                        "vgchange" "-ay" 
                                        (car (string-split #$target #\-)))
                               (sleep 1)
                               (zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                                               "lvchange" "-aay" "-y" "--sysinit" "--ignoreskippedcluster"
                                               (string-join (string-split #$target #\-) "/")))))))

(define (close-lvm-device sources target)
  "Return a gexp that closes TARGET, a LVM device."
  #~(zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                    "lvchange" "-an" "-y" (string-join (string-split #$target #\-) "/"))))

;; (define* (check-lvm-device md #:key
;;                             needed-for-boot?
;;                             (initrd-modules '())
;;                             #:allow-other-keys
;;                             #:rest rest)
;;   "Ensure the source of MD is valid."
;;   (let ((source   (mapped-device-source md))
;;         (location (mapped-device-location md)))
;;     (or (not (zero? (getuid)))
;;         (if (uuid? source)
;;             (match (find-partition-by-lvm-uuid (uuid-bytevector source))
;;               (#f
;;                (raise (condition
;;                        (&message
;;                         (message (format #f (G_ "no LVM partition with UUID '~a'")
;;                                          (uuid->string source))))
;;                        (&error-location
;;                         (location (source-properties->location
;;                                    (mapped-device-location md)))))))
;;               ((? string? device)
;;                (check-device-initrd-modules device initrd-modules location)))
;;             (check-device-initrd-modules source initrd-modules location)))))

(define lvm-device-mapping
  ;; The type of LVM mapped devices.
  (mapped-device-kind
   (open open-lvm-device)
   (close close-lvm-device)
   ;; (check check-lvm-device)
   ))


(define %lotus-mapped-device-guix-gnu (mapped-device
                                         (source "/dev/sda31")
                                         (target "guix-gnu")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-device-guix-root (mapped-device
                                          (source "/dev/sda31")
                                          (target "guix-root")
                                          (type lvm-device-mapping)))

(define %lotus-mapped-device-guix-swap (mapped-device
                                          (source "/dev/sda31")
                                          (target "guix-swap")
                                          (type lvm-device-mapping)))

(define %lotus-mapped-device-vg01-lv01 (mapped-device
                                        (source "/dev/test")
                                        (target "vg01-lv01")
                                        (type lvm-device-mapping)))

(define %lotus-mapped-device-vg02-lv01 (mapped-device
                                         (source "/dev/test")
                                         (target "vg02-lv01")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-device-vgres01-lvres01 (mapped-device
                                              (source "/dev/test")
                                              (target "vgres01-lvres01")
                                              (type lvm-device-mapping)))

(define %lotus-mapped-device-house-home (mapped-device
                                         (source "/dev/sda8")
                                         (target "house-home")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-device-ubuntu-local (mapped-device
                                         (source "/dev/sda10")
                                         (target "ubuntu-local")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-device-ubuntu-opt (mapped-device
                                         (source "/dev/sda10")
                                         (target "ubuntu-opt")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-devices
  (list
   %lotus-mapped-device-guix-root
   %lotus-mapped-device-guix-gnu
   %lotus-mapped-device-vg01-lv01
   %lotus-mapped-device-vg02-lv01
   %lotus-mapped-device-vgres01-lvres01
   %lotus-mapped-device-house-home
   %lotus-mapped-device-ubuntu-local
   %lotus-mapped-device-ubuntu-opt
   %lotus-mapped-device-guix-swap))


;; (define %lotus-swap-devices '("/dev/mapper/guix-swap"))
(define %lotus-swap-devices '("/dev/ubuntu/swap"))


(define %lotus-file-system-guix-root (file-system
                                       (mount-point "/")
                                       (device "/dev/mapper/guix-root")
                                       (type "ext4")
                                       (check? #f)
                                       (mount? #t)
                                       (create-mount-point? #t)
                                       (needed-for-boot? #t)
                                       (dependencies %lotus-mapped-devices)))

(define %lotus-file-system-guix-gnu (file-system
                                        (mount-point "/gnu")
                                        (device "/dev/mapper/guix-gnu")
                                        (type "ext4")
                                        (check? #f)
                                        (mount? #t)
                                        (create-mount-point? #t)
                                        (needed-for-boot? #t)
                                        (dependencies (append
                                                        (list %lotus-file-system-guix-root)
                                                        %lotus-mapped-devices))))

(define %lotus-file-system-vg01-lv01 (file-system
                                      (mount-point "/srv/volumes/local/vg01/lv01")
                                      (device "/dev/mapper/vg01-lv01")
                                      (type "ext4")
                                      (check? #f)
                                      (mount? #t)
                                      (create-mount-point? #f)
                                      (needed-for-boot? #f)))

(define %lotus-file-system-vg02-lv01 (file-system
                                      (mount-point "/srv/volumes/local/vg02/lv01")
                                      (device "/dev/mapper/vg02-lv01")
                                      (type "ext4")
                                      (check? #f)
                                      (mount? #t)
                                      (create-mount-point? #f)
                                      (needed-for-boot? #f)))

(define %lotus-file-system-vgres01-lvres01 (file-system
                                            (mount-point "/srv/volumes/local/vgres01/lvres01")
                                            (device "/dev/mapper/vgres01-lvres01")
                                            (type "reiserfs")
                                            (check? #f)
                                            (mount? #t)
                                            (create-mount-point? #f)
                                            (needed-for-boot? #f)))

(define %lotus-file-system-house-home (file-system
                                       (mount-point "/home")
                                       (device "/dev/mapper/house-home")
                                       (type "ext4")
                                       (check? #f)
                                       (mount? #t)
                                       (create-mount-point? #f)
                                       (needed-for-boot? #f)))

(define %lotus-file-system-ubuntu-local (file-system
                                         (mount-point "/usr/local")
                                         (device "/dev/mapper/ubuntu-local")
                                         (type "ext4")
                                         (check? #f)
                                         (mount? #t)
                                         (create-mount-point? #f)
                                         (needed-for-boot? #f)))

(define %lotus-file-system-ubuntu-opt (file-system
                                       (mount-point "/opt")
                                       (device "/dev/mapper/ubuntu-opt")
                                       (type "ext4")
                                       (check? #f)
                                       (mount? #t)
                                       (create-mount-point? #f)
                                       (needed-for-boot? #f)))

(define %lotus-lvm-file-systems
  (list
   ;; %lotus-file-system-guix-swap
   %lotus-file-system-guix-root
   %lotus-file-system-guix-gnu
   %lotus-file-system-vg01-lv01
   %lotus-file-system-vg02-lv01
   %lotus-file-system-vgres01-lvres01
   %lotus-file-system-house-home
   ;; %lotus-file-system-ubuntu-local
   %lotus-file-system-ubuntu-opt))

(define %lotus-file-system-boot-efi (file-system
                                     (mount-point "/boot/efi")
                                     (device (uuid "BAA8-1C0B" 'fat32))
                                     ;; (check? #f)
                                     (mount? #t)
                                     (create-mount-point? #t)
                                     (needed-for-boot? #t)
                                     (type "vfat")
                                     (dependencies (append
                                                    (list %lotus-file-system-guix-root)
                                                    %lotus-mapped-devices))))

(define %lotus-file-system-guix-other (file-system
                                        (mount-point "/srv/misc/guix-other")
                                        (device
                                         (uuid "d913fd98-61b6-43e0-a5dc-504b14ad9aee"
                                               'ext4))
                                        (type "ext4")))

(define %lotus-other-file-systems
  (list %lotus-file-system-boot-efi))
;; %lotus-file-system-guix-other


(define %lotus-file-systems
  (append %lotus-lvm-file-systems
          %lotus-other-file-systems
          %base-file-systems))



;; packages
(load "packages.scm")


(define %lotus-keyboard-layout (keyboard-layout "us" "altgr-intl"))


(define %lotus-vm-bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/vda")))

(define %lotus-efi-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout %lotus-keyboard-layout)))


(define %lotus-vm-initrd
  (lambda (file-systems . rest)
    (apply base-initrd file-systems
           #:extra-modules '("virtio.ko"
                             "virtio_balloon.ko"
                             "virtio_ring.ko"
                             "virtio_blk.ko"
                             "virtio_pci.ko"
                             "virtio_net.ko")
           rest)))

(define %lotus-metal-initrd base-initrd)


;; (define %lotus-simple-group (list (user-group
;;                                    (id 1000)
;;                                    (name "users"))))

(define %lotus-simple-users (list (user-account
                                   (uid 1000)
                                   (name "s")
                                   (comment "sharad")
                                   (group "users")
                                   (home-directory "/home/s/hell")
                                   (shell #~(string-append #$zsh "/bin/zsh"))
                                   (supplementary-groups
                                    '("wheel" "netdev" "audio" "video")))
                                  (user-account
                                   (uid 1002)
                                   (name "j")
                                   (comment "Jam")
                                   (group "users")
                                   (home-directory "/home/j")
                                   (supplementary-groups
                                    '("wheel" "netdev" "audio" "video")))))

(define %lotus-users (append %lotus-simple-users
                             %base-user-accounts))


(define %lotus-copy-current-config-file-in-etc (list
                                         ;; https://willschenk.com/articles/2019/installing_guix_on_nuc/
                                         ;; Copy current config to /etc/config.scm
                                         (simple-service 'config-file etc-service-type
                                                         `(("config.scm" ,this-config-file)))))

(define %lotus-many-services (list (service gnome-desktop-service-type)
                                   ;; (service xfce-desktop-service-type)
                                   ;; (service mate-desktop-service-type)
                                   ;; (service enlightenment-desktop-service-type)
                                   (service openssh-service-type)
                                   ;; (service tor-service-type)
                                   (set-xorg-configuration
                                    (xorg-configuration
                                     (keyboard-layout %lotus-keyboard-layout)))))

(define %lotus-few-services    (list (service gnome-desktop-service-type)
                                     (service xfce-desktop-service-type)
                                     (service openssh-service-type)
                                     (service tor-service-type)
                                     (set-xorg-configuration
                                      (xorg-configuration
                                       (keyboard-layout %lotus-keyboard-layout)))))

(define %lotus-simple-services %lotus-few-services)

(define %lotus-simple-and-desktop-services (append %lotus-copy-current-config-file-in-etc
                                                   %lotus-simple-services
                                                   %desktop-services))

(define %lotus-desktop-services (append %desktop-services))
(define %lotus-base-with-dhcp-services
  (append (list (service dhcp-client-service-type)
                (service openssh-service-type
                         (openssh-configuration
                          (port-number 2222))))
          %base-services))

(define %lotus-base-services %base-services)

(define %lotus-services  %lotus-simple-and-desktop-services)


(define %lotus-firmware (list linux-firmware))


;; https://github.com/alezost/guix-config/blob/master/system-config/os-main.scm

(define %lotus-locale "en_US.utf8")

(define %lotus-en-us-locale-definition (locale-definition (source "en_US")
                                                          (name   "en_US.utf8")))

(define %lotus-hi-in-locale-definition (locale-definition (source "hi_IN")
                                                          (name   "hi_IN.utf8")))

(define %lotus-ur-pk-locale-definition (locale-definition (source "ur_PK")
                                                          (name   "ur_PK.utf8")))

(define %lotus-fa-ir-locale-definition (locale-definition (source "fa_IR")
                                                          (name   "fa_IR.utf8")))

(define %lotus-ar-sa-locale-definition (locale-definition (source "ar_SA")
                                                          (name   "ar_SA.utf8")))

(define %lotus-locale-definitions (list %lotus-en-us-locale-definition
                                        %lotus-hi-in-locale-definition
                                        %lotus-ur-pk-locale-definition
                                        %lotus-ar-sa-locale-definition))


(define %lotus-timezone "Asia/Kolkata")


;; (define %lotus-host-name "komputilo")
(define %lotus-host-name "guilem")



(define %lotus-bootloader %lotus-efi-bootloader)
(define %lotus-initrd     %lotus-metal-initrd)


;; (define %lotus-setuid-programs %setuid-programs)

(define %lotus-setuid-programs (cons* #~(string-append #$ecryptfs-utils "/sbin/mount.ecryptfs_private")
                                      %setuid-programs))


(define %lotus-kernel linux)


(operating-system
 (kernel             %lotus-kernel)
 (firmware           %lotus-firmware)
 (initrd             %lotus-initrd)
 (locale             %lotus-locale)
 (locale-definitions %lotus-locale-definitions)
 (timezone           %lotus-timezone)
 (keyboard-layout    %lotus-keyboard-layout)
 (host-name          %lotus-host-name)
 (setuid-programs    %lotus-setuid-programs)
 (mapped-devices     %lotus-mapped-devices)
 (users              %lotus-users)
 (file-systems       %lotus-file-systems)
 (swap-devices       %lotus-swap-devices)
 (bootloader         %lotus-bootloader)
 (packages           %lotus-packages)
 (services           %lotus-services)
 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))

