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


;; Import nonfree linux module.
(use-modules (nongnu packages linux))


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
          (sleep 3)
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


(define %lotus-mapped-device-guix-store (mapped-device
                                         (source "/dev/sda31")
                                         (target "guix-store")
                                         (type lvm-device-mapping)))

(define %lotus-mapped-device-guix-root (mapped-device
                                          (source "/dev/sda31")
                                          (target "guix-root")
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

(define %lotus-mapped-device-ubuntu-swap (mapped-device
                                          (source "/dev/sda10")
                                          (target "ubuntu-swap")
                                          (type lvm-device-mapping)))
(define %lotus-mapped-devices
  (list
   %lotus-mapped-device-guix-root
   %lotus-mapped-device-guix-store
   %lotus-mapped-device-vg01-lv01
   %lotus-mapped-device-vg02-lv01
   %lotus-mapped-device-vgres01-lvres01
   %lotus-mapped-device-house-home
   %lotus-mapped-device-ubuntu-local
   %lotus-mapped-device-ubuntu-opt
   %lotus-mapped-device-ubuntu-swap))


;; (define %lotus-swap-devices '("/dev/mapper/ubuntu-swap"))
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

(define %lotus-file-system-guix-store (file-system
                                        (mount-point "/gnu")
                                        (device "/dev/mapper/guix-store")
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
    ;; %lotus-file-system-ubuntu-swap
    %lotus-file-system-guix-root
    %lotus-file-system-guix-store
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



(define %lotus-system-selected-package-names
  (list
   "m4"
   "binutils"
   ;; "coreutils"
   ;; "diffutils"
   ;; "findutils"
   ;; "gnu-make"
   ;; "patch"
   "shroud"
   "gdm"
   "gpm"
   "git"
   "git-remote-gcrypt"
   "guile-colorized"
   "file"
   "font-lohit"
   "screen"
   "tmux"
   "kitty"
   "lxqt-openssh-askpass"
   "gettext"
   "ecryptfs-utils"
   "zsh"
   "vim"
   "the-silver-searcher"
   "emacs-ag"
   "emacs-helm-ag"
   "emacs"
   "rxvt-unicode"
   "sakura"
   "seahorse"
   "libxft"
   "scsh"
   "openbox"
   "awesome"
   "i3-wm"
   "gparted"
   "parted"
   "ncurses-with-gpm"
   "ncurses"
   "stumpwm"
   "guile-wm"
   ;; "stumpwm-with-slynk"
   ;; "cl-stumpwm"
   "emacs-stumpwm-mode"
   "i3status"
   "dmenu"
   "st"
   "xrdb"
   "xterm"
   "xdotool"
   "xrandr"
   "arandr"
   "autorandr"
   "xrandr-invert-colors"
   "aspell"
   "xautolock"
   "slock"
   "xsetroot"
   "gcc-toolchain"
   "strace"
   "guile-readline"
   "stapler"
   "pidgin"
   "pidgin-otr"
   "telegram-purple"
   "glibc-utf8-locales"))

(define %lotus-other-packages
  (list 
   "xmodmap"
   ;; at
   "curl"
   "python"
   "ruby"
   "autocutsel"
   "xcompmgr"
   "wget"
   ;; "notify-send"
   ;; "notify"
   "xmlstarlet"  
   "xwininfo"
   "xmlstarlet"
   "imagemagick"
   ))

(define %lotus-package-names-for-installation 
  (append 
       %lotus-other-packages
       %lotus-system-selected-package-names))

(define %lotus-system-desktop-packages (list 
					 lvm2
                                         ;; for HTTPS access
                                         nss-certs
                                         ;; for user mounts
                                         gvfs))


(define %lotus-system-selected-packages
  (map specification->package
       %lotus-package-names-for-installation))

(define %lotus-system-packages (append %lotus-system-desktop-packages
                                      %lotus-system-selected-packages))

(define %lotus-packages (append %lotus-system-packages
                                %base-packages))


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


(define %lotus-simple-users (list
                             (user-account
                              (name "s")
                              (comment "Bob's sister")
                              (group "users")
                              (home-directory "/home/s/hell")
			      (shell #~(string-append #$zsh "/bin/zsh"))
                              (supplementary-groups
                               '("wheel" "netdev" "audio" "video")))
			     (user-account
                               (name "j")
                               (comment "Jam")
                               (group "users")
                               (home-directory "/home/j")
                               (supplementary-groups
                               '("wheel" "netdev" "audio" "video")))
			     ))

(define %lotus-users (append %lotus-simple-users
                             %base-user-accounts))


 (define %lotus-simple-services (list (service gnome-desktop-service-type)
                                      (service xfce-desktop-service-type)
                                      (service mate-desktop-service-type)
                                      (service enlightenment-desktop-service-type)
                                      (service openssh-service-type)
                                      (service tor-service-type)
                                      (set-xorg-configuration
                                      (xorg-configuration
                                        (keyboard-layout %lotus-keyboard-layout)))))

 (define %lotus-services (append %lotus-simple-services
                                 %desktop-services))

;; (define %lotus-services (append %desktop-services))
;; (define %lotus-services (append (list (service dhcp-client-service-type)
;;                                      (service openssh-service-type
;;                                               (openssh-configuration
;;                                                (port-number 2222))))
;;                                %base-services))

;; (define %lotus-services (append (list (service dhcp-client-service-type)
;;                                      (service openssh-service-type
;;                                                (openssh-configuration
;;                                                (port-number 2222))))
;;                                %base-services))

;; (define %lotus-services (append (list)
;;                                %base-services))



(define %lotus-firmware (list linux-firmware))


(define %lotus-locale "en_US.utf8")


;; (define %lotus-timezone "Europe/Berlin")
(define %lotus-timezone "Asia/Kolkata")


;; (define %lotus-host-name "komputilo")
(define %lotus-host-name "gscheme")



(define %lotus-bootloader %lotus-efi-bootloader)
(define %lotus-initrd %lotus-metal-initrd)


(define %lotus-setuid-programs (cons*
                                 #~(string-append #$ecryptfs-utils "/sbin/mount.ecryptfs_private")
                                 %setuid-programs))


(operating-system
 (kernel          linux)
 (firmware        %lotus-firmware)
 (initrd          %lotus-initrd)
 (locale          %lotus-locale)
 (timezone        %lotus-timezone)
 (keyboard-layout %lotus-keyboard-layout)
 (host-name       %lotus-host-name)
 (setuid-programs %lotus-setuid-programs)
 (mapped-devices  %lotus-mapped-devices)
 (users           %lotus-users)
 (file-systems    %lotus-file-systems)
 (swap-devices    %lotus-swap-devices)
 (bootloader      %lotus-bootloader)
 (packages        %lotus-packages)
 (services        %lotus-services)
 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))

