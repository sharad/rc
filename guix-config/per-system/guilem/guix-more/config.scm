;; This is an operating system configuration generated
;; by the graphical installer.

;; TODO: debug emacs semantic (semantic-parse-region-default (point-min) (point-max) nil nil nil)


(define %lotus-system-init #f)


(use-modules (guix utils))
(use-modules (guix packages))
(use-modules (gnu services networking))
(use-modules (gnu) (gnu system nss))
(use-service-modules networking ssh)
(use-package-modules bootloaders certs suckless wm)

(use-service-modules desktop networking cups ssh xorg avahi mail)
(use-package-modules certs gnome cups)

(use-modules (gnu packages shells))
(use-modules (gnu packages vim))

(use-modules (gnu))
(use-package-modules screen)

;; other guix

(use-modules (gnu system locale))
(use-modules (guix store))
(use-modules (rnrs lists))
(use-modules (srfi srfi-1))
(use-package-modules base idutils)
(use-service-modules dns mcron messaging)

(define this-config-file (local-file (assoc-ref (current-source-location) 'filename)))
;; (define nm-dnsmasq-file
;;   (plain-file "cache.conf"
;;               "cache-size=1000\n"))

;; non-guix

;; Import nonfree linux module.
(if (not %lotus-system-init)
  (use-modules (nongnu packages linux)))


(define %lotus-account-uid                   1000)
(define %lotus-account-user-name             "s")
(define %lotus-account-comment               "sharad")
(define %lotus-account-group-name            "users")
(define %lotus-account-supplementry-groups   '("wheel" "netdev" "audio" "video"))
(define %lotus-account-home-parent-directory "/home")
(define %lotus-account-home-directory        (string-append %lotus-account-home-parent-directory "s/hell"))
(define %lotus-account-shell                 #~(string-append #$zsh "/bin/zsh"))
(define %lotus-account-create-home-directory #f)
(define %lotus-guix-substitute-urls '("https://ci.guix.gnu.org"
                                      "https://bayfront.guixsd.org"
                                      "http://guix.genenetwork.org"
                                      "https://guix.tobias.gr"
                                      "https://ci.guix.info/"
                                      "https://berlin.guixsd.org"))
(define %lotus-guix-extra-options   '(
                                      ;; "--max-jobs=2"
                                      ;; "--cores=1"
                                      "--gc-keep-derivations=yes"
                                      "--gc-keep-outputs=yes"))
(define %lotus-guix-use-substitutes #t) ;always true

(define %lotus-network-manager-dns "dnsmasq")


(use-modules (gnu packages linux))
(use-modules (guix modules))

(define (open-lvm-device source target)
  "Return a gexp that maps SOURCES to TARGETS as a LVM device, using
'lvm'."
  (with-imported-modules (source-module-closure '((gnu build file-systems)))
    #~(let ((source  #$source)
            (lvm-bin #$(file-append lvm2-static "/sbin/lvm")))
        ;; Use 'lvm2-static', not 'lvm2', to avoid pulling the
        ;; whole world inside the initrd (for when we're in an initrd).
        (begin
          (format #t "Enabling ~a~%" #$target)
          (sleep 3)
          (system* lvm-bin "vgscan" "--mknodes")
(sleep 3)
(system* lvm-bin "vgscan" "--mknodes")
(sleep 1)
(system* lvm-bin "vgscan" "--mknodes")
(sleep 1)
(system* lvm-bin "vgchange" "-ay" (car (string-split #$target #\-)))
(sleep 1)
(zero? (system* lvm-bin "lvchange" "-aay" "-y" "--sysinit" "--ignoreskippedcluster"
                (string-join (string-split #$target #\-) "/")))
(sleep 1)
(zero? (system* lvm-bin "lvchange" "-aay" "-y" "--sysinit" "--ignoreskippedcluster"
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

;; The type of LVM mapped devices.
(define lvm-device-mapping (mapped-device-kind (open open-lvm-device)
                                               ;; (check check-lvm-device)
                                               (close close-lvm-device)))





(define %lotus-mapped-device-guix-root       (mapped-device (source "/dev/sda31")
                                                            (target "guix-root")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-guix-boot       (mapped-device (source "/dev/sda31")
                                                            (target "guix-boot")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-guix-gnu        (mapped-device (source "/dev/sda31")
                                                            (target "guix-gnu")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-guix-swap       (mapped-device (source "/dev/sda31")
                                                            (target "guix-swap")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-guix-tmp        (mapped-device (source "/dev/sda31")
                                                            (target "guix-tmp")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-guix-var        (mapped-device (source "/dev/sda31")
                                                            (target "guix-var")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-vg01-lv01       (mapped-device (source "/dev/test")
                                                            (target "vg01-lv01")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-vg02-lv01       (mapped-device (source "/dev/test")
                                                            (target "vg02-lv01")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-vgres01-lvres01 (mapped-device (source "/dev/test")
                                                            (target "vgres01-lvres01")
                                                            (type   lvm-device-mapping)))

(define %lotus-mapped-device-house-home      (mapped-device (source "/dev/test")
                                                            (target "house-home")
                                                            (type   lvm-device-mapping)))


(define %lotus-mapped-devices (list %lotus-mapped-device-guix-root
                                    %lotus-mapped-device-guix-boot
                                    %lotus-mapped-device-guix-gnu
                                    %lotus-mapped-device-guix-var
                                    %lotus-mapped-device-guix-swap
                                    %lotus-mapped-device-guix-tmp
                                    %lotus-mapped-device-house-home
                                    %lotus-mapped-device-vgres01-lvres01
                                    %lotus-mapped-device-vg01-lv01
                                    %lotus-mapped-device-vg02-lv01))


;; (define %lotus-swap-devices '("/dev/mapper/guix-swap"))
;; (define %lotus-swap-devices '("/dev/guix/swap"))
(define device-mapping-guix-swap %lotus-mapped-device-guix-swap)
(define %lotus-swap-devices      '("/dev/mapper/guix-swap"))


(define %lotus-file-system-guix-root       (file-system (mount-point         "/")
                                                        (device              "/dev/mapper/guix-root")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        %lotus-mapped-devices)))

(define %lotus-file-system-guix-boot       (file-system (mount-point         "/boot")
                                                        (device              "/dev/mapper/guix-boot")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append (list %lotus-file-system-guix-root)
                                                                                     %lotus-mapped-devices))))

(define %lotus-file-system-guix-gnu        (file-system (mount-point         "/gnu")
                                                        (device              "/dev/mapper/guix-gnu")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append (list %lotus-file-system-guix-root)
                                                                                     %lotus-mapped-devices))))

(define %lotus-file-system-guix-tmp        (file-system (mount-point         "/tmp")
                                                        (device              "/dev/mapper/guix-tmp")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append (list %lotus-file-system-guix-root)
                                                                                     %lotus-mapped-devices))))

(define %lotus-file-system-guix-var        (file-system (mount-point         "/var")
                                                        (device              "/dev/mapper/guix-var")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append (list %lotus-file-system-guix-root)
                                                                                     %lotus-mapped-devices))))

(define %lotus-file-system-house-home      (file-system (mount-point         "/home")
                                                        (device              "/dev/mapper/house-home")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #f)))

(define %lotus-file-system-vg01-lv01       (file-system (mount-point         "/srv/volumes/local/vg01/lv01")
                                                        (device              "/dev/mapper/vg01-lv01")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #f)
                                                        (create-mount-point? #f)
                                                        (needed-for-boot?    #f)))

(define %lotus-file-system-vg02-lv01       (file-system (mount-point         "/srv/volumes/local/vg02/lv01")
                                                        (device              "/dev/mapper/vg02-lv01")
                                                        (type                "ext4")
                                                        (check?              #f)
                                                        (mount?              #f)
                                                        (create-mount-point? #f)
                                                        (needed-for-boot?    #f)))

(define %lotus-file-system-vgres01-lvres01 (file-system (mount-point         "/srv/volumes/local/vgres01/lvres01")
                                                        (device              "/dev/mapper/vgres01-lvres01")
                                                        (type                "reiserfs")
                                                        (check?              #f)
                                                        (mount?              #t)
                                                        (create-mount-point? #f)
                                                        (needed-for-boot?    #f)))


(define %lotus-file-system-boot-efi        (file-system (mount-point         "/boot/efi")
                                                        (device              (uuid "BAA8-1C0B" 'fat32))
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (type                "vfat")
                                                        (dependencies        (append (list %lotus-file-system-guix-boot
                                                                                           %lotus-file-system-guix-root)
                                                                                     %lotus-mapped-devices))))


(define %lotus-lvm-mount-home-file-systems         (if %lotus-system-init
                                                       (list)
                                                       (list %lotus-file-system-house-home)))

(define %lotus-lvm-unmount-home-file-systems       (list))


(define %lotus-mount-lvm-system-file-systems       (list %lotus-file-system-guix-root
                                                         ;; %lotus-file-system-guix-swap
                                                         %lotus-file-system-guix-boot
                                                         %lotus-file-system-guix-gnu
                                                         %lotus-file-system-guix-tmp
                                                         %lotus-file-system-guix-var))

(define %lotus-unmount-lvm-system-file-systems     (list))


(define %lotus-mount-lvm-non-system-file-systems   (list %lotus-file-system-vg01-lv01
                                                         ;; %lotus-file-system-vg02-lv01
                                                         %lotus-file-system-vgres01-lvres01))

(define %lotus-unmount-lvm-non-system-file-systems (list %lotus-file-system-vg02-lv01))


(define %lotus-lvm-system-file-systems             (append %lotus-mount-lvm-system-file-systems
                                                           %lotus-unmount-lvm-system-file-systems
                                                           %lotus-lvm-mount-home-file-systems
                                                           %lotus-lvm-unmount-home-file-systems
                                                           %lotus-mount-lvm-non-system-file-systems
                                                           %lotus-unmount-lvm-non-system-file-systems))


(define %lotus-lvm-file-systems                    (append %lotus-lvm-system-file-systems))


(define %lotus-other-file-systems                  (list %lotus-file-system-boot-efi))


(define %lotus-file-systems                        (append %lotus-lvm-file-systems
                                                           %lotus-other-file-systems
                                                           %base-file-systems))


;; packages
(load "packages.scm")
(define %lotus-packages (if %lotus-system-init
                            (cons* nss-certs lvm2 vim zsh %base-packages)
                            %lotus-reinit-packages))


(define %lotus-keyboard-layout (keyboard-layout "us" "altgr-intl"))


;; https://www.hubert-lombard.website/GuixSD/html/GuixSD-0.16_en-Dual-Boot-avec-Debian-Testing.html
(define %lotus-grub-other-menuentries '(menu-entries
	               				(list
						  (menu-entry
		        			  (label "Debian GNU/Linux")
	                			  (linux "/boot/vmlinuz-4.19.0-1-amd64")
	                			  (linux-arguments '("root=/dev/sda1"))
	                			  (initrd "/boot/initrd.img-4.19.0-1-amd64")))))

(define %lotus-vm-bootloader
  (bootloader-configuration (bootloader grub-bootloader)
                            (target     "/dev/vda")))

(define %lotus-efi-bootloader
  (bootloader-configuration (bootloader      grub-efi-bootloader)
                            (target          "/boot/efi")
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


(define %lotus-simple-users (list (user-account (uid                    %lotus-account-uid)
                                                (name                   %lotus-account-user-name)
                                                (comment                %lotus-account-comment)
                                                (group                  %lotus-account-group-name)
                                                (home-directory         (string-append %lotus-account-home-parent-directory "/" %lotus-account-user-name "/" "hell"))
                                                (shell                  %lotus-account-shell)
                                                (supplementary-groups   %lotus-account-supplementry-groups)
                                                (create-home-directory? %lotus-account-create-home-directory))

                                  (user-account (uid                    1002)
                                                (name                   "j")
                                                (comment                "Jam")
                                                (group                  %lotus-account-group-name)
                                                (home-directory         (string-append %lotus-account-home-parent-directory "/" "j"))
                                                (supplementary-groups   %lotus-account-supplementry-groups)
                                                (create-home-directory? %lotus-account-create-home-directory))))

(define %lotus-users (append %lotus-simple-users
                             %base-user-accounts))


(define %lotus-copy-current-config-file-in-etc (list (simple-service 'config-file etc-service-type
                                                                     ;; https://willschenk.com/articles/2019/installing_guix_on_nuc/
                                                                     ;; Copy current config to /etc/config.scm
                                                                     `(("config/config.scm"                   ,this-config-file)
                                                                       ;; ("dnsmasq.d/cache.conf" ,nm-dnsmasq-file)
                                                                       ("config/package.scm"                  ,this-package-file)))))


(define (remove-services types services)
  (remove! (lambda (service)
             (any (lambda (type) (eq? (service-kind service) type))
                  types))
           services))


;; Vixie cron schedular
(define updatedb-job
  ;; Run 'updatedb' at 3AM every day.  Here we write the
  ;; job's action as a Scheme procedure.
  #~(job '(next-hour '(3))
         (lambda ()
           (execl (string-append #$findutils "/bin/updatedb")
                  ;; "updatedb"
                  "--prunepaths=/tmp /var/tmp /gnu/store /run"))))

(define garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  ;; The job's action is a shell command.
  #~(job "5 0 * * *"            ;Vixie cron syntax
         "guix gc -F 1G"))

(define idutils-job
  ;; Update the index database as user "charlie" at 12:15PM
  ;; and 19:15PM.  This runs from the user's home directory.
  #~(job '(next-minute-from (next-hour '(12 19)) '(15))
         (string-append #$idutils "/bin/mkid src")
         #:user "s"))


;; https://guix.gnu.org/manual/en/html_node/Scheduled-Job-Execution.html
(define %lotus-mcron-services (list (service mcron-service-type
                                             (mcron-configuration (jobs (list garbage-collector-job
                                                                              ;; idutils-job
                                                                              updatedb-job))))))


(define %lotus-bitlbee-services (list (service bitlbee-service-type)))


;; https://guix.gnu.org/manual/en/html_node/Mail-Services.html
(define %lotus-mail-aliases-services (list (service mail-aliases-service-type
                                                    '(("postmaster" "bob")
                                                      ("bob"        "bob@example.com" "bob@example2.com")))))


;; https://lists.nongnu.org/archive/html/help-guix/2016-08/msg00061.html
;; https://wingolog.org/pub/alt-os-config.scm
(define %lotus-dovecot-services (list (dovecot-service #:config
                                                       (dovecot-configuration
                                                        (mail-location "maildir:~/.maildir")
                                                        (listen        '("127.0.0.1"))))))

(define %lotus-exim-services (list (service exim-service-type
                                            (exim-configuration
                                             (config-file #f)))))


;; https://notabug.org/thomassgn/guixsd-configuration/src/master/config.scm
;; https://guix.gnu.org/manual/en/html_node/Networking-Services.html
;; https://jonathansblog.co.uk/using-dnsmasq-as-an-internal-dns-server-to-block-online-adverts
;; https://stackoverflow.com/questions/48644841/multiple-addn-hosts-conf-in-dnsmasq
(define %lotus-dnsmasq-services (list (service dnsmasq-service-type
                                               (dnsmasq-configuration (no-resolv? #t)
                                                                      ;; (resolv-file)
                                                                      ;; (no-resolv? #f)
                                                                      ;; (servers '("82.196.9.45"
                                                                      ;;            "51.255.48.78"
                                                                      ;;            "51.15.98.97"))
                                                                      (local-service? #t)))))

;; https://guix.gnu.org/manual/en/html_node/Networking-Services.html
(define %lotus-network-manager-services (list (service network-manager-service-type
                                                       (network-manager-configuration (dns %lotus-network-manager-dns)))))

(define %lotus-avahi-services (list (service avahi-service-type)))


;; https://github.com/alezost/guix-config/blob/master/system-config/os-main.scm
(define %lotus-mingetty-services (list (service mingetty-service-type
                                                (mingetty-configuration (tty "tty1")))
                                       (service mingetty-service-type
                                                (mingetty-configuration (tty "tty2")))
                                       (service mingetty-service-type
                                                (mingetty-configuration (tty "tty3")))
                                       (service mingetty-service-type
                                                (mingetty-configuration (tty "tty4")))
                                       (service mingetty-service-type
                                                (mingetty-configuration (tty "tty5")))
                                       (service mingetty-service-type
                                                (mingetty-configuration (tty "tty6")))))


(define %lotus-cups-services (list (service cups-service-type
                                            (cups-configuration (web-interface? #t)
                                                                (default-paper-size "A4")
                                                                (extensions (list cups-filters
                                                                                  hplip-minimal))))))



(define %lotus-desktop-nm-services (modify-services %desktop-services
                                     (network-manager-service-type config =>
                                                                   (network-manager-configuration (inherit config)
                                                                                                  ;; (vpn-plugins '("network-manager-openconnect"))
                                                                                                  (dns "dnsmasq")))
                                     ;; https://gitlab.com/Efraim/guix-config/blob/master/macbook41_config.scm
                                     (guix-service-type config =>
                                                        (guix-configuration (inherit config)
                                                                            ;; (use-substitutes? %lotus-guix-use-substitutes)
                                                                            ;; (authorized-keys '())
                                                                            (substitute-urls (append %lotus-guix-substitute-urls
                                                                                                     %default-substitute-urls))
                                                                            (extra-options %lotus-guix-extra-options)))))

;; https://issues.guix.info/issue/35674
(when #t
  (set! %lotus-desktop-nm-services (modify-services %lotus-desktop-nm-services
                                     (gdm-service-type config =>
                                                       (gdm-configuration (inherit config)
                                                                          (xorg-configuration
                                                                           (xorg-configuration
                                                                            (keyboard-layout %lotus-keyboard-layout)))
                                                                          ;; (allow-empty-passwords? #t)
                                                                          (auto-login?            #f)
                                                                          (default-user           %lotus-account-user-name))))))


(define %lotus-desktop-services %lotus-desktop-nm-services)


(define %lotus-many-services (list (service openssh-service-type)
                                   ;; (service gnome-desktop-service-type)
                                   ;; (service xfce-desktop-service-type)
                                   ;; (service mate-desktop-service-type)
                                   ;; (service enlightenment-desktop-service-type)
                                   (service tor-service-type)))

(define %lotus-few-services  (list (service openssh-service-type)
                                   (service tor-service-type)))

(define %lotus-simple-services %lotus-few-services)

(define %lotus-simple-and-desktop-services (append %lotus-simple-services
                                                   %lotus-mail-aliases-services
                                                   %lotus-dovecot-services
                                                   %lotus-mcron-services
                                                   %lotus-cups-services
                                                   %lotus-desktop-services))


(define %lotus-base-with-dhcp-services
  (append (list (service dhcp-client-service-type)
                (service openssh-service-type
                         (openssh-configuration (port-number 2222))))
          %base-services))

(define %lotus-base-services %base-services)


(define %lotus-final-services (if %lotus-system-init
                                  %lotus-base-with-dhcp-services
                                  %lotus-simple-and-desktop-services))

(define %lotus-services       (append %lotus-copy-current-config-file-in-etc
                                      %lotus-final-services))


(define %lotus-firmware (if %lotus-system-init
                            %base-firmware
                            (list linux-firmware)))


;; https://github.com/alezost/guix-config/blob/master/system-config/os-main.scm

(define %lotus-locale "en_US.utf8")

(define %lotus-locate-names (list "en_US"
                                  "hi_IN"
                                  "ur_PK"
                                  "fa_IR"
                                  "ar_SA"))

(define %lotus-all-locale-definitions  (map (lambda (locale)
                                              (locale-definition (source locale)
                                                                 (name   (string-append locale "." "utf8"))))
                                            %lotus-locate-names))

(define %lotus-locale-definitions      (append %lotus-all-locale-definitions
                                               %default-locale-definitions))


(define %lotus-timezone  "Asia/Kolkata")


;; (define %lotus-host-name "komputilo")
(define %lotus-host-name "guilem")



(define %lotus-bootloader %lotus-efi-bootloader)
(define %lotus-initrd     %lotus-metal-initrd)


;; (define %lotus-setuid-programs %setuid-programs)

(define %lotus-setuid-programs (cons* #~(string-append #$ecryptfs-utils "/sbin/mount.ecryptfs_private")
                                      %setuid-programs))


(define %lotus-kernel (if %lotus-system-init
                          linux-libre
                          linux))


(operating-system
  (kernel              %lotus-kernel)
  (firmware            %lotus-firmware)
  (initrd              %lotus-initrd)
  (locale              %lotus-locale)
  (locale-definitions  %lotus-locale-definitions)
  (timezone            %lotus-timezone)
  (keyboard-layout     %lotus-keyboard-layout)
  (host-name           %lotus-host-name)
  (setuid-programs     %lotus-setuid-programs)
  (mapped-devices      %lotus-mapped-devices)
  (users               %lotus-users)
  (file-systems        %lotus-file-systems)
  ;; (swap-devices        %lotus-swap-devices)
  (bootloader          %lotus-bootloader)
  (packages            %lotus-packages)
  (services            %lotus-services)
  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))


;; TO SEE host-file
;; https://guix.gnu.org/manual/en/html_node/Networking-Services.html

