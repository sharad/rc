;; This is an operating system configuration generated
;; by the graphical installer.

;; TODO: debug emacs semantic (semantic-parse-region-default (point-min) (point-max) nil nil nil)


(define %lotus-system-init #f)
(define %lotus-use-nongnu  #t)

(define nongnu-desktop?    (and %lotus-use-nongnu
                                (not %lotus-system-init)))


(use-modules (guix utils))
(use-modules (guix packages))
(use-modules (gnu services networking))
(use-modules (gnu packages messaging))
(use-modules (lotus packages chat))
(use-modules (gnu services audio))
(use-modules (gnu services kerberos))
(use-modules (gnu) (gnu system nss))
(use-modules (guix channels))
(use-modules (guix inferior))
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

;; non-guix

;; Import nonfree linux module.
(when nongnu-desktop?
  (use-modules (lotus packages utils))
  (use-modules (nongnu packages linux)
               (nongnu system linux-initrd)))


(define this-config-file (local-file (assoc-ref (current-source-location) 'filename)))


(when (not %lotus-system-init)
  (load "local/local.scm"))

(define (lotus-local-value local alternat)
  (if (not %lotus-system-init)
      local
      alternat))


(define %lotus-bitlbee-service-use-default?     #f)
(define %lotus-host-name                        (lotus-local-value %local-host-name "komputilo"))
(define %lotus-account-uid                      1000)
(define %lotus-account-user-name                "s")
(define %lotus-account-comment                  "sharad")
(define %lotus-account-group-name               "users")
(define %lotus-account-group-gid                 1000)
(define %lotus-account-supplementry-groups      '("wheel" "netdev" "audio" "video"))
(define %lotus-account-home-parent-directory    "/home")
(define %lotus-account-shell                    #~(string-append #$zsh "/bin/zsh"))
(define %lotus-gdm-auto-login                   #f)
(define %lotus-gdm-allow-empty-password         #t)

;; (define %lotus-nm-dnsmasq-ns-path            #~(string-append #$nm-dnsmasq-ns "/etc/NetworkManager/dnsmasq.d"))

(define %lotus-account-create-home-directory    #f)
(define %lotus-guix-substitute-urls             '("https://ci.guix.gnu.org"
                                                  ;; "https://bayfront.guixsd.org"
                                                  ;; "http://guix.genenetwork.org" -- Backtrace
                                                  "https://guix.tobias.gr"
                                                  "https://ci.guix.info/"
                                                  ;; "https://berlin.guixsd.org"
                                                  "https://berlin.guix.gnu.org"))
(define %lotus-guix-extra-options               '(
                                                  ;; "--max-jobs=2"
                                                  ;; "--cores=1"
                                                  "--gc-keep-derivations=yes"
                                                  "--gc-keep-outputs=yes"))
(define %lotus-guix-use-substitutes             #t) ;always true

(define %lotus-network-manager-dns              "dnsmasq")
;; (define %lotus-default-realm                    #f)

(define %lotus-fs-guix-check?                   #t)
(define %lotus-fs-guix-root-check?              %lotus-fs-guix-check?)
(define %lotus-fs-guix-boot-check?              %lotus-fs-guix-check?)
(define %lotus-fs-guix-gnu-check?               %lotus-fs-guix-check?)
(define %lotus-fs-guix-tmp-check?               %lotus-fs-guix-check?)
(define %lotus-fs-guix-var-check?               %lotus-fs-guix-check?)

(define %lotus-fs-house-check?                  #t)
(define %lotus-fs-house-home-check?             %lotus-fs-house-check?)

(define %lotus-guix-boot-mount?                 #f)
(define %lotus-guix-boot-create-mount-point?    #f)
(define %lotus-guix-boot-needed-for-boot?       #f)
(define %lotus-guix-bootefi-mount?              #f)
(define %lotus-guix-bootefi-create-mount-point? #f)
(define %lotus-guix-bootefi-needed-for-boot?    #f)


(use-modules (gnu packages linux))
(use-modules (guix modules))

(define (open-non-udev-lvm-device source targets)
  "Return a gexp that maps SOURCES to TARGETSS as a LVM device, using
'lvm'."
  (with-imported-modules (source-module-closure '((gnu build file-systems)))
                         #~(let ((source   #$source)
                                 (targets  '#$targets)
                                 (lvm-bin  #$(file-append lvm2-static "/sbin/lvm")))
                             ;; Use 'lvm2-static', not 'lvm2', to avoid pulling the
                             ;; whole world inside the initrd (for when we're in an initrd).
                             (begin
                               (format #t "Enabling ~a~%" '#$targets)
                               (sleep 1)
                               (system* lvm-bin "vgscan" "--mknodes")
                               (sleep 1)
                               (map (lambda (file) (system* lvm-bin "vgchange" "-ay" (car (string-split file #\-))))
                                    '#$targets)
                               (sleep 1)
                               (map (lambda (file) (zero? (system* lvm-bin "lvchange" "-aay" "-y" "--sysinit" "--ignoreskippedcluster"
                                                                   (string-join (string-split file #\-) "/"))))
                                    '#$targets)
                               #t))))

(define (close-non-udev-lvm-device sources targets)
  "Return a gexp that closes TARGETS, a LVM device."
  #~(begin
      (map (lambda (file) (zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                                          "lvchange" "-an" "-y" (string-join (string-split file #\-) "/"))))
           '#$targets)
      #t))

;; (define* (check-non-udev-lvm-device md #:key
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
(define non-udev-lvm-device-mapping (mapped-device-kind (open open-non-udev-lvm-device)
                                                        ;; (check check-non-udev-lvm-device)
                                                        (close close-non-udev-lvm-device)))


(define (open-udev-lvm-device source targets)
  "Return a gexp that maps SOURCES to TARGETSS as a LVM device, using
'lvm'."
  (with-imported-modules (source-module-closure '((gnu build file-systems)))
                         #~(let ((source   #$source)
                                 (targets  '#$targets)
                                 (lvm-bin  #$(file-append lvm2-static "/sbin/lvm")))
                             ;; Use 'lvm2-static', not 'lvm2', to avoid pulling the
                             ;; whole world inside the initrd (for when we're in an initrd).
                             (begin
                               (format #t "Enabling ~a~%" '#$targets)
                               (map (lambda (file) (system* lvm-bin "vgchange" "-ay" (car (string-split file #\-))))
                                    '#$targets)
                               (sleep 1)
                               (map (lambda (file) (zero? (system* lvm-bin "lvchange" "-aay" "-y" "--sysinit" "--ignoreskippedcluster"
                                                                   (string-join (string-split file #\-) "/"))))
                                    '#$targets)
                               #t))))

(define (close-udev-lvm-device sources targets)
  "Return a gexp that closes TARGETS, a LVM device."
  #~(begin
      (map (lambda (file) (zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                                          "lvchange" "-an" "-y" (string-join (string-split file #\-) "/"))))
           '#$targets)
      #t))

;; (define* (check-udev-lvm-device md #:key
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
(define udev-lvm-device-mapping (mapped-device-kind (open open-udev-lvm-device)
                                                    ;; (check check-udev-lvm-device)
                                                    (close close-udev-lvm-device)))


(define %lotus-mapped-device-guix-root       (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-root"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-guix-boot       (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-boot"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-guix-gnu        (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-gnu"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-guix-swap       (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-swap"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-guix-tmp        (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-tmp"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-guix-var        (mapped-device (source "/dev/sda31")
                                                            (target (string-append %local-disk-serial-id "X" "guix-var"))
                                                            (type   non-udev-lvm-device-mapping)))

(define %lotus-mapped-device-house-home      (mapped-device (source "/dev/test")
                                                            (target (string-append %local-disk-serial-id "X" "house-home"))
                                                            (type   non-udev-lvm-device-mapping)))


(define %lotus-mapped-devices (list %lotus-mapped-device-guix-root
                                    %lotus-mapped-device-guix-boot
                                    %lotus-mapped-device-guix-gnu
                                    %lotus-mapped-device-guix-var
                                    %lotus-mapped-device-guix-swap
                                    %lotus-mapped-device-guix-tmp
                                    %lotus-mapped-device-house-home))


;; guix system: error: service 'swap-/dev/mapper/guix-swap' requires 'device-mapping-guix-swap', which is not provided by any service
(define %lotus-swap-devices      (if #t
                                  (list (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-swap"))
                                  (list)))


(define %lotus-file-system-guix-root       (file-system (mount-point         "/")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-root"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-guix-root-check?)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)))

(define %lotus-file-system-guix-boot       (file-system (mount-point         "/boot")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-boot"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-guix-boot-check?)
                                                        (mount?              %lotus-guix-boot-mount?)
                                                        (create-mount-point? %lotus-guix-boot-create-mount-point?)
                                                        (needed-for-boot?    %lotus-guix-boot-needed-for-boot?)
                                                        (dependencies        (append ;; %lotus-mapped-devices
                                                                                     (list %lotus-file-system-guix-root)))))

(define %lotus-file-system-guix-gnu        (file-system (mount-point         "/gnu")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-gnu"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-guix-gnu-check?)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append ;; %lotus-mapped-devices
                                                                                     (list %lotus-file-system-guix-root)))))

(define %lotus-file-system-guix-tmp        (file-system (mount-point         "/tmp")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-tmp"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-guix-tmp-check?)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append  ;; %lotus-mapped-devices
                                                                                     (list %lotus-file-system-guix-root)))))

(define %lotus-file-system-guix-var        (file-system (mount-point         "/var")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "guix-var"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-guix-var-check?)
                                                        (mount?              #t)
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #t)
                                                        (dependencies        (append ;; %lotus-mapped-devices
                                                                                     (list %lotus-file-system-guix-root)))))

(define %lotus-file-system-house-home      (file-system (mount-point         "/home")
                                                        (device              (string-append "/dev/mapper/" %local-disk-serial-id "X" "house-home"))
                                                        (type                "ext4")
                                                        (check?              %lotus-fs-house-home-check?)
                                                        (mount?              (if %lotus-system-init #f #t))
                                                        (create-mount-point? #t)
                                                        (needed-for-boot?    #f)
                                                        (dependencies        (append ;; %lotus-mapped-devices
                                                                                     (list))))) ;; %lotus-file-system-guix-root


(define %lotus-file-system-boot-efi        (file-system (mount-point         "/boot/efi")
                                                        (device              (lotus-local-value %local-fs-boot-efi-partition
                                                                                                "/dev/sda1"))
                                                        (mount?              %lotus-guix-bootefi-mount?)
                                                        (create-mount-point? %lotus-guix-bootefi-create-mount-point?)
                                                        (needed-for-boot?    %lotus-guix-bootefi-needed-for-boot?)
                                                        (type                "vfat")
                                                        (dependencies        (append ;; %lotus-mapped-devices
                                                                                     (list %lotus-file-system-guix-boot
                                                                                           %lotus-file-system-guix-root)))))


(define %lotus-lvm-mount-home-file-systems         (if %lotus-system-init
                                                       (list)
                                                       (list %lotus-file-system-house-home)))

(define %lotus-lvm-unmount-home-file-systems       (if %lotus-system-init
                                                       (list %lotus-file-system-house-home)
                                                       (list)))


(define %lotus-mount-lvm-system-file-systems       (list %lotus-file-system-guix-root
                                                         ;; %lotus-file-system-guix-swap
                                                         %lotus-file-system-guix-boot
                                                         %lotus-file-system-guix-gnu
                                                         %lotus-file-system-guix-tmp
                                                         %lotus-file-system-guix-var))

(define %lotus-unmount-lvm-system-file-systems     (list))


(define %lotus-mount-lvm-non-system-file-systems   (lotus-local-value %local-mount-lvm-non-system-file-systems
                                                                      (list)))

(define %lotus-unmount-lvm-non-system-file-systems (lotus-local-value %local-unmount-lvm-non-system-file-systems
                                                                      (list)))


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
(if (not %lotus-system-init)
    (load "packages.scm"))

(define %lotus-packages (if %lotus-system-init
                            (cons* nss-certs lvm2 vim zsh %base-packages)
                            %lotus-reinit-packages))


(define %lotus-keyboard-layout (keyboard-layout "us" "altgr-intl"))


;; https://gist.github.com/trzczy/517c5ec6459b2fe47197c489f092afae
;; https://www.hubert-lombard.website/GuixSD/html/GuixSD-0.16_en-Dual-Boot-avec-Debian-Testing.html
;; https://www.gnu.org/software/grub/manual/grub/html_node/Device-syntax.html

(define %lotus-grub-ubuntu-menuentries (lotus-local-value %local-grub-ubuntu-menuentries
                                                          (list)))

(define %lotus-vm-bootloader
  (bootloader-configuration (bootloader grub-bootloader)
                            (target     "/dev/vda")))

(define %lotus-efi-bootloader
  (bootloader-configuration (bootloader      grub-efi-bootloader)
                            (target          "/boot/efi")
                            (keyboard-layout %lotus-keyboard-layout)
                            ;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
                            ;; (terminal-outputs '(console gfxterm))
                            ;; (terminal-inputs '(console))
                            (menu-entries    %lotus-grub-ubuntu-menuentries)))

(define %lotus-nonefi-bootloader
  (bootloader-configuration (bootloader      grub-bootloader)
                            (target          "/dev/sda")
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

(define %lotus-nongnu-metal-initrd microcode-initrd)


(define %lotus-simple-groups (list (user-group (name %lotus-account-group-name)
                                               (id   %lotus-account-group-gid))))

(define %lotus-groups        (append %lotus-simple-groups %base-groups))


(define %lotus-simple-users  (list (user-account (uid                    %lotus-account-uid)
                                                 (name                   %lotus-account-user-name)
                                                 (comment                %lotus-account-comment)
                                                 (group                  %lotus-account-group-name)
                                                 (home-directory         (string-append %lotus-account-home-parent-directory "/" %lotus-account-user-name "/" "hell"))
                                                 (shell                  %lotus-account-shell)
                                                 (supplementary-groups   %lotus-account-supplementry-groups)
                                                 (create-home-directory? %lotus-account-create-home-directory))

                                   (user-account (uid                    (+ %lotus-account-uid 2))
                                                 (name                   "j")
                                                 (comment                "Jam")
                                                 (group                  %lotus-account-group-name)
                                                 (home-directory         (string-append %lotus-account-home-parent-directory "/" "j"))
                                                 (supplementary-groups   %lotus-account-supplementry-groups)
                                                 (create-home-directory? %lotus-account-create-home-directory))))

(define %lotus-users        (append %lotus-simple-users
                                    %base-user-accounts))


;; https://guix.gnu.org/manual/devel/en/html_node/Unattended-Upgrades.html
;; https://lists.gnu.org/archive/html/help-guix/2019-07/msg00180.html

(define %lotus-copy-current-config-file-in-etc (list (simple-service 'config-file etc-service-type
                                                                     ;; https://willschenk.com/articles/2019/installing_guix_on_nuc/
                                                                     ;; Copy current config to /etc/config.scm
                                                                     (append
                                                                      (list `("config/config.scm" ,this-config-file))
                                                                      ;; ("dnsmasq.d/cache.conf" ,nm-dnsmasq-file)
                                                                      (if (not %lotus-system-init)
                                                                          (list
                                                                           `("config/package.scm" ,this-package-file))
                                                                          (list))
                                                                      (if (not %lotus-system-init)
                                                                          (list)
                                                                          ;; `("NetworkManager/dnsmasq.d" ,%lotus-nm-dnsmasq-ns-path)
                                                                          (list))))))


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
                  "--prunepaths=`/tmp /var/tmp /gnu/store /run'"))))

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


;; https://guix.gnu.org/es/blog/2020/improve-internationalization-support-for-the-guix-data-service/
;; https://guix.gnu.org/en/blog/2020/introduction-to-the-guix-data-service-the-missing-blog-post/
;; https://data.guix.gnu.org/README
;; https://guix.gnu.org/manual/en/html_node/Guix-Services.html
;; https://guix.gnu.org/manual/en/guix.html#Debugging-Build-Failures
;; https://guix.gnu.org/manual/en/html_node/Continuous-Integration.html
;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
(define %lotus-publish-services (list (service guix-publish-service-type
                                               (guix-publish-configuration (advertise?             #t)
                                                                           (compression            '(("lzip" 7) ("gzip" 9)))
                                                                           (cache                  "/var/cache/guix/publish")
                                                                           (cache-bypass-threshold (* 100 1024 1024))
                                                                           (ttl                    (* 3 24 60 60))))))


;; https ://guix.gnu.org/manual/en/html_node/Scheduled-Job-Execution.html
(define %lotus-mcron-services (list (service mcron-service-type
                                             (mcron-configuration (jobs (list garbage-collector-job
                                                                              ;; idutils-job
                                                                              updatedb-job))))))


(define %lotus-bitlbee-configuration (bitlbee-configuration
                                      (plugins (if %lotus-bitlbee-service-use-default? '() (list skype4pidgin)))
                                      (bitlbee (if %lotus-bitlbee-service-use-default? bitlbee bitlbee-purple))))

(define %lotus-bitlbee-services (list (service bitlbee-service-type
                                               %lotus-bitlbee-configuration)))


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
                                                                      (local-service? #t)))))

;; https://guix.gnu.org/manual/en/html_node/Networking-Services.html
(define %lotus-network-manager-services (list (service network-manager-service-type
                                                       (network-manager-configuration (dns %lotus-network-manager-dns)))))

(define %lotus-avahi-services (list (service avahi-service-type)))

(define %lotus-gpm-services  (list (service gpm-service-type)))

;; https://unix.stackexchange.com/questions/617858/how-to-enable-bluetooth-in-guix
(define %lotus-bluez-services (list (bluetooth-service #:auto-enable? #t)))


(define %lotus-audio-services (list (service mpd-service-type
                                             (mpd-configuration
                                              (user %lotus-account-user-name)
                                              (music-dir "~/Music")))))


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


;; https://github.com/alezost/guix-config/blob/master/system-config/os-main.scm
;; (define %lotus-polkit-services (list (service polkit-service-type)))
(define %lotus-polkit-services (list))


(define %lotus-krb5-services (if %lotus-default-realm
                                 (list (service krb5-service-type
                                                (krb5-configuration
                                                 (default-realm (cadr (assoc ':default %lotus-default-realm)))
                                                 (allow-weak-crypto? #t)
                                                 (realms (cdr (assoc ':realms %lotus-default-realm))))))
                                 (list )))


;; services modifications


(define %lotus-desktop-general-services %desktop-services)

(set! %lotus-desktop-general-services (modify-services
                                          %lotus-desktop-general-services
                                        (network-manager-service-type config =>
                                                                      (network-manager-configuration (inherit config)
                                                                                                     ;; (vpn-plugins '("network-manager-openconnect"))
                                                                                                     (dns "dnsmasq")))))

(set! %lotus-desktop-general-services (modify-services
                                          %lotus-desktop-general-services
                                        ;; https://gitlab.com/Efraim/guix-config/blob/master/macbook41_config.scm
                                        (guix-service-type config =>
                                                           (guix-configuration (inherit config)
                                                                               ;; (use-substitutes? %lotus-guix-use-substitutes)
                                                                               ;; (authorized-keys '())
                                                                               ;; (tmpdir "/gnu/tmp") ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
                                                                               (tmpdir (string-append "/srv/guix/build/" %local-disk-hitachi-serial-id "/tmp")) ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
                                                                               (substitute-urls (append %lotus-guix-substitute-urls
                                                                                                        %default-substitute-urls))
                                                                               (extra-options %lotus-guix-extra-options)))))

;; https://issues.guix.info/issue/35674
(when #t
  (set! %lotus-desktop-general-services (modify-services
                                            %lotus-desktop-general-services
                                          (gdm-service-type config =>
                                                            (gdm-configuration (inherit config)
                                                                               (xorg-configuration
                                                                                (xorg-configuration
                                                                                 (keyboard-layout %lotus-keyboard-layout)))
                                                                               (allow-empty-passwords? %lotus-gdm-allow-empty-password)
                                                                               (auto-login?            %lotus-gdm-auto-login)
                                                                               (default-user           %lotus-account-user-name))))))
(when #f
  ;; https://www.mail-archive.com/search?l=help-guix@gnu.org&q=subject:%22Re%5C%3A+Guix+Bluetooth+Headset%22&o=newest&f=1
  (set! %lotus-desktop-general-services (modify-services
                                            %lotus-desktop-general-services
                                          (pulseaudio-service-type config =>
                                                                   (pulseaudio-configuration
                                                                    (inherit config)
                                                                    (script-file (local-file "/etc/guix/default.pa")))))))

;; services add


(define %lotus-desktop-services %lotus-desktop-general-services)


(define %lotus-network-services  (list (service openssh-service-type)
                                       (service tor-service-type)))

(define %lotus-heavy-wm-services (list (service gnome-desktop-service-type)
                                       (service xfce-desktop-service-type)
                                       (service mate-desktop-service-type)
                                       (service enlightenment-desktop-service-type)))

(define %lotus-many-services (append %lotus-network-services
                                     %lotus-heavy-wm-services))

(define %lotus-few-services  (append (list (service gnome-desktop-service-type))
				                             %lotus-network-services))


(define %lotus-simple-services %lotus-few-services)

(define %lotus-simple-and-desktop-services (append %lotus-simple-services
                                                   %lotus-mail-aliases-services
                                                   %lotus-dovecot-services
                                                   %lotus-gpm-services
                                                   %lotus-bluez-services
                                                   %lotus-audio-services
                                                   %lotus-publish-services
                                                   %lotus-mcron-services
                                                   %lotus-cups-services
                                                   %lotus-polkit-services
                                                   %lotus-krb5-services
                                                   %lotus-bitlbee-services
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


(define %lotus-firmware (if nongnu-desktop?
                            (list linux-firmware)
                            %base-firmware))


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


(define %lotus-bootloader %lotus-efi-bootloader)
(define %lotus-initrd     (lotus-local-value %lotus-nongnu-metal-initrd %lotus-metal-initrd))


;; (define %lotus-setuid-programs %setuid-programs)

(define %lotus-setuid-programs (cons* #~(string-append #$ecryptfs-utils "/sbin/mount.ecryptfs_private")
                                      %setuid-programs))


(define %lotus-pinned-linux (let* ((channels (list (channel (name 'nonguix)
                                                            (url "https://gitlab.com/nonguix/nonguix")
                                                            (commit "6ee35a1f1cd50ba0d4bfee37f2b084bc0797885c"))
                                                   ;; (channel (name 'lotus)
                                                   ;;          (url "https://github.com/sharad/guix")
                                                   ;;          (commit "2079c605eff01bb5487d836ab2cbd30c68e42ecd"))
                                                   (channel (name 'guix)
                                                            (url "https://git.savannah.gnu.org/git/guix.git")
                                                            (commit "d9f1752c55a458d80c48192150f68ef37738a91f"))))
                                   (inferior (inferior-for-channels channels)))
                              (first (lookup-inferior-packages inferior "linux" "5.11.2"))))

(define %lotus-kernel (if nongnu-desktop?
                          linux
                          ;; %lotus-pinned-linux
                          ;; https://gitlab.com/nonguix/nonguix/-/blob/master/README.org Avoiding kernel recompilation
                          ;; guix system image --image-size=7.2GiB /path/to/this/channel/nongnu/system/install.scm
                          ;; https://guix.gnu.org/manual/en/html_node/Building-the-Installation-Image.html
                          ;; guix system image -t iso9660 gnu/system/install.scm
                          linux-libre))


(operating-system
 (kernel              %lotus-kernel)
 (kernel-arguments    '(" modprobe.blacklist=usbmouse,usbkbd --verbose nosplash debug"))
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
 (groups              %lotus-groups)
 (file-systems        %lotus-file-systems)
 (swap-devices        %lotus-swap-devices)
 (bootloader          %lotus-bootloader)
 (packages            %lotus-packages)
 (services            %lotus-services)
 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))


;; TO SEE host-file
;; https://guix.gnu.org/manual/en/html_node/Networking-Services.html

