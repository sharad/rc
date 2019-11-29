
(define %lotus-system-selected-package-names (list "zsh"
                                                   "ecryptfs-utils"))

(define %lotus-package-names-for-installation 
  (append %lotus-system-selected-package-names
          ;; %lotus-other-packages
          ;; %lotus-mail-packages
          ;; %lotus-font-packages
          ;; %lotus-media-packages
          ;; %lotus-gui-packages
          ;; %lotus-text-packages
          ;; %lotus-notification-packages
          ))

(define %lotus-system-desktop-packages
  (list lvm2
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
