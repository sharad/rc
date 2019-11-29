


(define %lotus-system-selected-package-names (list "glibc-utf8-locales"
                                                   "polkit"
                                                   "polkit-gnome"

                                                   "gpm"
                                                   "font-adobe-source-code-pro"
                                                   "font-terminus"
                                                   "font-dejavu"
                                                   "font-hack"
                                                   "font-awesome"
                                                   "font-arabic-misc"

                                                   "zsh"
                                                   "cl-fad"
                                                   "cl-slime-swank"
                                                   "stumpwm"
                                                   "guile-wm"
                                                   ;; "windowmaker"
                                                   ;; "twm"
                                                   ;; "herbstluftwm"

                                                   "ecryptfs-utils"
                                                   "dconf"))

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


(define this-package-file
  (local-file (basename (assoc-ref (current-source-location)
                                   'filename))
              "package.scm"))

(define %lotus-copy-current-package-file-in-etc (list
                                                 ;; https://willschenk.com/articles/2019/installing_guix_on_nuc/
                                                 ;; Copy current config to /etc/config.scm
                                                 (simple-service 'package-file etc-service-type
                                                                 `(("config.scm" ,this-package-file)))))

