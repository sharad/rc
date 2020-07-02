

(use-modules (gnu packages glib))


(define %lotus-disputed-packages (list ;; "sbcl"
                                       ;; "cl-fad"
                                       ;; "cl-slime-swank"
				       ))

(define %lotus-other-packages (list "polkit"
                                    "polkit-gnome"
                                    "autorandr"
                                    ;; "ppp"
				    "xmodmap"
				    "nm-dnsmasq-ns"
                                    ;; "dconf"
                                    "glib-networking"

                                    "xf86-input-evdev"))

(define %lotus-system-selected-package-names (list "glibc-utf8-locales"
                                                   "gdm"
                                                   "gpm"
                                                   "slock" ; need suid
                                                   "zsh"
                                                   "stumpwm"
                                                   "guile-wm"
                                                   "windowmaker"
                                                   ;; "findutils"
                                                   "idutils"
                                                   ;; "twm"
                                                   ;; "herbstluftwm"
                                                   "ecryptfs-utils"))

;; (define %lotus-font-packages (list "gs-fonts"
;;                                    "font-gnu-freefont-ttf"
;;                                    "font-adobe-source-code-pro"
;;                                    "font-terminus"
;;                                    "font-dejavu"
;;                                    "font-hack"
;;                                    "font-awesome"
;;                                    "font-arabic-misc"
;;                                    "font-lohit"))

(define %lotus-mail-packages (list))
(define %lotus-font-packages (list))
(define %lotus-media-packages (list))
(define %lotus-gui-packages (list))
(define %lotus-text-packages (list))
(define %lotus-notification-packages (list))


(define %lotus-package-names-for-installation
  (append %lotus-disputed-packages
          %lotus-other-packages
          %lotus-system-selected-package-names
          %lotus-mail-packages
          %lotus-font-packages
          %lotus-media-packages
          %lotus-gui-packages
          %lotus-text-packages
          %lotus-notification-packages))

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

(define %lotus-reinit-packages (append %lotus-system-packages
                                %base-packages))


(define this-package-file
  (local-file (assoc-ref (current-source-location) 'filename)))

;; (define %lotus-copy-current-package-file-in-etc (list
;;                                                  ;; https://willschenk.com/articles/2019/installing_guix_on_nuc/
;;                                                  ;; Copy current config to /etc/config.scm
;;                                                  (simple-service 'package-file etc-service-type
;;                                                                  `(("config.scm" ,this-package-file)))))

