
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


(define %lotus-emacs-disputed-packages (list))
;; "notification-daemon" "trezord-udev-rules" "trezord"

(define %lotus-emacs-device-hardware-packages (list))

(define %lotus-emacs-other-packages (list "emacs-ag"
                                          "emacs-helm-ag"
                                          "emacs"
                                          "emacs-bbdb"
                                          "emacs-ebdb"
                                          "emacs-gnus-harvest"
                                          "emacs-geiser"
                                          "emacs-sesman"
                                          "guile-studio"
                                          "emacs-guix"
                                          "emacs-math-symbol-lists"
                                          "emacs-pretty-mode"
                                          "emacs-el-mock"
                                          "emacs-flyspell-correct"
                                          "emacs-powerline"
                                          "emacs-spaceline"
                                          "emacs-emojify"
                                          "emacs-org"
                                          "emacs-org-noter" ;; https://github.com/weirdNox/org-noter
                                          "emacs-closql"
                                          "emacs-diminish"
                                          ;; "emacs-pass"
                                          "emacs-direnv"
                                          "emacs-editorconfig"
                                          "emacs-develock"
                                          "emacs-el-x"
                                          "emacs-default-encrypt"
                                          "emacs-default-text-scale"
                                          "emacs-deft"
                                          "emacs-dashboard"
                                          "emacs-typo"
                                          ;; https://develop.spacemacs.org/layers/+lang/c-c++/README.html
                                          "emacs-lsp-ui"
                                          "emacs-lsp-ivy"
                                          "emacs-helm-lsp"
                                          "emacs-company-lsp"
                                          ;; "clangd"
                                          ;; "cquery"
                                          "ccls"
                                          "emacs-ccls"
                                          "emacs-slack"
                                          "emacs-pdf-tools"
                                          ;; "emacs-org-roam" ;; need roam internet service login
                                          "emacs-standard-dirs"
                                          "emacs-helm-switch-to-repl"
                                          "emacs-org-pretty-table"
                                          "emacs-chess"
                                          "emacs-company-emoji"
                                          "emacs-dash-docs"
                                          "emacs-lsp-ivy"
                                          "emacs-smart-hungry-delete"
                                          "emacs-webpaste"
                                          "emacs-vterm"
                                          "emacs-ediprolog"
                                          "emacs-hyperbole"
                                          "emacs-recutils"
                                          "emacs-ledger-mode"
                                          "emacs-agda2-mode"
                                          "emacs-pinentry"))

(define %lotus-emacs-lang-packages (list)) ;; "ocaml"
;; ;; "ocaml-merlin"
;; "opam"

(define %lotus-emacs-audio-packages (list))

(define %lotus-emacs-user-selected-package-names (list))

(define %lotus-emacs-network-packages (list))

(define %lotus-emacs-mail-packages (list))

(define %lotus-emacs-font-packages (list))

(define %lotus-emacs-media-packages (list))

(define %lotus-emacs-gui-packages (list))

(define %lotus-emacs-text-packages (list))

(define %lotus-emacs-notification-packages (list))

(define %lotus-emacs-test-packages (list))
;; "icecat"

(define %lotus-emacs-file-packages (list))

(define %lotus-emacs-misc-packages (list))


(define %lotus-emacs-package-names-for-installation (append %lotus-emacs-user-selected-package-names
                                                            %lotus-emacs-lang-packages
                                                            %lotus-emacs-audio-packages
                                                            %lotus-emacs-disputed-packages
                                                            %lotus-emacs-device-hardware-packages
                                                            %lotus-emacs-other-packages
                                                            %lotus-emacs-network-packages
                                                            %lotus-emacs-mail-packages
                                                            %lotus-emacs-font-packages
                                                            %lotus-emacs-media-packages
                                                            %lotus-emacs-gui-packages
                                                            %lotus-emacs-text-packages
                                                            %lotus-emacs-notification-packages
                                                            %lotus-emacs-test-packages
                                                            %lotus-emacs-file-packages
                                                            %lotus-emacs-misc-packages))

(define %lotus-emacs-user-desktop-packages (list))
        ;; lvm2
        ;; for user mounts
        ;; gvfs
        ;; (list bind "utils")

(define %lotus-emacs-user-selected-packages (map specification->package
                                                 %lotus-emacs-package-names-for-installation))

(define %lotus-emacs-user-packages (append %lotus-emacs-user-desktop-packages
                                           %lotus-emacs-user-selected-packages))

(define %lotus-emacs-packages (append %lotus-emacs-user-packages))

(packages->manifest %lotus-emacs-packages)

