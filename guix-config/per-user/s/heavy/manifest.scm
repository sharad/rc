
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base gcc llvm base python version-control less ccache pkg-config glib gnome cmake messaging autotools flex bison compression m4 gawk xorg onc-rpc gsasl kerberos image commencement fontutils shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dev-cdesktop-package-names (list "ocaml@4.07.1"
                                                ;; "ocaml"
                                                ;; "ocaml-merlin"
                                                "ocaml4.07-merlin"
                                                "ocaml4.07-utop"
                                                "ocaml4.07-core"
                                                ;; "ocaml-findlib"
                                                "ocaml4.07-findlib"
                                                "ocaml4.07-ppxlib"
                                                "opam"
                                                "emacs-tuareg"
                                                ;; qemu
                                                "virt-manager"
                                                "virt-viewer"
                                                "libvirt"

                                                ;; "font-google-noto"
                                                ;; "font-adobe-source-han-sans"
                                                "font-cns11643"))




(define %lotus-dev-cdesktop-packages
  (append
   (map specification->package
        %lotus-dev-cdesktop-package-names)))

(packages->manifest %lotus-dev-cdesktop-packages)
