
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix


(define %lotus-ocaml-heavy-package-names (list "ocaml"
                                               "ocamlbuild"
                                               "ocaml-findlib"
                                               "opam"
                                               "emacs-tuareg"))

(define %lotus-ocaml-4.07-heavy-package-names (list "ocaml@4.07.1"
                                                    "ocaml4.07-merlin"
                                                    "ocaml4.07-utop"
                                                    "ocaml4.07-core"
                                                    "ocaml4.07-findlib"
                                                    "ocaml4.07-ppxlib"))

(define %lotus-vm-heavy-package-names (list "virt-manager"
                                            "virt-viewer"
                                            ;; "qemu"
                                            "libvirt"))

(define %lotus-font-heavy-package-names (list "font-cns11643"))
;; "font-google-noto"
;; "font-adobe-source-han-sans"))

(define %lotus-misc-heavy-package-names (list "ungoogled-chromium"
                                              ;; "wine64"
                                              "wine"))

(define %lotus-heavy-package-names (append %lotus-ocaml-heavy-package-names
                                           %lotus-vm-heavy-package-names
                                           %lotus-font-heavy-package-names
                                           %lotus-misc-heavy-package-names))

(define %lotus-heavy-packages
  (append
   (map specification->package
        %lotus-heavy-package-names)))

(packages->manifest %lotus-heavy-packages)
