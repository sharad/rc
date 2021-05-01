
;; https://wingolog.org/archives/2015/08/04/developing-v8-with-guix
(use-package-modules base
                     gcc
                     llvm
                     base
                     python
                     version-control
                     less
                     ccache
                     pkg-config
                     glib
                     gnome
                     cmake
                     messaging
                     autotools
                     flex
                     bison
                     compression
                     m4
                     gawk
                     xorg
                     onc-rpc
                     gsasl
                     kerberos
                     image
                     commencement
                     fontutils
                     shells)

(use-modules (lotus packages cdesktopenv))

(define %lotus-dev-utits-names (list "glibc"
                                     "glibc-locales"
                                     "binutils"
                                     "pkg-config"
                                     "coreutils"
                                     "diffutils"
                                     "findutils"
                                     "tar"
                                     "patch"
                                     "sed"
                                     "grep"
                                     "gawk"
                                     "moreutils"
                                     "flex"
                                     "bison"
                                     "make"
                                     "autoconf"
                                     "automake"
                                     "libtool"
                                     "patchelf"
                                     "gcc-toolchain"
                                     "gdb"

                                     "dosfstools"

                                     "rpm"

                                     "picocom"

                                     "smalltalk"

                                     "go"

                                     "gprolog"
                                     ;; "swi-prolog"
                                     ;; "emacs-ediprolog"

                                     "node"


                                     "ghc"
                                     "idris"


                                     "rust"
                                     ;; "scala"
                                     "vala"
                                     ;; "kotlin"

                                     "r"
                                     "r-ggplot2"
                                     "r-cowplot"
                                     "erlang"
                                     "elixir"

                                     ;; "fribidi"
                                     ;; "bicon"

                                     "artanis"
                                     "doxygen"))


(define %lotus-dev-emacs-name (list "emacs-emacsql-sqlite3"
                                    "emacs-lsp-ui"
                                    "emacs-lsp-treemacs"
                                    "emacs-helm-lsp"
                                    "emacs-company-lsp"
                                    "emacs-eglot"
                                    "emacs-lsp-mode"
                                    "emacs-ccls"
                                    "emacs-eldev"
                                    "emacs-bluetooth"
                                    "ccls"))


(define %lotus-dev-tools-names (list "python-gitlab"
                                     "python-argcomplete"))

(define %lotus-dev-package-experimental-names (list "screen-message" ;; "rofi-master"
                                                    "zssh"
                                                    "lrzsz"
                                                    ;; gnome-pie
                                                    ;; python-ulauncher
                                                    "gnutls"
                                                    ;; "emacs-hyperbole"
                                                    "p11-kit"))

(define %lotus-dev-package-names (append %lotus-dev-utits-names
                                         %lotus-dev-emacs-name
                                         %lotus-dev-tools-names
                                         %lotus-dev-package-experimental-names))

(define %lotus-dev-packages (append (map specification->package
                                         %lotus-dev-package-names)))

(packages->manifest %lotus-dev-packages)
