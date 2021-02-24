
(define %lotus-net-package-names (list "rdesktop"
                                       "mit-krb5"))

(define %lotus-net-packages (append (map specification->package
                                         %lotus-net-package-names)))

(packages->manifest %lotus-net-packages)




