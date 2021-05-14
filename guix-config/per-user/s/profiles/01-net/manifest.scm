
(define %lotus-net-rdp-package-names (list "rdesktop"
                                           "mit-krb5"))

(define %lotus-net--messagining-package-names (list "pidgin"
                                                    "skype4pidgin"
                                                    "pidgin-otr"
                                                    "geeqie"
                                                    "blueman"
                                                    "telegram-purple"
                                                    "telegram-purple"))

(define %lotus-net-package-names (append %lotus-net-rdp-package-names
                                         %lotus-net--messagining-package-names))

(define %lotus-net-packages (append (map specification->package
                                         %lotus-net-package-names)))

(packages->manifest %lotus-net-packages)




