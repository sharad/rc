
(define %lotus-failed-package-names (list "nomad"
                                          "openfortivpn"
                                          "wmutils-core"
                                          "wmutils-opt"
                                          "hello"))



(define %lotus-failed-packages (append (map specification->package
                                            %lotus-failed-package-names)))

(packages->manifest %lotus-failed-packages)

