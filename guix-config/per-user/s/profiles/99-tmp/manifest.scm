
(define %lotus-tmp-package-names (list "hello"))



(define %lotus-tmp-packages (append (map specification->package
                                         %lotus-tmp-package-names)))

(packages->manifest %lotus-tmp-packages)

