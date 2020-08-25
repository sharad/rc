
(define %lotus-dynamic-hash-package-names (list "conkeror-firefox"
                                                "p4"
                                                "lesspipe" 
                                                "emacs-develock"))

(define %lotus-dynamic-hash-packages
  (append
   (map specification->package
        %lotus-dynamic-hash-package-names)))

;; set LD_LIBRARY_PATH as some command built and used
;; (packages->manifest %lotus-dynamic-hash-packages)

(packages->manifest %lotus-dynamic-hash-packages)

