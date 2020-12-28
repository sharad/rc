


;; https://www.kuon.ch/post/2020-01-12-android-app/
;; https://lepiller.eu/en/running-android-studio-on-guix.html

;; https://archlinux.org/packages/community/x86_64/android-tools/
;; https://github.com/archlinux/svntogit-community/tree/packages/android-tools/trunk

;; https://archlinux.org/packages/community/any/gradle/
;; https://github.com/archlinux/svntogit-community/tree/packages/gradle/trunk

(define %lotus-java-package-names
  (list "icedtea" ;; "icedtea:out"
        ;; java
        "icedtea:jdk"
        ;; "maven"
        "java-picocli"
        "maven-compact"))

(define %lotus-java-packages
  (append
   ;; (list '(gcc "lib"))
   (map specification->package
        %lotus-java-package-names)))

(packages->manifest %lotus-java-packages)
