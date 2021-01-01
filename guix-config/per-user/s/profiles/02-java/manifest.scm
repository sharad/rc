
;; https://www.kuon.ch/post/2020-01-12-android-app/
;; https://lepiller.eu/en/running-android-studio-on-guix.html

;; https://archlinux.org/packages/community/x86_64/android-tools/
;; https://github.com/archlinux/svntogit-community/tree/packages/android-tools/trunk

;; https://archlinux.org/packages/community/any/gradle/
;; https://github.com/archlinux/svntogit-community/tree/packages/gradle/trunk

;; (use-package-modules scheme java base)

(define %lotus-java-package-names
  (list ;; "maven"
        ;; "maven-compact"
        "java-picocli"))

(define %lotus-java-misc-packages (list "icedtea"
                                        "icedtea:jdk"))

(define %lotus-java-packages
 (append (map specification->package
              %lotus-java-package-names)))

(concatenate-manifests (list (packages->manifest %lotus-java-packages)
                             (specifications->manifest %lotus-java-misc-packages)))

