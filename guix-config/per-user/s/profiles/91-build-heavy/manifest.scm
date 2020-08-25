
(define %lotus-build-heavy-package-names
  (list ;; "ungoogled-chromium"
        "firefox"))

(define %lotus-build-heavy-packages
  (append (map specification->package
               %lotus-build-heavy-package-names)))

(packages->manifest %lotus-build-heavy-packages)
