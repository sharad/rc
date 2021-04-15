
(define %lotus-sysdev-package-names
  (list "gparted" ;; required for situations
        "parted"
        "gptfdisk"
        "efibootmgr"
        "grub-efi"))

(define %lotus-sysdev-packages
  (append (map specification->package
               %lotus-sysdev-package-names)))

(packages->manifest %lotus-sysdev-packages)
