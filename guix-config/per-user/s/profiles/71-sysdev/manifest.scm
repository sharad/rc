
(define %lotus-sysdev-package-names
  (list "gparted" ;; required for situations
        "parted"
        "gptdisk"
        "efibootmgr"
        "grub-efi"))

(define %lotus-sysdev-packages
  (append
   ;; (list '(gcc "lib"))
   (map specification->package
        %lotus-sysdev-package-names)))

(packages->manifest %lotus-sysdev-packages)
