
(define %lotus-failed-package-names (list "python-rofi-tmux"
                                          "ghc"
                                          "hello"))



(define %lotus-failed-packages (append (map specification->package
                                            %lotus-failed-package-names)))

(packages->manifest %lotus-failed-packages)

