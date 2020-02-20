
(define %local-use-guix-official-mirror #f)


(define %backup-default-channels (list (channel
                                        ;; (name 'guix-github)
                                        (name 'guix)
                                        ;; (branch "master")
					(commit "d498aa8862f49c92e9d5af4e33ef075e8276c99f")
                                        ;; (commit "c829faacefa2d80ff3229e95b7bb93d777ce3a5e")
                                        ;; (url "https://github.com/guix-mirror/guix.git")
                                        (url "https://git.savannah.gnu.org/git/guix.git")
					)))

;; Default list of channels.
(define %guix-official-channels %default-channels)

(define %local-default-channels
  (if %local-use-guix-official-mirror
      %guix-official-channels
      %backup-default-channels))


(define %guix-lotus-channels (list (channel
                                    (name 'lotus)
                                    (url "https://github.com/sharad/guix"))))

(define %nonguix-channels (list (channel
                                 (name 'nonguix)
				 (commit "35df08183ed141ffc8b869683eb017ff93c3f068")
                                 (url "https://gitlab.com/nonguix/nonguix"))))


(define %local-channels (append %nonguix-channels
                                %guix-lotus-channels
                                %local-default-channels))


%local-channels

