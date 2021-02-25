
(define %local-use-guix-official-mirror #t)


(define %backup-default-channels (list (channel (name 'guix)
                                                ;; (name 'guix-github)
                                                (branch "master")
                                                (url "https://github.com/guix-mirror/guix.git"))))

;; Default list of channels.
(define %guix-official-channels %default-channels)

(define %local-default-channels
  (if %local-use-guix-official-mirror
      %guix-official-channels
      %backup-default-channels))




(define %guix-lotus-channels (list (channel (name 'lotus)
                                            (url "https://github.com/sharad/guix"))))
                                    
(define %nonguix-channels (list (channel (name 'nonguix)
                                         (url "https://gitlab.com/nonguix/nonguix")
                                         ;; Enable signature verification:
                                         (introduction
                                          (make-channel-introduction
                                           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                                           (openpgp-fingerprint
                                            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

(define %wigust-channels (list (channel (name 'wigust)
                                        (url "https://notabug.org/wigust/guix-wigust.git"))))

(define %guix-more-channels (list (channel (name 'guix-more)
                                           (url "https://framagit.org/tyreunom/guix-more.git"))))


(define %local-channels (append ;; %guix-more-channels
                                %nonguix-channels
                                %guix-lotus-channels
                                %local-default-channels))


%local-channels

