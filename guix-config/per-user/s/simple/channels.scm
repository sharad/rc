
(define %local-use-guix-official-mirror #t)

(define %backup-default-channels
  ;; Default list of channels.
  (list (channel
         ;; (name 'guix-github)
         (name 'guix)
         (branch "master")
         (url "https://github.com/guix-mirror/guix.git"))))

(define %guix-official-channels %default-channels)

(define %local-default-channels
  (if %local-use-guix-official-mirror
      %guix-official-channels
      %backup-default-channels))

(define %local-channels
  (cons* (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix"))
         (channel
          (name 'lotus)
          (url "https://github.com/sharad/guix"))
         %local-default-channels))

%local-channels
