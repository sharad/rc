

(define emacs
  (make <service>
    #:provides '(emacs)
    #:requires '()
    #:start (make-system-constructor "emacs --daemon")
    #:stop (make-system-destructor "emacsclient --eval \"(kill-emacs)\"")))

(define redshift
  (make <service>
    #:provides '(redshift)
    #:start    (make-forkexec-constructor '("redshift" "-t" "6500:5000" "-b" "1.0 0.8")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

