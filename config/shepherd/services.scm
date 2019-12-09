

(define emacs
  (make <service>
    #:provides '(emacs)
    #:requires '()
    #:start (make-system-constructor "emacs --daemon")
    #:stop (make-system-destructor "emacsclient --eval \"(kill-emacs)\"")))

(define redshift
  (make <service>
    #:provides '(redshift)
    #:start    (make-forkexec-constructor '("xterm")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define offline-syncimap
  ;; offline-syncimap 5 minutes after midnight every day.
  ;; The job's action is a shell command.
  ;; Vixie cron syntax
  #~(job "*/3 * * * *" "~/bin/syncimap "))

(define mcron-task (service mcron-service-type
                            (mcron-configuration
                             (jobs (list offline-syncimap)))))


