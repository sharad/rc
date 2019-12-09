
(use-modules (shepherd service))
(use-modules (guix utils))
(use-modules (gnu))
;; (use-service-modules mcron)
(use-modules (gnu services mcron))

(load "services.scm")

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(register-services emacs
                   ;; gpg-agent
                   ;; ibus-daemon
                   ;; jackd
                   mcron-task
                   redshift)

;; Send shepherd into the background
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start (list redshift mcron-task)) ; services to start automatically


