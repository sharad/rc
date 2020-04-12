
(use-modules (shepherd service))
(use-modules (guix utils))
(use-modules (gnu))
;; (use-service-modules mcron)
(use-modules (gnu services mcron))


(define (in-x-window?)
  (not (eq? #f (getenv "DISPLAY"))))

(define (services-applicable)
  (if (in-x-window?)
      x-services
      tty-services))


(load "services.scm")

(define unregistered-services (list 
				;; gpg-agent
				;; ibus-daemon 
				;; jackd
				))
(define unused-services       (list emacs))
(define tty-services          (list pkttyagent))
(define x-services            (list redshift polkit-gnome-agent osdsh autocutsel keynav conky xautolock))


;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(apply register-services (append unused-services tty-services x-services)) 


;; Send shepherd into the background
(action 'shepherd 'daemonize)


;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start (services-applicable)) ; services to start automatically



