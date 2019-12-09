
(load "services.scm")

(register-services emacs
                   gpg-agent
                   ibus-daemon
                   jackd)
(action 'shepherd 'daemonize) ; send shepherd into background
(for-each start (list emacs)) ; services to start automatically
 
