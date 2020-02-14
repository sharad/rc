

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

(define polkit-gnome-agent
  (make <service>
    #:provides '(polkit-gnome-agent)
    #:start    (make-forkexec-constructor '("/run/current-system/profile/libexec/polkit-gnome-authentication-agent-1")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define pkttyagent
  ;; TODO: need terminal
  (make <service>
    #:provides '(pkttyagent)
    #:start    (make-forkexec-constructor '("pkttyagent")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #f))

(define conky
  (make <service>
    #:provides '(conky)
    #:start    (make-forkexec-constructor '("conky" "-d" "-c" (string-append (getenv "HOME") "/.conkyrc/main/conkyrc"))) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define keynav
  (make <service>
    #:provides '(keynav)
    #:start    (make-forkexec-constructor '("keynav")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define xautolock
  (make <service>
    #:provides '(xautolock)
    #:start    (make-forkexec-constructor '("xautolock" "-detectsleep" "-locker" "alarm")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define autocutsel
  (make <service>
    #:provides '(autocutsel)
    #:start    (make-forkexec-constructor '("autocutsel" "-f")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define compton
  (make <service>
    #:provides '(compton)
    #:start    (make-forkexec-constructor '("compton")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define osdsh
  (make <service>
    #:provides '(osdsh)
    #:start    (make-forkexec-constructor '("osdsh")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))
