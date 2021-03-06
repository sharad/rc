

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
  (let ((conkyrc (string-append (getenv "HOME") "/.conkyrc/main/conkyrc"))) ; to make it more obvious
    (make <service>
      #:provides '(conky)
      ;; #:start    (make-forkexec-constructor (list "conky" "-d" "-c" conkyrc)) ; to make it more obvious
      #:start    (make-forkexec-constructor (list "conky" "-c" conkyrc)) ; to make it more obvious
      #:stop     (make-kill-destructor)
      #:respawn? #t)))

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
    #:start    (make-forkexec-constructor '("autocutsel")) ; to make it more obvious
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

(define dunst
  (make <service>
    #:provides '(dunst)
    #:start    (make-forkexec-constructor '("dunst")) ; to make it more obvious
    #:stop     (make-kill-destructor)
    #:respawn? #t))

(define notification-daemon
  (let ((cmd (string-append (getenv "HOME") "/.guix-profile/libexec/notification-daemon")))
    (make <service>
      #:provides '(notification-daemon)
      #:start    (make-forkexec-constructor (list cmd)) ; to make it more obvious
      #:stop     (make-kill-destructor)
      #:respawn? #t)))


;; https://guix.gnu.org/manual/en/html_node/Scheduled-Job-Execution.html
