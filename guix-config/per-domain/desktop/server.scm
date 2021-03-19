
;; 10.8.29 Guix Services
;; https://guix.gnu.org/manual/en/html_node/Guix-Services.html#Guix-Data-Service

;; Continuous Integration
;; Cuirass
;; https://guix.gnu.org/manual/en/html_node/Continuous-Integration.html
(define %cuirass-specs
  #~(list
     '((#:name . "my-manifest")
       (#:load-path-inputs . ("guix"))
       (#:package-path-inputs . ("custom-packages"))
       (#:proc-input . "guix")
       (#:proc-file . "build-aux/cuirass/gnu-system.scm")
       (#:proc . cuirass-jobs)
       (#:proc-args . ((subset . "manifests")
                       (systems . ("x86_64-linux"))
                       (manifests . (("config" . "guix/manifest.scm")))))
       (#:inputs . (((#:name . "guix")
                     (#:url . "git://git.savannah.gnu.org/guix.git")
                     (#:load-path . ".")
                     (#:branch . "master")
                     (#:no-compile? . #t))
                    ((#:name . "config")
                     (#:url . "https://git.example.org/config.git")
                     (#:load-path . ".")
                     (#:branch . "master")
                     (#:no-compile? . #t))
                    ((#:name . "custom-packages")
                     (#:url . "https://git.example.org/custom-packages.git")
                     (#:load-path . ".")
                     (#:branch . "master")
                     (#:no-compile? . #t)))))))

(service cuirass-service-type
         (cuirass-configuration
          (specifications %cuirass-specs)))

;; Guix Data Service
;; https://guix.gnu.org/manual/en/html_node/Guix-Services.html#Guix-Data-Service
(define %guix-data-specs
  (list
   (package 'guix-data-service)         ;The Guix Data Service package to use.
   (user    "guix-data-service")        ;The system user to run the service as.
   (group   "guix-data-service")        ;The system group to run the service as.
   (port    8765)                       ;The port to bind the web service to.
   (host    "127.0.0.1")                ;The host to bind the web service to.
   (getmail-idle-mailboxes #f)          ;If set, this is the list of mailboxes that the getmail service will be configured to listen to.
   (commits-getmail-retriever-configuration #f) ;If set, this is the getmail-retriever-configuration object with which to configure getmail to fetch mail from the guix-commits mailing list.
   (extra-options '())                          ;Extra command line options for guix-data-service.
   (extra-process-jobs-options '())))           ;Extra command line options for guix-data-service-process-jobs.

(service guix-data-service-type
         (guix-data-service-configuration
          (specifications %guix-data-specs)))

;; Guix Build Coordinator
(define %guix-build-coordinator-specs
  (list
   (package 'guix-build-coordinator)    ;The Guix Build Coordinator package to use.
   (user    "guix-build-coordinator")   ;the system user to run the service as.
   (group "guix-build-coordinator")     ;The system group to run the service as.
   (database-uri-string "sqlite:///var/lib/guix-build-coordinator/guix_build_coordinator.db") ;The URI to use for the database.
   (agent-communication-uri "http://0.0.0.0:8745")         ;The URI describing how to listen to requests from agent processes.
   (client-communication-uri "http://127.0.0.1:8746")      ;The URI describing how to listen to requests from clients. The client API allows submitting builds and currently isn’t authenticated, so take care when configuring this value.))
   (allocation-strategy #~basic-build-allocation-strategy) ;A G-expression for the allocation strategy to be used. This is a procedure that takes the datastore as an argument and populates the allocation plan in the database.))
   (hooks ’())                                             ;An association list of hooks. These provide a way to execute arbitrary code upon certain events, like a build result being processed.))
   (guile guile-3.0-latest))) ;The Guile package with which to run the Guix Build Coordinator.

(service guix-build-coordinator-service-type
         (guix-build-coordinator-configuration
          (specifications %guix-build-coordinator-specs)))

