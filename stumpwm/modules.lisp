
(in-package :stumpwm)

(require :cl-fad)


;; FOR GUIX
(defvar *contrib-dirs* nil)

(defun local-set-contrib-dir ()
  (let* ((contrib-dirs '(#p"~/.stumpwm.d/contrib/"
                         #p"~/.stumpwm/contrib/"
                         #p"/usr/local/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib/"))
         (contrib-dirs (member-if #'probe-file contrib-dirs)))
    (dolist (dir contrib-dirs)
      (when dir
        (unless (member dir *contrib-dirs*)
          (push dir *contrib-dirs*)
          (message "adding ~a" dir)
          (set-module-dir dir)
          (setf *load-path* nil)
          (dolist (mdir asdf:*central-registry*)
            (add-to-load-path mdir)))))))
;; FOR GUIX


;; #-quicklisp
(local-set-contrib-dir)

(defun load-external-module (module)
  #+quicklisp
  (if (ql:where-is-system module)
      (ql:quickload module)
      (message "failed to load ~a" module))
  #-quicklisp
  (stumpwm:load-module module))


;;{{{ Load module
#+cl-fad
(progn
  (defun list-directory-resursively (dir &key (predicate t))
    (flatten
     (mapcar
      #'(lambda (e)
          (append (when (or (eq predicate t)
                            (funcall predicate e))
                    (list e))
                  (list-directory-resursively e :predicate predicate)))
      (when (cl-fad:directory-pathname-p dir)
        (cl-fad:list-directory dir)))))

  (defun stumpwm-contrib-modules (dirs)
    (let ((modules-sets (mapcar #'(lambda (dir)
                                    (mapcar #'(lambda (asd-path)
                                                (car (last (pathname-directory asd-path))))
                                            (list-directory-resursively dir
                                                                        :predicate #'(lambda (path)
                                                                                       (string-equal (pathname-type path) "asd")))))
                                dirs)))
      (reverse (apply #'append modules-sets))))

  (defvar *stumpwm-contrib-exclude-modules* '("notify"
                                              "qubes"
                                              "lookup"))
  (defvar *stumpwm-contrib-include-modules-in-end* '("notify"))
  (setf *stumpwm-contrib-include-modules-in-end* '())

  (defun stumpwm-contrib-included-modules (&rest dirs)
    (let ((dirs (or dirs *contrib-dirs*)))
      (set-difference (stumpwm-contrib-modules dirs)
                      *stumpwm-contrib-exclude-modules*
                      :test #'string-equal)))

  (defun load-all-modules ()
    (dolist (mod (append (reverse (stumpwm-contrib-included-modules))
                         *stumpwm-contrib-include-modules-in-end*))
      (stumpwm::message "loading ~a" mod)
      ;; (sleep 1)
      (if nil
          (stumpwm::load-external-module mod)
          (ignore-errors
           (stumpwm::load-external-module mod)))))

  (when t
    (message "loading all modules now")
    (load-all-modules))

  (defcommand load-all-external-modules () ()
    (load-all-modules)))


;; enable
#+stumptray
(when (fboundp 'stumptray:stumptray)
  (stumptray:stumptray))

#+clipboard-history
(progn
  (define-key *root-map* (kbd "C-y") "show-clipboard-history")
  ;; start the polling timer process
  (clipboard-history:start-clipboard-manager))


#+wifi
(setf wifi:*iwconfig-path*
      (let ((default-path (or wifi:*iwconfig-path*
                              "/sbin/iwconfig"))
            (found-path   (some #'probe-file
                                (list wifi:*iwconfig-path*
                                      "/sbin/iwconfig"
                                      "/run/current-system/profile/sbin/iwconfig"))))
        (if found-path (namestring found-path) default)))

#+net
(progn
  (defun net::net-device ())
  "Returns statically assigned device name or tries to find it be default gw.
For the second case rescans route table every minute."
  (let ((ip-cmd (let ((default-path "/sbin/ip")
                      (found-path   (some #'probe-file
                                          '("/sbin/ip"
                                            "/run/current-system/profile/sbin/ip"))))
                  (if found-path (namestring found-path) default))))
   (if net::*net-device*
      net::*net-device*
      (if (and net::*last-route-device*
               (< (- (net::now) net::*last-route-rescan-time*) 60))
          net::*last-route-device*
          (let ((new-device (or (net::find-default) "lo")))
            (when (string/= new-device net::*last-route-device*)
              (setf net::*net-ipv4*
                    (string-trim '(#\Newline)
                     (run-shell-command
                      (format nil
                              "~A -o -4 addr list ~A | awk '{print $4}' | cut -d/ -f1" ip-cmd new-device))
                     t))
              (setf net::*net-ipv6*
                    (string-trim '(#\Newline)
                     (run-shell-command
                      (format nil
                              "~A -o -6 addr list ~A | awk '{print $4}' | cut -d/ -f1" ip-cmd new-device))
                     t))
              (setq net::*net-last-tx* 0
                    net::*net-last-rx* 0
                    net::*net-last-time* nil
                    net::*net-rx* nil
                    net::*net-tx* nil
                    net::*net-time* nil))
            (setq net::*last-route-rescan-time* (net::now)
                  net::*last-route-device* new-device)))))
  (setf net::*net-modeline-fmt* ;; "%d: %u"
        "%d: %i"))

;;}}}
