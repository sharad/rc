

(in-package :stumpwm)

(require :cl-fad)

;; FOR GUIX
(defvar *contrib-dirs* nil)

(defun local-set-contrib-dir ()
  (let* ((contrib-dirs '(;; "/usr/local/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib/"
                         "/home/s/hell/.stumpwm.d/contrib/"))
         (contrib-dirs  (member-if #'probe-file contrib-dirs)))
    (dolist (dir contrib-dirs)
      (when dir
        (unless (member dir *contrib-dirs*)
          (push dir *contrib-dirs*))
        (message "adding ~a" dir)
        (set-module-dir dir)
        (dolist (mdir asdf:*central-registry*)
          (add-to-load-path mdir))))))
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

;; (defcommand load-external-module (name) ((:module "Load Module: "))
;;   ())

;; (add-to-load-path #p"~/.stumpwm.d/modules")

;;{{{ Load module
#+cl-fad
(progn
  (defun list-directory-resursively (dir &key (predicate t))
    (flatten
     (mapcar
      #'(lambda (e)
          (append
           (when (or
                  (eq predicate t)
                  (funcall predicate e))
             (list e))
           (list-directory-resursively e :predicate predicate)))
      (when (cl-fad:directory-pathname-p dir)
        (cl-fad:list-directory dir)))))

  (defun stumpwm-contrib-modules (dirs)
    (reverse
     (apply #'append
            (mapcar #'(lambda (dir)
                        (mapcar #'(lambda (asd-path)
                                    (car (last (pathname-directory asd-path))))
                                (list-directory-resursively dir
                                                            :predicate #'(lambda (path)
                                                                           (string-equal (pathname-type path) "asd")))))
                    dirs))))

  (defvar *stumpwm-contrib-exclude-modules* '(
                                              "notify"
                                              "qubes"))
                                              ;; "swm-gaps"
                                              ;; "pinentry"
                                              ;; "pass"
                                              ;; "end-session"
                                              ;; "desktop-entry"
                                              ;; "command-history"
                                              ;; "clipboard-history"
                                              ;; "notifications"


  (defvar *stumpwm-contrib-exclude-modules* '("notify" "qubes"))

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


  (defun stumpwm-contrib-new-modules ()
   (let ((modlist '("remember-win"
                    ;; media
                    "amixer"
                    "aumix"

                    ;; minor-mode
                    "mpd"
                    "notification"

                    ;; modelines
                    "battery"
                    "battery-portable"
                    "cpu"
                    "disk"
                    "hostname"
                    "maildir"
                    "mem"
                    "net"
                    "wifi"

                    ;; util
                    "alert-me"
                    "app-menu"
                    "debian"
                    "globalwindows"
                    "kbd-layouts"
                    "logitech-g15-keysyms"
                    ;; "notify"
                    "numpad-layouts"
                    "passwd"
                    "perwindowlayout"
                    "productivity"
                    ;; "qubes"
                    #+sbcl
                    "sbclfix"
                    "screenshot"
                    "searchengines"
                    "stumpish"
                    "stumptray"
                    "surfraw"
                    "swm-emacs"
                    "ttf-fonts"
                    "undocumented"
                    "urgentwindows"
                    "windowtags"
                    "winner-mode"
                    ;; "stumpwm.contrib.dbus"
                    "notify")))
    (set-difference
     (stumpwm-contrib-included-modules *contrib-dirs*)
     modlist :test #'string-equal)))

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

;; (load-external-module "wmii-like-stumpwmrc")

;;}}}
