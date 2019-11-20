;; -*-lisp-*-

(in-package :stumpwm)


;; Load swank.


;; SLIME

;; Add this code to your .stumpwmrc for interactive control of stumpwm via slime:

;; Load swank.
;; C-z ; swank will kick this off

;; (load "/usr/share/common-lisp/source/slime/swank-loader.lisp")
;; (load "/usr/share/emacs/site-lisp/slime/swank-loader.lisp")


;; FOR GUIX
(when nil
 (load "/usr/share/common-lisp/source/slime/swank-loader.lisp"))


;; (let ((swank-loader (make-list-from-emacs-eval "(car (list swank-loader-full-path))")))
;;   (load swank-loader))

;; (load "/all/res/share/common-lisp/quicklisp/dists/quicklisp/software/slime-20120307-cvs/swank-loader.lisp")

;; (if (functionp 'swank-loader::init)
;;     (swank-loader::init))

;; (swank-loader::init)
;; from http://lists.common-lisp.net/pipermail/slime-devel/2008-August/015346.html
#+swank-loader
(swank-loader::init :setup nil)


;; (if (and
;;      (package-name :swank-loader)
;;      (find-symbol "init" :swank-loader))
;;     (if (functionp 'swank-loader::init)
;;         (swank-loader::init)))


#+swank-loader
(defcommand swank () ()
  (setf stumpwm:*top-level-error-action* :break)
  (if (swank:create-server :port 4005
                           :style swank:*communication-style*
                           :dont-close t)
      (when nil
        (emacs-eval "(slime-disconnect)")
        ;; (emacs-eval "(slime-connect \"localhost\" 4005)")
        ;; (emacs-eval "(let ((slime-protocol-version 'ignore)) (slime-connect \"localhost\" 4005))")
        (emacs-eval "(slime-connect-ignore-version \"localhost\" 4005)")
        (echo-string (current-screen)
                     (concat
                      "Emacs already connected to this swank server.\n"
                      "No need for starting swank. M-x slime-connect "
                      "RET RET, then (in-package stumpwm).")))))

;; if 4005 not in use then only call
;; (swank)                                 ;start swank

;; Controlling emacs through stumpwm commands
;; This is an example of how you can use stumpwm commands to call emacs functions.

(defcommand wanderlust () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "M-x"))
  (window-send-string "wl")
  (send-meta-key (current-screen) (kbd "RET")))

