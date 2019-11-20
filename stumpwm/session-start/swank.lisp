;; -*-lisp-*-

(in-package :stumpwm)


;; Load swank.


;; SLIME

;; Add this code to your .stumpwmrc for interactive control of stumpwm via slime:

;; Load swank.
;; C-z ; swank will kick this off

;; FOR GUIX
(defun load-swank-loader ()
  (let* ((loaders '(#p"/run/current-system/profile/share/common-lisp/source/cl-slime-swank/swank-loader.lisp" 
                    #p"~/.config/guix/current/share/common-lisp/source/cl-slime-swank/swank-loader.lisp"
                    #p"~/.guix-profile/share/common-lisp/source/cl-slime-swank/swank-loader.lisp"
    		    #p"/usr/share/common-lisp/source/slime/swank-loader.lisp"))
         (loader  (find-if #'probe-file loaders)))
    (load loader)))
;; FOR GUIX

;; from http://lists.common-lisp.net/pipermail/slime-devel/2008-August/015346.html
(require :swank)
(load-swank-loader)

#+swank-loader
(when (functionp 'swank-loader::init)
   ;; (setq swank-loader::*fasl-directory* "/tmp/fasl/")
   (swank-loader::init :setup nil))

#+swank
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

