

(in-package :stumpwm)


(defun load-external-module (module)
  (if (and
       (boundp '*contrib-dir*)
       (probe-file *contrib-dir*))
      (load-module module)
#+quicklisp
      (if (ql:where-is-system module)
          (ql:quickload module))))

(add-to-load-path #p"~/.stumpwm.d/modules")

;; ;;{{{ Load module
(load-external-module "amixer")
(load-external-module "aumix")
(load-external-module "battery")
(load-external-module "battery-portable")
;;(load-external-module "cpu")
;; (load-external-module "disk")
(load-external-module "g15-keysyms")
(load-external-module "maildir")
(load-external-module "mem")
(load-external-module "mpd")
;; (load-external-module "net")
(load-external-module "notifications")
(load-external-module "productivity")
#+sbcl (load-external-module "sbclfix")
;; (load-external-module "surfraw")
(load-external-module "wifi")
(load-external-module "window-tags")
;;(load-external-module "wmii-like-stumpwmrc")
;;}}}
