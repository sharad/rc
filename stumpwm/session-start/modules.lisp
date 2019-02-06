

(in-package :stumpwm)

#-quicklisp
(defvar *contrib-dir* nil)

(defun load-external-module (module)
  #+quicklisp
  (if (ql:where-is-system module)
      (ql:quickload module)
      (message "failed to load ~a" module))
  #-quicklisp
  (when (and
         (boundp '*contrib-dir*)
         (probe-file *contrib-dir*))
    (stumpwm:load-module module)
    (stumpwm::message "failed to load ~a" module)))

;; (add-to-load-path #p"~/.stumpwm.d/modules")

;; ;;{{{ Load module

(when nil
 (load-external-module "amixer")
 (load-external-module "aumix")
 (load-external-module "battery")
 (load-external-module "battery-portable")
 (load-external-module "cpu")
 (load-external-module "disk")
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
 (load-external-module "window-tags"))

(defun load-all-modules ()
  (dolist
      (mod
        '(
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
          "notify"
          ;; "stumpwm.contrib.dbus"
          ))
    (stumpwm::message "loading ~a" mod)
    (ignore-errors
      (stumpwm::load-external-module mod))))

(load-all-modules)

;; enable
#+stumptray
(when (fboundp 'stumptray:stumptray)
    (stumptray:stumptray))

(defcommand load-all-eexternal-modules () ()
  (load-all-modules))

;; (load-external-module "wmii-like-stumpwmrc")

;;}}}
