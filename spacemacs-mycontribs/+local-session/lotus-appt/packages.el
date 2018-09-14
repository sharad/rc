;;; packages.el --- lotus-appt layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-appt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-appt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-appt/pre-init-PACKAGE' and/or
;;   `lotus-appt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-appt-packages
  '(
    (appt :location local)
    )
  "The list of Lisp packages required by the lotus-appt layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-appt/init-appt ()
  (use-package appt
      ;; :defer t
      :defer t
      :config
      (progn
        (progn
          (use-package startup-hooks
              :defer t
              :config
              (progn
                (progn
                  (add-to-enable-startup-interrupting-feature-hook
                   )))))
        (progn

          (setq appt-msg-countdown-list '(10 5 1) ; XEmacs
                appt-audible (cons 3 .5)          ;
                appt-check-time-syntax nil        ; XEmacs
                appt-announce-method 'appt-persistent-message-announce  ; XEmacs
                appt-display-duration 59)

          (defvar diary-display-function-old nil "diary-display-function-old")
          (defvar show-diary-entries-and-appts t "show-diary-entries-and-appts")

          (defun diary-save-hook ()
            "Stuff to do when saving the diary files."
            (if (not running-xemacs)
                (appt-activate 1) ; use (appt-activate 1) for GNU Emacs
              (appt-initialize))
            (unless diary-display-function-old
              (message "Could disable it with disable-diary-appt-display-for function.")))

          (defun add-diary-save-hook ()
            "find-file-hooks hook to add the diary-save-hook when appropriate"
            (if (string-match "diary" (buffer-name))
                (add-hook 'after-save-hook 'diary-save-hook)))
          (add-hook 'find-file-hooks 'add-diary-save-hook)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Heres nice way to highlight an appointment in a brighter face on the
          ;; modeline so that you actually notice its appeared! (Doesnt work for me
          ;; in GNU Emacs CVS unfortunately ColinMarquardt)
          ;; ---great bit of code from Jeff Miller to highlight appointments in red on modeline---
          (defface appt-face
              '((t (:foreground "red" :background "white")))
            "Face to indicate a current appointment."
            :group 'appt)

          (defadvice appt-disp-window (before appt-hilite-more activate)
            (when appt-mode-string
              (put-text-property 1 (- (length appt-mode-string) 1)
                                 'face 'appt-face appt-mode-string)))

          (defadvice appt-check (after appt-hilite activate)
            (when appt-mode-string
              (put-text-property 1 (- (length appt-mode-string) 1)
                                 'face 'appt-face appt-mode-string)
              (force-mode-line-update)))
          )

        (progn
          ;; http://alfredobuttari.wordpress.com/2008/02/08/emacs-appt-mode-reminders-with-gtk-popups/
          (setq appt-display-format 'popup appt-audible t)
          ;; (defvar appt-notifier (concat  "DISPLAY=0.0 dbus-launch --autolaunch=" (getenv "MY_DBUS_SESSION") " /usr/bin/notify-send 'Appointment' '%s'"))
          ;; (defvar appt-notifier "zenity --info --title='Appointment' --text='%s'")

          (defvar appt-notifier
            '("notify-send Appointment '%s'"
              "pgrep osdsh && osdctl -s '%s'"))

          ;; (dolist (cmd appt-notifier)
          ;;         (shell-command (format cmd "asdfsdf")))

          (defun appt-notify (string)
            (if (consp appt-notifier)
                (dolist (cmd appt-notifier)
                  (shell-command (format cmd string)))
              (shell-command (format appt-notifier string))))


          (defun appt-display-message (string mins)
            "Display a reminder about an appointment.
The string STRING describes the appointment, due in integer MINS minutes.
The format of the visible reminder is controlled by `appt-display-format'.
The variable `appt-audible' controls the audible reminder."
            ;; let binding for backwards compatability. Remove when obsolete
            ;; vars appt-msg-window and appt-visible are dropped.
            (let ((appt-display-format
                   (if (eq appt-display-format 'ignore)
                       (cond (appt-msg-window 'window)
                             (appt-visible 'echo))
                     appt-display-format)))
              (cond ((eq appt-display-format 'window)
                     (funcall appt-disp-window-function
                              (number-to-string mins)
                              ;; TODO - use calendar-month-abbrev-array rather
                              ;; than %b?
                              (format-time-string "%a %b %e " (current-time))
                              string)
                     (run-at-time (format "%d sec" appt-display-duration)
                                  nil
                                  appt-delete-window-function))
                    ((eq appt-display-format 'echo)
                     (message "%s" string))
                    ((eq appt-display-format 'popup)
                     (appt-notify string)))
              (if appt-audible (beep 1))))

          ;; Note that you need zenity to be installed on your box. If you dont
          ;; like popups maybe you can replace the zenity command with a
          ;; notification from libnotify. Also, if you are a KDE userwellI assume
          ;; theres something equivalent to zenity or libnotify but you have to
          ;; figure it out by youself .
          ))))


;;; packages.el ends here
