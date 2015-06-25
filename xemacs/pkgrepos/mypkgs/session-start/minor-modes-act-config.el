;;; minor-modes-act-config.el ---

;; Copyright 2011 Sharad Pratap
;;
;; Author: sh4r4d@gmail.com
;; Version: $Id: minor-modes-act.el,v 0.0 2011/04/13 05:33:48 spratap Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:




(deh-require-maybe pabbrev
  (setq pabbrev-read-only-error nil)
  (defun pabbrev-activate ()
    (unless buffer-read-only
      (pabbrev-mode 1)))

  (add-element-to-lists 'pabbrev-activate pgm-langs)
  ;; Error during redisplay: (wrong-type-argument arrayp nil)
  ;; is courtesy of pabbrev
  ;; run
  ;; (remove-element-to-lists 'pabbrev-activate pgm-langs)
  ;; to correct it.
  )

(deh-require-maybe (and
                    (executable-find "p4")
                    vc-p4)

  (defvar enable-p4-login nil "test")

  (if (and (setq vc-p4-require-p4config t)
           (not (getenv "P4CONFIG")))
      (setenv "P4CONFIG" ".p4conf"))

  (require 'host-info)

  (defvar run-office-activate-failed-max 7 "run-office-activate")
  (defvar run-office-activate-failed 0 "run-office-activate")
  (defvar run-office-activate t "run-office-activate")

  (defun is-perforce-is-alive ()
    (if (shell-command-no-output "timeout -k 3 2 p4 depots")
        t
        (progn
          (incf run-office-activate-failed)
          nil)))

  (defun login-to-perforce ()
    (if (and
         enable-p4-login
         sharad-in-office-with-perforce
         (is-perforce-is-alive))
        ;; fix p4 timeout problem, detect it than disable it for next run.
        (if (shell-command-no-output "timeout -k 3 2 p4 user -o")
            t
            (shell-command-no-output "zenity --password | timeout -k 7 6 p4 login"))))

  (defun office-activate ()
    (if (and
         run-office-activate
         (< run-office-activate-failed run-office-activate-failed-max))
        (let ((file (buffer-file-name)))
          (when (and file
                     (with-timeout
                         (4 (progn
                              (login-to-perforce)
                              nil)))
                     (with-timeout
                         (4 (progn
                              (incf run-office-activate-failed)
                              nil))
                       (vc-p4-registered file)))
            ;; if file is handled by perforce than assume it is
            ;; related to office perforce repository.
            (office-mode 1)))
        (progn
          (message-notify "office-activate" "perforce is not reachable, so disabling office-activate.")
          (setq run-office-activate nil))))


  (if sharad-in-office-with-perforce
      (add-element-to-lists 'office-activate pgm-langs)))


(unless (fboundp 'login-to-perforce)
  (defun login-to-perforce ()
    (message "login-to-perforce: p4 not installed.")
    t))

(unless (fboundp 'is-perforce-is-alive)
  (defun is-perforce-is-alive ()
    (message "is-perforce-is-alive: p4 not installed.")
    t))

(defvar enable-p4-login nil "test")

(provide 'minor-modes-act-config)
;;; minor-modes-act.el ends here
