;;
;; nm.el
;; Login : <s@taj>
;; Started on  Fri Nov 26 00:54:06 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


;;{{ from http://www.emacswiki.org/emacs/GnusNetworkManager
;(when (and nil (xrequire 'dbus))
(when (xrequire 'dbus)
  (defvar gnus-nm-dbus-registration nil
    "dsfdsf")
    (defvar gnus-nm-connected-hook nil
      "Functions to run when network is connected.")
    (defvar gnus-nm-disconnected-hook nil
      "Functions to run when network is disconnected.")
    (setq gnus-nm-dbus-registration nil)

    (defun imap-nuke-server-processes ()
      "Brutally kill running IMAP server background processes. Useful
when Gnus hangs on network outs or changes."
      (interactive)
      (let ((sm (if gnus-select-method
                    (cons gnus-select-method gnus-secondary-select-methods)
                    gnus-secondary-select-methods)))
        (while sm
          (let ((method (car (car sm)))
                (vserv (nth 1 (car sm))))
            (when (and (eq 'nnimap method)
                       (not (string= "localhost"
                                     ;(second (find-if
                                     (second (remove-if-not
                                              (lambda (e)
                                                (if (listp e)
                                                    (eq 'nnimap-address (car e))))
                                              sm))))
                       (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))
              (gnus-message 6 "Killing IMAP process for server %s" vserv)
              (delete-process (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))))
          (setq sm (cdr sm)))))

    (defun gnus-nm-agent-unplug()
      "Kill IMAP server processes and unplug Gnus agent."
      (gnus-message 6 "Network is disconnected, unplugging Gnus agent.")
      (with-current-buffer gnus-group-buffer
        (imap-nuke-server-processes) ; optional, help prevent hangs in IMAP processes when network has gone down.
        (gnus-agent-toggle-plugged nil)))

    (defun gnus-nm-agent-plug()
      "Plug Gnus agent."
      (gnus-message 6 "Network is connected, plugging Gnus agent.")
      (with-current-buffer gnus-group-buffer
        (gnus-agent-toggle-plugged t)))

    (defun gnus-nm-state-dbus-signal-handler (nmstate)
      "Handles NetworkManager signals and runs appropriate hooks."
      (when (and (fboundp 'gnus-alive-p) (gnus-alive-p))
        (cond
          ((or (= 4 nmstate) (= 1 nmstate))
           (run-hooks 'gnus-nm-disconnected-hook))
          ((= 3 nmstate)
           (run-hooks 'gnus-nm-connected-hook)))))

    (defun gnus-nm-enable()
      "Enable integration with NetworkManager."
      (interactive)
      (when (not gnus-nm-dbus-registration)
        (progn (setq gnus-nm-dbus-registration
                     (dbus-register-signal :system
                                           "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                                           "org.freedesktop.NetworkManager" "StateChanged"
                                           'gnus-nm-state-dbus-signal-handler))
               (gnus-message 6 "Enabled integration with NetworkManager"))))

    (defun gnus-nm-disable()
      "Disable integration with NetworkManager."
      (interactive)
      (when gnus-nm-dbus-registration
        (progn (dbus-unregister-object gnus-nm-dbus-registration)
               (setq gnus-nm-dbus-registration nil)
               (gnus-message 6 "Disabled integration with NetworkManager"))))

    ;; Add hooks for plugging/unplugging on network state change:
    (add-hook 'gnus-nm-connected-hook    'gnus-nm-agent-plug)
    (add-hook 'gnus-nm-connected-hook    'gnus-group-send-queue)
    (add-hook 'gnus-nm-disconnected-hook 'gnus-nm-agent-unplug)
    ;; Add hooks for enabling/disabling integration on startup/shutdown:
    (add-hook 'gnus-started-hook   'gnus-nm-enable)
    (add-hook 'gnus-exit-gnus-hook 'gnus-nm-disable))
;;}} (gnus-nm-disable)

(provide 'nm-config)

