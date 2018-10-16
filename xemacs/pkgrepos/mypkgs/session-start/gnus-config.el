;;
;; gnus-config.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Wed Jun 30 12:02:07 2010 Sharad Pratap
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


;;{{

(require 'host-info)

(when nil
  (when (and (boundp 'gnus-lib-path) gnus-lib-path (file-directory-p gnus-lib-path))
    (add-to-list 'load-path gnus-lib-path)
    (require 'gnus-util (concat gnus-lib-path "/gnus-util.el"))
    (load-file (concat gnus-lib-path "/gnus-util.el"))
    (require 'gnus-util (concat gnus-lib-path "/gnus-util.el"))
    (load-file (concat gnus-lib-path "/gnus-util.el"))
    (load-file (concat gnus-lib-path "/mm-util.el"))))

;;}}


;;{{ Good :: Excellent beautiful Great!! Thanks XSteve
;; Use the keybinding M-F7 to toggle between the gnus window configuration and your normal editing windows.
(defun xsteve-gnus ()
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or
         (string-equal "*Group*" bufname)
         (string-equal "*BBDB*" bufname)
         (string-match "\*Summary" bufname)
         (string-match "\*Article" bufname))
        (progn
          (xsteve-bury-gnus))
                                        ;unbury
        (if (get-buffer "*Group*")
            (unless (xsteve-unbury-gnus)
              (gnus-plugged))
              ; (gnus-unplugged))
            ;; (progn
            ;;   (xsteve-unbury-gnus)
            ;;   (if (functionp 'gnus-summary-rescan-group)
            ;;       (gnus-summary-rescan-group)))
            (gnus-plugged)))))
            ;;(gnus-unplugged)))))

(defun xsteve-unbury-gnus ()
  (interactive)
  (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
    (set-window-configuration gnus-bury-window-configuration)))

(defun xsteve-bury-gnus ()
  (interactive)
  (setq gnus-bury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or
             (string-equal "*Group*" bufname)
             (string-equal "*BBDB*" bufname)
             (string-match "\*Summary" bufname)
             (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))))))

;;}}

;;{{

(when (and (xrequire 'cl)
          (xrequire 'footnote))
  (setq footnote-body-tag-spacing 1))

;;}}


;;{{ from http://www.emacswiki.org/emacs/GnusNetworkManager

;; If you want to start up Gnus in offline or online state depending on
;; the current network status, you can add a custom Gnus startup
;; function in ~/.emacs, something like this:

(if (xrequire 'dbus)
    (defun nm-is-connected()
      (equal 3 (dbus-get-property
                :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                "org.freedesktop.NetworkManager" "State")))
    (defun switch-to-or-startup-gnus ()
      "Switch to Gnus group buffer if it exists, otherwise start Gnus in plugged or unplugged state,
depending on network status."
      (interactive)
      (if (or (not (fboundp 'gnus-alive-p))
              (not (gnus-alive-p)))
          (if (nm-is-connected)
              (gnus)
              (gnus-unplugged))
          (switch-to-buffer gnus-group-buffer)
          (delete-other-windows))))


;;}}

(provide 'gnus-config)

;;; gnus-config.el ends here
