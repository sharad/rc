;;; init-setup.el --- Startup related functions advices

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'utils-custom)


;; hack-local-variables-confirm (all-vars unsafe-vars risky-vars dir-name)

;; (defadvice hack-local-variables-confirm (around bypass-success disable)
(defadvice hack-local-variables-confirm (around bypass-success activate)
  ;; check session-config.el
  (if (and (boundp 'desktop-restore-in-progress)
           desktop-restore-in-progress)
      t
      (unless (have-x-focus)
        (message-notify "hack-local-variables-confirm" "Need attention.")
        ad-do-it)))

(ad-activate 'hack-local-variables-confirm)

(defmacro defnotify-ad-before (fn)
  `(defadvice fn (before (concat "notify-ad-" ,(symbol-name fn)) disable)
     (unless (have-x-focus)
       (message-notify ,(symbol-name fn) "Need attention."))))

(defun activate-notify-ad-before (fn)
  `(ad-activate ,fn))

;; epa-passphrase-callback-function
;; ;; (defun epa-passphrase-callback-function (context key-id handback)

(deh-section "set dbus env"
  ;; (let* ((display-str (or (getenv "DISPLAY" (selected-frame))
  ;;                         ":0.0"))
  ;;        (dismajor-str (if (>= (length display-str) 2)
  ;;                          (substring display-str 1 2)
  ;;                          "0")))
  ;;   (setenv-from-file
  ;;    (concat
  ;;     "~/.dbus/session-bus/"
  ;;     (trim-string (lotus-read-file "/var/lib/dbus/machine-id"))
  ;;     "-" dismajor-str)
  ;;    '(:system :session)))

  (defun set-dbus-session ()
    (interactive)
    (flet ((get-string-from-file (filePath)
                                 "Return filePath's file content."
                                 (with-temp-buffer
                                   (insert-file-contents filePath)
                                   (buffer-string))))
      (let ((machine-id-file
             (find-if #'file-exists-p  '("/etc/machine-id"
                                         "/var/lib/dbus/machine-id"))))
       (let* ((display-str  (or (getenv "DISPLAY" (selected-frame)) ":0.0"))
              (dismajor-str (if (>= (length display-str) 2)
                                (substring display-str 1 2)
                              "0"))
              (dbus-file    (concat "~/.dbus/session-bus/" (string-trim (lotus-read-file machine-id-file)) "-" dismajor-str)))
          (ignore-errors
            (dbus-setenv :system "DISPLAY" display-str))
          (ignore-errors
            (dbus-setenv :session "DISPLAY" display-str))
          (setenv-from-file
           dbus-file
           '(:system :session))))))

  (set-dbus-session))


(provide 'init-setup)
;;; startup-config.el ends here
