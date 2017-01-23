;;; controller-config.el --- controller comint

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(require 'comint)



(defvar controller-ip-buffers nil "Alist of ip and controller buffer.")

;; from: http://curiousprogrammer.wordpress.com/2009/03/27/emacs-comint/

(deh-require-maybe passwds-config

  (defun connect-controller (ip)
    (interactive "scontrollerip: ")
    (let ((buf (apply 'make-comint "controller" "telnet" nil `(,ip "2323"))))
      (acons k v controller-ip-buffers)
      (delete-other-windows)
      (switch-to-buffer-other-window buf)
      (other-window -1)
      (comint-send-string (get-buffer-process buf)
                          (concat controller-user "\n"
                                  controller-pass "\n"
                                  controller-spass "\n\n"))
      buf))

  (defun controller-buffer ()
    (let ((con (get-buffer-process "*controller*"))
          (if con
              con
              (connect-controller (read-from-minibuffer "controllerip: "))))))


  (defun controller-send-cmd ()
    (with-current-buffer (controller-buffer)
      ))

;;


  (testing
   (setq u (apply 'make-comint "controller" "telnet" nil `( ,(read-from-minibuffer "controllerip: ") "2323")))
   (switch-to-buffer-other-window u))


  )

(provide 'controller-config)
;;; controller.el ends here

