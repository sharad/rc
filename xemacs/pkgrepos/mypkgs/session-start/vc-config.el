;;; vc-config.el --- Vsrsion Control

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: files

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




(defun file-auto-rcs-register (&optional file)
  (let ((file (or file (buffer-file-name (current-buffer)))))
    (if (and (not (vc-rcs-registered file))
             (vc-register file))
        (vc-switch-backend file 'RCS)
        (vc-checkin (list file) 'RCS (number-to-string
                               (+ 0.1 (string-to-number (vc-working-revision file 'RCS))))
                    (read-from-minibuffer "comment: ")))))

;; (add-hook 'after-save-hook 'file-auto-rcs-register)



(provide 'vc-config)
;;; vc-config.el ends here

