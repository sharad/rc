;;; package-config.el --- package

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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




(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))



(eval-after-load "package"
  '(progn
    (require 'cl)
    (require 'misc-config)

    (defvar sharad/package-installed-archive "~/.xemacs/pkgrepos/elpa/installed-archive.el" "Known Installed packages.")

    (when (file-exists-p sharad/package-installed-archive)
      (when (set-difference (mapcar 'car  (sharad/read-file sharad/package-installed-archive))
                            (mapcar 'car package-alist))
        (message "Your do not have all packages installed.\n install it will sharad/package-install-from-installed-archive.")))

    (defun sharad/update-installed-package-archive ()
      (interactive)
      (if package-alist
          (write-region (prin1-to-string package-alist) nil sharad/package-installed-archive)
          (message "package-alist is not defiend, not doing anything.")))

    (defun sharad/package-install-from-installed-archive ()
      (interactive)
      (require 'cl)
      (let* ((packages-from-installed-archive  (mapcar 'car  (sharad/read-file sharad/package-installed-archive)))
             (packages-from-package-alist (mapcar 'car package-alist))
             (packages-missing (set-difference packages-from-installed-archive packages-from-package-alist)))
        (dolist (p packages-missing)
          (package-install p))))))


(provide 'package-config)

;;; package.el ends here

