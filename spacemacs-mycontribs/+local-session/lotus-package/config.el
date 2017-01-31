;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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
(when nil
;; http://www.emacswiki.org/emacs/el-get

(progn ;; "package detail"
  ;; Make sure a package is installed
  (defun package-require (package)
    "Install a PACKAGE unless it is already installed
or a feature with the same name is already active.

Usage: (package-require 'package)"
                                        ; try to activate the package with at least version 0.
    (package-activate package '(0))
                                        ; try to just require the package. Maybe the user has it in his local config
    (condition-case nil
        (require package)
                                        ; if we cannot require it, it does not exist, yet. So install it.
      (error (package-install package))))

  (when nil
    (add-hook 'sharad/enable-desktop-restore-interrupting-feature
              ;; 'sharad/enable-startup-interrupting-feature-hook
              '(lambda ()
                 (run-at-time-or-now 7
                                     '(lambda ()
                                        ;; Initialize installed packages
                                        (package-initialize)
                                        ;; package init not needed, since it is done anyway in emacs 24 after reading the init
                                        ;; but we have to load the list of available packages
                                        (package-refresh-contents)))))))

(defun sharad/update-installed-package-archive ()
  (interactive)
  (require 'package)
  (if package-alist
      (write-region
       (with-output-to-string
           (pp package-alist))
       ;; (prin1-to-string package-alist)
       nil sharad/package-installed-archive)
      (error "package-alist is not defiend, not doing anything.")))

(defun sharad/package-install-from-installed-archive ()
  (interactive)
  (require 'cl)
  (let* ((packages-from-installed-archive
          (mapcar 'car (sharad/read-sexp sharad/package-installed-archive)))
         (packages-from-package-alist (mapcar 'car package-alist))
             (packages-missing (set-difference packages-from-installed-archive packages-from-package-alist)))
    (if packages-missing
        (progn
          (package-refresh-contents)
          (dolist (p packages-missing)
            (package-install p)))
        (message "No missing package found."))))
)
;;; config.el ends here
