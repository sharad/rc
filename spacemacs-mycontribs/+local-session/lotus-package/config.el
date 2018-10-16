;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
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
    (add-hook 'lotus-enable-desktop-restore-interrupting-feature
              ;; 'lotus-enable-startup-interrupting-feature-hook
              '(lambda ()
                 (run-at-time-or-now 7
                                     '(lambda ()
                                        ;; Initialize installed packages
                                        (package-initialize)
                                        ;; package init not needed, since it is done anyway in emacs 24 after reading the init
                                        ;; but we have to load the list of available packages
                                        (package-refresh-contents)))))))

(defun lotus-update-installed-package-archive ()
  (interactive)
  (require 'package)
  (if package-alist
      (write-region
       (with-output-to-string
           (pp package-alist))
       ;; (prin1-to-string package-alist)
       nil lotus-package-installed-archive)
      (error "package-alist is not defiend, not doing anything.")))

(defun lotus-package-install-from-installed-archive ()
  (interactive)
  (require 'cl)
  (let* ((packages-from-installed-archive
           (mapcar 'car (lotus-read-sexp lotus-package-installed-archive)))
         (packages-from-package-alist (mapcar 'car package-alist))
         (packages-missing (set-difference packages-from-installed-archive packages-from-package-alist)))
    (if packages-missing
        (progn
          (package-refresh-contents)
          (dolist (p packages-missing)
            (package-install p)))
        (message "No missing package found."))))



(defun package-autoremove ()            ;adapt it
  "Remove packages that are no more needed.

Packages that are no more needed by other packages in
`package-selected-packages' and their dependencies
will be deleted."
  (interactive)
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or package-selected-packages
            (yes-or-no-p
             (format-message
              "`package-selected-packages' is empty! Really remove ALL packages? ")))
    (let ((removable (package--removable-packages)))
      (if removable
          (when (y-or-n-p
                 (format "%s packages will be deleted:\n%s, proceed? "
                         (length removable)
                         (mapconcat #'symbol-name removable ", ")))
            (mapc (lambda (p)
                    (package-delete (cadr (assq p package-alist)) t))
                  removable))
          (message "Nothing to autoremove")))))

)
;;; config.el ends here
