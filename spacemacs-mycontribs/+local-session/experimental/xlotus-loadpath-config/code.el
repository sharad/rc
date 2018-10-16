;;; loadpath-config.el --- Load Path

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords:

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

(require 'macros-config      "~/.xemacs/pkgrepos/mypkgs/session-start/macros-config.el")
(require 'basic-utils-config "~/.xemacs/pkgrepos/mypkgs/session-start/basic-utils-config.el")

(deh-section "General"

  (deh-section "loadpath"                                ;add to loadpath

    (add-to-list 'load-path "/usr/local/share/emacs/23.3/site-lisp") ;; need it for gtags.el gtags over tramp

    (defun package-dir-add-to-loadpath (package-dir &optional recursive)
      (when (file-directory-p package-dir)
        (mapc
         (if recursive
             (lambda (path)
               (add-to-list 'load-path path)
               (let ((default-directory path))
                 (normal-top-level-add-subdirs-to-load-path)))
             (lambda (path)
               (add-to-list 'load-path path)))
         (remove-if-not
	  'file-directory-p
	  (directory-files package-dir t "[a-zA-Z]+")))))




    (mapc
     '(lambda (dir)
       (add-to-list 'load-path dir t))  ;auto-install at end as they are generally outdated.
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info.d/common/elisp"
       ,(concat "~/.osetup/info.d/hosts/" (system-name) "/elisp"))))

    ;; (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/elpa")
    (dolist (d (remove-if-not
                'file-directory-p
                (directory-files "~/.xemacs/pkgrepos/world/" t "[a-zA-Z]+")))
      (package-dir-add-to-loadpath d t))


    (setq load-path (remove-if-not 'file-directory-p load-path))

  (deh-section "byte-compile"                                ;byte compile
    (defun package-dir-byte-compile (package-dir)
      (when (file-directory-p package-dir)
        (mapc #'(lambda (dir)
                  (ignore-errors (byte-recompile-directory dir 0)))
              (directory-files package-dir t "[a-zA-Z]+"))))

    ;; (package-dir-byte-compile "~/.xemacs/pkgrepos/world")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/elpa")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/misc")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/gits")
    (package-dir-add-to-loadpath "/usr/local/share/emacs/site-lisp")

    (mapc
     '(lambda (dir)
       (byte-recompile-directory dir 0))
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info.d/common/elisp"
       ,(concat "~/.osetup/info.d/hosts/" (system-name) "/elisp")))))


(require 'dot-emacs-helper nil nil)
(require 'macros-config)
(require 'basic-utils-config)
(require 'basic-config)
(require 'general-testing)
(require 'common-info nil t)
(require 'auto-load-config)
(require 'macros-config)
(require 'basic-utils-config)
(eval-when-compile
 (require 'use-package))




(provide 'loadpath-config)
;;; loadpath-config.el ends here
