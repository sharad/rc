;;
;; elpa.el
;; Login : <s@taj>
;; Started on  Sun Dec  5 14:42:09 2010 Sharad Pratap
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


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

;; (when
;;     (load
;;      (expand-file-name "~/.xemacs/elpa/package.el"))
;;   (package-initialize))

(deh-require-maybe 'package
  (setq package-user-dir
        (expand-file-name (convert-standard-filename "~/.xemacs/elpa")))
  (package-initialize)

  (defconst *elpa-package-dir* "~/.xemacs/elpa")

  (when (file-directory-p *elpa-package-dir*)
    (mapc #'(lambda (path)
              (when (file-directory-p path)
                (add-to-list 'load-path path)))
          (directory-files *elpa-package-dir* t "[a-zA-Z]+"))
    (byte-recompile-directory *elpa-package-dir*)))


(user-provide 'elpa)

