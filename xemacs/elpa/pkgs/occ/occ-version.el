;;; occ-version.el --- occ version                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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

(provide 'occ-version)


;;;###autoload
(defun occ-release ()
  "The release version of Occ.
Inserted by installing Occ mode or when a release is made."
  (let ((occ-release "9.2"))
    occ-release))
;;;###autoload
(defun occ-git-version ()
  "The Git version of Occ mode.
Inserted by installing Occ or when a release is made."
  (let ((occ-git-version "9.2-43-gf9a8cc-elpaplus"))
    occ-git-version))
;;;###autoload
(defvar occ-odt-data-dir "/usr/share/emacs/etc/occ"
  "The location of ODT styles.")


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; occ-version.el ends here
