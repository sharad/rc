;;; publishing.el --- publishing configuration

;; Copyright (C) 2015  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords:convenience, data, hypermedia, wp

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

;; Publishing related configuration.

;;; Code:

(provide 'publishing)

(defvar *doc-root*            (expand-file-name "Documents" "~"))
(defvar *created-content-dir* (expand-file-name "CreatedContent" *doc-root*))
(defvar *website-address*     "http://emacs-publishing.org/")


;; ;; Add function to set all four
;; (defvar *misc-top-dir*           (expand-file-name "contents/virtual/misc/default" *created-content-dir*))
;; (defvar *misc-top-style-dir*     (expand-file-name "generic/misc/style" *misc-top-dir*))
;; (defvar *misc-generated-top-dir* (expand-file-name "gen/misc" *created-content-dir*))
;; (defvar *misc-website-address*   (concat *website-address* "misc/"))
;; ;; variables

(defun top-dir (type &optional dir)
  (expand-file-name
   (expand-file-name
    "default"
    (expand-file-name
     type
     "contents/virtual")")
   ( type "/default")
   *created-content-dir*))

;;; publishing.el ends here
