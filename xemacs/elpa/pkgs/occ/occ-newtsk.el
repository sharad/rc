;;; occ-newtsk.el --- occ new task                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'occ-newtsk)


(require 'org-capture+)
(require 'org-capture+-helm)


(defvar occ-tsk-templates-alist
  '(("TODO")
    ("MEETING")))

(defun occ-select-template ())

(org-capture+-build-helm-template-sources)

(org-capture+-helm-select-template)



;;; occ-newtsk.el ends here
