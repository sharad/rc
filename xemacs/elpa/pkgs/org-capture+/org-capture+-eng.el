;;; org-capture+-eng.el --- org capture plus eng     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

(provide 'org-capture+-eng)


(require 'org-capture+-helm-dynamic)


(defun org-capture+-select-type ()
  '(entry item chckitem table-line plain log-note))

(defun org-capture+-select-target ()
  '(
    (file "path/to/file")
    (id "id of existing Org entry")
    (file+headline "path/to/file" "node headline")
    (file+olp "path/to/file" "Level 1 heading" "Level 2" ...)
    (file+olp+datetree "path/to/file" "Level 1 heading" ...)
    (file+function "path/to/file" function-finding-location)
    (clock)
    (function function-finding-location)))

;;; org-capture+-eng.el ends here
