;;; occ-debug-method.el --- occ debug method         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, abbrev

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

(provide 'occ-debug-method)


(require 'occ-util-common)
(require 'occ-obj)

(cl-defmethod occ-print-tsk ((obj occ-obj-tsk))
  "Dump tsk"
  (occ-message "occ-print-tsk: %s" obj))

(cl-defmethod occ-print-tsk ((obj occ-obj-ctx-tsk))
  "Dump ctx-tsk"
  (let ((tsk (occ-obj-tsk obj)))
    (occ-message "occ-print-tsk: %s" tsk)))

;;; occ-debug-method.el ends here
