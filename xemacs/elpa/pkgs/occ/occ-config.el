;;; occ-config.el --- occ config                     -*- lexical-binding: t; -*-

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

(provide 'occ-config)


(defcustom occ-unnamed t)

(defcustom occ-clockout-unassociable-to-unnamed 'ask) ;; TODO: or could ask to continue for TIME(m/h) with current task.



(defun occ-confirm (config
                    msg
                    timeout)
  (cond
   ((null config) nil)
   ((function config) (funcall config))
   ((eq config 'ask)  (y-or-n-p msg))
   ((eq config t) t)
   (t nil)))



;;; occ-config.el ends here
