;;; planner-bugz.el --- Planner Bugzilla Interface

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
;; Keywords: lisp

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


(defun planner-bugzilla-find-bugtasks-in-page (page)
  )

(defun planner-bugzilla-update-existing-task (how)
  ;; how could be 'ask, 'force
  )

(defun planner-bugzilla-fetch-new-task ()
  ;; add url username used in bug list.
  )

(defun get-attribute (prompt)
  (let (retval)
    (setq retval (read-from-minibuffer prompt))
    (if (not (string-equal retval ""))
        retval)))

(defun get-value (prompt)
  (read-from-minibuffer prompt))


(defun planner-bugzilla-make-bug-search-criteria ()
  (interactive)
  (let (attribute)
    (loop until (not (setq attribute (get-attribute "attributes: ")) "")
         collect (cons attribute (get-value (concat "value for " attribute ": "))))))






(provide 'planner-bugz)
;;; planner-bugz.el ends here
