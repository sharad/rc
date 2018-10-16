;;; org-context-clock-assoc-predicate.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

;; "Org tasks associated to context predicate functions"

(defvar org-context-clock-task-associated-context-predicate-fns nil)

(defun org-task-associated-to-context-by-predicate-p (task context)
  (if context
      (some
       '(lambda (fn) (> 0 (funcall fn context task)))
       org-context-clock-task-associated-context-predicate-fns)))


(org-context-clock-assoc-api-set :predicate :taskp 'org-context-clock-task-associated-to-context-by-predicate-p)

(provide 'org-context-clock-assoc-predicate)
;;; org-context-clock-assoc-predicate.el ends here
