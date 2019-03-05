;;; occ-util-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

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





(provide 'occ-util-common)



(defun occ-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((helm-always-two-windows nil))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist
                     def
                     inherit-input-method)))

(cl-defmethod ignore-p ((buff buffer))
  nil)



(defmacro run-unobtrusively (&rest body)
  `(while-no-input
    (redisplay)
    ,@body))


(defmacro run-unobtrusively (&rest body)
  `(let ((retval (while-no-input
                   (redisplay)
                   ,@body)))
     (when (eq retval t)
       (message "user input %s retval %s" last-input-event retval))
     retval))


(defmacro condition-case-control (enable var bodyform &rest handlers)
  (if enable
      `(condition-case ,var
           ,bodyform
         ,@handlers)
    bodyform))
(put 'condition-case-control 'lisp-indent-function 2)






;;; occ-util-common.el ends here
