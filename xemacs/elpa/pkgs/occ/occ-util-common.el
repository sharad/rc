;;; occ-util-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(cl-defmethod ignore-p ((buff buffer))
  nil)



(defmacro run-unobtrusively (&rest body)
  `(while-no-input
    (redisplay)
    ,@body))



(when nil
 (defun time-consuming ()
  (cl-loop for i below (* 1000 1000 1) sum i))

 (defun test-no-input ()
  (let ((retval))
    (message "last-input-event %s retval %s" last-input-event retval)))

 (progn
   (run-with-idle-timer 3 nil #'test-no-input)
   (run-with-idle-timer 6 nil #'test-no-input)
   (run-with-idle-timer 18 nil #'test-no-input)))

(provide 'occ-util-common)
;;; occ-util-common.el ends here
