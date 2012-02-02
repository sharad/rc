;;; general-testing.el --- Testing macros etc

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad at home>
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


;;{{ Testing


(defvar testing nil "Set it to true when you want to enable test code.")

(defmacro testing (&rest forms)
  "For the purpose of testing."
  (if (and (boundp 'testing)
           testing)
      `(progn ,@forms)))


(testing (message "asdfsdf"))

;;}}



(provide 'general-testing)

;;; testing.el ends here
