;;; occ-obj-utils.el --- object utils                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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

(provide 'occ-obj-utils)


(require 'occ-obj)


(defvar occ-select-clock-in-operate-label    'occ-operate "should not be null")
(defvar occ-select-clock-in-select-label     'occ-selected "should not be null")
(defvar occ-select-clock-in-quit-label       'occ-quitted "should not be null")
(defvar occ-select-clock-in-true-label       'occ-true "should not be null")
(defvar occ-select-clock-in-false-label      'occ-false "should not be null")
(defvar occ-select-clock-in-default-function #'identity)
(defvar occ-select-clock-in-default-label    "Select")

(cl-assert occ-select-clock-in-operate-label)
(cl-assert occ-select-clock-in-true-label)
(cl-assert occ-select-clock-in-false-label)

(defun occ-build-return-lambda (action &optional label)
  #'(lambda (candidate)
      (let* ((value
              (funcall action candidate))
             (label
              (or label
                  (if value
                      occ-select-clock-in-true-label
                    occ-select-clock-in-false-label))))
       (make-occ-return :label label :value value))))

(defun occ-select-clock-in-tranform (action)
  "Will make all action except first to return occ-select-clock-in-label."
  (cons
   (cons                                ;add default select operation.
    occ-select-clock-in-default-label
    (occ-build-return-lambda occ-select-clock-in-default-function
                             occ-select-clock-in-operate-label))
   (mapcar #'(lambda (a)
               (if (consp a)
                   (cons (car a)
                         (occ-build-return-lambda a))
                 (occ-build-return-lambda a)))
           action)))

(defun occ-select-clock-in-tranformer-fun-transform (tranformer-fun)
  "Will make transformer fun to change action except first to return occ-select-clock-in-label."
  #'(lambda (action
             candidate)
      (occ-select-clock-in-tranform
       (funcall tranformer-fun action candidate))))

(cl-defmethod occ-return-operate-p (retval)
  retval)

(cl-defmethod occ-return-operate-p ((retval occ-return))
  (eq
   occ-select-clock-in-operate-label
   (occ-return-label retval)))

;; (cl-defmethod occ-return-operate-p ((retval null))
;;   nil)

(cl-defmethod occ-return-get-value (retval)
  retval)

(cl-defmethod occ-return-get-value ((retval occ-return))
  (occ-return-value retval))

(cl-defmethod occ-return-get-label (retval)
  retval)

(cl-defmethod occ-return-get-label ((retval occ-return))
  (occ-return-label retval))


;;; occ-obj-utils.el ends here
