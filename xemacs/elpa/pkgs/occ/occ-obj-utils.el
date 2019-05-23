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


(defvar occ-return-select-label     :occ-selected    "should not be null")
(defvar occ-return-quit-label       :occ-nocandidate "should not be null")
(defvar occ-return-nocndidate-label :occ-quitted     "should not be null")
(defvar occ-return-timeout-label    :occ-timeout     "should not be null") ;TODO: need to implement.
(defvar occ-return-true-label       :occ-true        "should not be null")
(defvar occ-return-false-label      :occ-false       "should not be null")
(cl-assert occ-return-select-label)
(cl-assert occ-return-quit-label)
(cl-assert occ-return-nocndidate-label)
(cl-assert occ-return-true-label)
(cl-assert occ-return-false-label)

(defvar occ-return-select-function #'identity)
(defvar occ-return-select-name     "Select")
(cl-assert occ-return-select-function )
(cl-assert occ-return-select-name     )



(defun occ-build-return-lambda (action &optional label)
  #'(lambda (candidate)
      (let* ((value
              (funcall action candidate))
             (label
              (or label
                  (if value
                      occ-return-true-label
                    occ-return-false-label))))
        (occ-make-return label value))))

(defun occ-return-tranform (action)
  "Will make all action except first to return occ-return-label."
  (cons
   (cons                                ;add default select operation.
    occ-return-select-name
    (occ-build-return-lambda occ-return-select-function
                             occ-return-select-label))
   (mapcar #'(lambda (a)
               (if (consp a)
                   (cons (car a)
                         (occ-build-return-lambda (cdr a)))
                 (occ-build-return-lambda a)))
           action)))

(defun occ-return-tranformer-fun-transform (tranformer-fun)
  "Will make transformer fun to change action except first to return occ-return-label."
  #'(lambda (action
             candidate)
      (occ-return-tranform (funcall tranformer-fun
                                    action candidate))))

;; (cl-defmethod occ-return-operate-p (retval)
;;   retval)

;; (cl-defmethod occ-return-operate-p ((retval occ-return))
;;   (eq
;;    occ-return-operate-label
;;    (occ-return-label retval)))

(cl-defmethod occ-return-in-labels-p (retval &rest label)
  retval)

(cl-defmethod occ-return-in-labels-p ((retval occ-return) &rest label)
  (memq
   (occ-return-label retval)
   label))

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
