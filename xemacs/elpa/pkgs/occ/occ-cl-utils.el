;;; occ-cl-utils.el --- occ cl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-cl-utils)


(require 'cl-macs)
(require 'cl-generic)


(defun cl-classname (inst)
  (intern
   (substring
    (symbol-name (aref inst 0))
    (length "cl-struct-"))))
(defun cl-get-field (object field)
  (cl-struct-slot-value (cl-classname object) field object))
(defun cl-set-field (object field value)
  (setf (cl-struct-slot-value (cl-classname object) field object) value))
(defun cl-get-fields (object fields)
  (mapcar
   #'(lambda (field)
       (cons field (cl-get-field object field)))
   fileds))
(defun cl-class-slots (class)
  (mapcar
   #'(lambda (slot) (aref slot 1))
   (cl--struct-class-slots
    (cl--struct-get-class class))))
;; (defun cl-class-slot-value (obj slot)
;;   (when (member slot (cl-class-slots (cl-classname obj)))
;;     (cl-struct-slot-value (cl-classname obj) slot obj)))
(defun cl-class-obj-slot-value (class slot obj)
  (when (member slot (cl-class-slots class))
    (cl-struct-slot-value class slot obj)))
(defun cl-obj-slot-value (obj slot)
  (cl-class-obj-slot-value (cl-classname obj) slot obj))
(defun cl-obj-plist-value (obj)
  (cl-obj-slot-value obj 'plist))


;; TODO: find how to do calculation without eval here.

(cl-defun cl-method-param-signs (method)
  "Get params signatures for all defined methods"
  (let ((method-instances (cl--generic method)))
   (mapcar
    #'(lambda (x) (aref x 1))
    (if method-instances
        (aref method-instances 3)))))

(cl-defun cl-method-param-case (signature-val-spec)
   "signature-val-spec = (METHOD (PARAMS VAL))"
   (cl-destructuring-bind (method (param-spec val)) signature-val-spec
     (remove
      nil
      (mapcar
       #'(lambda (fspec)
           (eval
            `(pcase ',fspec
               (,param-spec ,val)
               (_ nil))))
       (cl-method-param-signs method)))))

;; ;; (cl-method-param-signs 'occ-rankprop)

;; ((occ-tsk (eql current-clock)) (occ-tsk (eql heading-level)) (occ-tsk (eql key)) (occ-tsk (eql status)) (occ-obj-ctx-tsk (eql timebeing)) (occ-obj-ctx-tsk (eql root)) (occ-obj-ctx-tsk (eql currfile)) (occ-obj-ctx-tsk symbol) (occ-tsk symbol) (t t))

;; (pcase '(occ-obj-ctx-tsk (eql currfile))
;;   (`(occ-obj-ctx-tsk (eql ,val)) val)
;;   (_ nil))


;; (pcase '(occ-obj-ctx-tsk (eql timebeing))
;;   (`(occ-obj-ctx-tsk (eql ,val)) val)
;;   (_ nil))

;; (pcase '(occ-tsk (eql current-clock))
;;   (`(occ-obj-ctx-tsk (eql ,val)) val)
;;   (_ nil))

;; (cl-destructuring-bind (method (param-spec val)) '(occ-rankprop (`(occ-obj-ctx-tsk (eql ,val)) val))
;;   (list method param-spec val))


;; (occ-rankprop `(occ-obj-ctx-tsk (eql ,val)) val)

;; (cl-method-param-case '(occ-rankprop (`(occ-obj-ctx-tsk (eql ,val)) val)))

;; (pcase (quote (occ-tsk (eql current-clock))) ((\` (occ-obj-ctx-tsk (eql (\, val)))) val) (_ nil))

;; (pcase (quote (t t)) ((\` (occ-obj-ctx-tsk (eql (\, val)))) val) (_ nil))

;; (pcase (quote (occ-tsk (eql current-clock))) ((\` (occ-obj-ctx-tsk (eql (\, val)))) val) (_ nil))

;; (pcase--expand (quote (occ-tsk (eql current-clock))) (((\` (occ-obj-ctx-tsk (eql (\, val)))) val) (_ nil)))

;; (cl-method-param-case '(occ-rankprop (`(occ-obj-ctx-tsk (eql ,val)) val)))

;; (pcase--expand '(occ-tsk (eql current-clock)) '((`(occ-obj-ctx-tsk (eql ,val)) val) (_ nil)))

;; (pcase (quote (occ-tsk (eql current-clock))) ((\` (occ-obj-ctx-tsk (eql (\, val)))) val) (_ nil))

(cl-defun cl-method-param-case-with-value (signature-val-spec obj)
 "signature-val-spec = (METHOD PARAMS VAL)"
 (cl-destructuring-bind (method (param-spec val)) signature-val-spec
   (remove
    nil
    (mapcar
     #'(lambda (fspec)
         (let ((first-arg
                (eval
                 `(pcase ',fspec
                    (,param-spec ,val)
                    (_ nil)))))
           (when (and
                  first-arg
                  (funcall method (cons first-arg obj)))
             first-arg)))
     (cl-method-param-signs method)))))

(defun cl-method-param-case-with-value-new (signature-val-spec obj)
  "signature-val-spec = (METHOD PARAMS VAL)"
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove
     nil
     (mapcar
      #'(lambda (fspec)
          (let ((first-arg
                 (eval
                  `(pcase ',fspec
                     (,param-spec ,val)
                     (_ nil)))))
            (when (and
                   first-arg
                   ;; (funcall method (cons first-arg obj))) -- TODO BUG make it general
                   (funcall method obj first-arg))
              first-arg)))
      (cl-method-param-signs method)))))


(defun cl-method-arg-get (method fn)
  (mapcar
   fn
   (cl-method-param-signs method)))

(defun cl-method-first-arg (method)
  (mapcar
   #'(lambda (fspec) (cadar fspec))
   (cl-method-param-signs method)))

(defun cl-method-first-arg (method)
  (cl-method-arg-get method #'cadar))

(defun cl-method-first-arg-with-value (method obj)
  (mapcar
   #'(lambda (fspec)
       (let ((first-arg (cadar fspec)))
         (when (funcall method (cons first-arg obj)) first-arg)))
   (cl-method-param-signs method)))





;;; occ-cl-utils.el ends here
