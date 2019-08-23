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


(eval-when-compile
  (require 'pcase))
(require 'cl-macs)
(require 'cl-generic)

(when nil
  (progn

    (setq tsk-test (occ-make-tsk-at-point))

    (cl-structure-class cl-struct-occ-tree-tsk)

    (cl-structure-class- cl-struct-occ-tree-tsk)

    (cl-structure-class tsk-test)

    (cl--struct-get-class tsk-test)

    (cl--struct-class-slots cl-struct-occ-tree-tsk)

    (cl--struct-get-class cl-struct-occ-tree-tsk)

    (cl--struct-class-slots cl-struct-cl-structure-class)

    (cl-structure-class-parents cl-struct-cl-structure-class)

    (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-cl-structure-class)

    (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-occ-tree-tsk)

    (cl--struct-get-class tsk-test)

    (cl-struct-slot-value 'cl-structure-class 'name (symbol-value (aref tsk-test 0)))

    (cl--struct-class-slots (symbol-value (aref tsk-test 0)))


    (cl-class-parent-names (cl-class tsk-test))

    (cl-inst-class-parent-names tsk-test)

    (occ-operations-for-prop tsk-test 'root)))




(defun occ-flatten (L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (if (null L)
      nil
    (if (atom (first L))
        (cons (first L) (occ-flatten (rest L)))
      (append (occ-flatten (first L)) (occ-flatten (rest L))))))


;; (setq test-xyz (make-occ-ctx))

;; (cl--struct-class-slots
;;  (cl--struct-get-class 'occ-ctx))


(defun cl-class (inst)
  (let* ((class-sym (aref inst 0))
         (class-sym (if (boundp class-sym)
                        class-sym
                      (intern (concat "cl-struct-" (symbol-name class-sym))))))
    (symbol-value class-sym)))

(defun cl-classname (class)
  (cl-struct-slot-value 'cl-structure-class
                        'name
                        class))


(defun cl-class-parents (class)
  (cl-struct-slot-value 'cl-structure-class
                        'parents
                        class))

(defun cl-class-parent-names (class)
  (mapcar
   #'(lambda (parent)
       (cons
        (cl-struct-slot-value 'cl-structure-class
                              'name
                              parent)
        (when parent (cl-class-parent-names parent))))
   (cl-class-parents class)))


(defun cl-inst-classname (inst)
  (cl-classname (cl-class inst)))

(defun cl-inst-class-parent-names (inst)
  (occ-flatten
   (cl-class-parent-names (cl-class inst))))

(defun cl-inst-class-names (inst)
  (cons
   (cl-inst-classname inst)
   (occ-flatten
    (cl-class-parent-names (cl-class inst)))))


(defun cl-get-field (object field)
  (cl-struct-slot-value (cl-inst-classname object) field object))
(defun cl-set-field (object field value)
  (setf (cl-struct-slot-value (cl-inst-classname object) field object) value))
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
;;   (when (member slot (cl-class-slots (cl-inst-classname obj)))
;;     (cl-struct-slot-value (cl-inst-classname obj) slot obj)))
(defun cl-class-obj-slot-value (class slot obj)
  (when (member slot (cl-class-slots class))
    (cl-struct-slot-value class slot obj)))
(defun cl-obj-slot-value (obj slot)
  (cl-class-obj-slot-value (cl-inst-classname obj) slot obj))
(defun cl-obj-plist-value (obj)
  (cl-obj-slot-value obj 'plist))


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
          (funcall
           `(lambda ()
              (pcase ',fspec
                (,param-spec ,val)
                (_ nil)))))
      (cl-method-param-signs method)))))

(cl-defun cl-method-param-case-with-value (signature-val-spec obj)
  "signature-val-spec = (METHOD PARAMS VAL)"
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove
     nil
     (mapcar
      #'(lambda (fspec)
          (let ((first-arg
                 (funcall
                  `(lambda ()
                     (pcase ',fspec
                       (,param-spec ,val)
                       (_ nil))))))
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
                 (funcall
                  `(lambda ()
                     (pcase ',fspec
                       (,param-spec ,val)
                       (_ nil))))))
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


(defun cl-method-param-values (method param-exp val)
  (funcall
   `(lambda ()
      (cl-method-param-case '(,method (,param-exp ,val))))))


(defun cl-collect-on-classes (fn inst)
  (apply #'append
         (mapcar #'(lambda (class)
                     (funcall fn class))
                 (cl-inst-class-names inst))))


;;; occ-cl-utils.el ends here
