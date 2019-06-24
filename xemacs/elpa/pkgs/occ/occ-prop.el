;;; occ-prop.el --- occ properties methods           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: sharad <sh4r4d _>
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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

;;; Code:

(provide 'occ-prop)


(require 'dash)
(require 'org-misc-utils-lotus)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-rank)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


(defun occ-readprop-props () ;;TODO: check about them
  (cl-method-param-case
   ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-readprop-elem-from-user-with (`(occ-tsk occ-ctx (eql ,val)) val))))

;; (defun occ-get-property-props () ;;TODO: check about them
;;   (cl-method-param-case
;;    '(occ-get-property  (`(occ-ctx (eql ,val)) val))))


(cl-defgeneric occ-match-prop-method-args (obj)
  "occ-match-prop-method-args")

;; (occ-match-prop-method-args (occ-make-ctx-at-point))

(cl-defmethod occ-match-prop-method-args ((obj occ-tsk))
  (cl-method-param-case '(occ-readprop-elem-from-user (`(occ-tsk (eql ,val)) val))))

;; (cl-defmethod occ-match-prop-method-args-with ((obj occ-tsk)
;;                                                (ctx occ-ctx))
;;   (cl-method-sigs-matched-arg
;;    ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
;;    '(occ-readprop-elem-from-user-with (`(occ-tsk occ-ctx (eql ,val)) val))
;;    '(occ-get-property  (`(occ-ctx (eql ,val)) val))
;;    ctx))

(cl-defmethod occ-match-prop-method-args ((obj occ-obj-ctx-tsk))
  (cl-method-sigs-matched-arg
   ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-readprop-elem-from-user-with (`(occ-obj-ctx-tsk (eql ,val)) val))
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))
   (occ-obj-ctx ctx)))


(cl-defgeneric occ-properties-to-edit (obj)
  "occ-properties-to-edit")

(cl-defgeneric occ-properties-to-edit-with (obj
                                            ctx)
  "occ-properties-to-edit-with")

(cl-defgeneric occ-properties-to-calculate-rank (obj)
  "occ-properties-to-calculate-rank")

;; (cl-defgeneric occ-properties-to-calculate-rank-with (obj
;;                                                       ctx)
;;   "occ-properties-to-calculate-rank-with")


(cl-defmethod occ-properties-to-edit ((obj occ-tsk))
  (occ-match-prop-method-args obj))

;; (cl-defmethod occ-properties-to-edit-with ((obj occ-tsk)
;;                                            (ctx occ-ctx))
;;   (occ-match-prop-method-args-with obj ctx))

;; (cl-defmethod occ-properties-to-edit ((obj occ-obj-ctx-tsk))
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;    (occ-match-prop-method-args-with tsk ctx)))

(cl-defmethod occ-properties-to-edit ((obj occ-obj-ctx-tsk))
  (occ-match-prop-method-args-with obj))


(cl-defmethod occ-properties-to-calculate-rank ((obj occ-tsk))
  (let ((class 'occ-ctx))
    (cl-method-param-case '(occ-rankprop (`(occ-tsk (eql ,val)) val)))))

;; (cl-defmethod occ-properties-to-calculate-rank-with ((obj occ-tsk)
;;                                                      (ctx occ-ctx))
;;   (let ((class 'occ-ctx))
;;     (cl-method-param-case '(occ-rankprop-with (`(occ-tsk occ-ctx (eql ,val)) val)))))

;; (cl-defmethod occ-properties-to-calculate-rank ((obj occ-obj-ctx-tsk))
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-properties-to-calculate-rank-with tsk ctx)))


;; NOTE:
;; (cl-defmethod occ-properties-to-calculate-rank ((obj occ-obj-ctx-tsk))
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (let ((class 'occ-ctx))
;;       (cl-method-param-case '(occ-rankprop-with (`(occ-tsk occ-ctx (eql ,val)) val))))))

(cl-defmethod occ-properties-to-calculate-rank ((obj occ-obj-ctx-tsk))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((class 'occ-ctx))
      (cl-method-param-case '(occ-rankprop (`(occ-obj-ctx-tsk (eql ,val)) val))))))

;; (let ((class 'occ-tsk))
;;   (cl-method-param-case `'(occ-rankprop (`(class (eql ,val)) val))))

;; (let ((class 'occ-tsk))
;;   (cl-method-param-case `'(occ-rankprop (`(,class (eql ,,'val)) val))))

;; (let ((class 'occ-tsk))
;;   `'(occ-rankprop  (,``(,,class (eql ,val)) val)))

;; (let ((class 'occ-tsk))
;;   (cl-method-param-case `'(occ-rankprop  (,`(,class (eql ,',val)) val))))

;; (cl-method-param-case '(occ-rankprop (`(occ-tsk (eql ,val)) val)))


;; (let ((class 'occ-tsk))
;;   (cl-method-param-case `'(occ-rankprop  (,``(,,class (eql ,val)) val))))


(defun occ-org-entry-get (pom
                          prop)
  (lotus-org-with-safe-modification
    (org-entry-get pom
                   prop)))

(defun occ-org-entry-put (pom
                          prop
                          value)
  (lotus-org-with-safe-modification
    (org-entry-put pom
                   prop
                   value)))

(defun occ-org-entry-get-multivalued-property (pom
                                               prop)
  (lotus-org-with-safe-modification
    (org-entry-get-multivalued-property pom prop)))

(defun occ-org-entry-put-multivalued-property (pom
                                               prop
                                               values)
  (lotus-org-with-safe-modification
    (apply #'org-entry-put-multivalued-property
           pom prop values)))

(defun occ-org-entry-add-to-multivalued-property (pom
                                                  prop
                                                  value)
  (lotus-org-with-safe-modification
    (org-entry-add-to-multivalued-property pom
                                           prop
                                           value)
    t))

(defun occ-org-entry-remove-from-multivalued-property (pom
                                                       prop
                                                       value)
  (lotus-org-with-safe-modification
    (org-entry-remove-from-multivalued-property pom
                                                prop
                                                value)
    t))

(defun occ-org-entry-member-in-multivalued-property (pom
                                                     prop
                                                     values)
  (lotus-org-with-safe-modification
    (org-entry-member-in-multivalued-property pom
                                              prop
                                              values)))


(cl-defmethod occ-prop-is-list ((prop symbol))
  "Method tell property represent list or not."
  ;; 'list
  ;; (error "Implement method occ-prop-is-list for prop %s" prop)
  (occ-debug :debug "occ-prop-is-list: no method for prop %s using default." prop)
  nil)

(cl-defmethod occ-edit-operation-validate ((prop symbol)
                                           operation)
  (if (occ-prop-is-list prop)
      (memq operation '(mget mput madd mremove member))
    (memq operation '(get put))))

(cl-defgeneric occ-org-operate-property (pom
                                         prop
                                         operation
                                         values)
  "occ-org-operate-property")

(cl-defmethod occ-org-operate-property ((pom marker)
                                        (prop string)
                                        operation
                                        values)
  ;; (unless (occ-edit-operation-validate prop operation)
  ;;   (error "occ-org-operate-property: operation %s is not allowed for prop %s" operation prop))
  (case operation
    ((get)     (list (occ-org-entry-get pom prop)))
    ((put)     (occ-org-entry-put pom prop (car values)))
    ((mget)    (occ-org-entry-get-multivalued-property pom prop))
    ((mput)    (occ-org-entry-put-multivalued-property pom prop values))
    ((madd)    (occ-org-entry-add-to-multivalued-property pom prop (car values)))
    ((mremove) (occ-org-entry-remove-from-multivalued-property pom prop (car values)))
    ((member)  (occ-org-entry-member-in-from-multivalued-property pom prop (car values)))))

(cl-defmethod occ-org-operate-property-at-point ((mrk marker)
                                                 (prop symbol)
                                                 operation
                                                 values)
  (unless (occ-edit-operation-validate prop operation)
    (error "occ-org-operate-property: operation %s is not allowed for prop %s" operation prop))
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug :debug "occ-org-operate-property-at-point: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (car range)))))
        (if (and range start)
            (when (numberp start)
              (goto-char start))
          (error "occ-org-operate-property-at-point: not able to create property block to add property %s: %s"
                 prop values))))

    (if (org-get-property-block)
        (progn
          (occ-debug :debug
                     "occ-org-operate-property-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop values)
          (let ((retval (occ-org-operate-property mrk
                                                  (symbol-name prop)
                                                  operation
                                                  values)))
            (occ-debug :debug "occ-org-operate-property: (occ-org-operate-property mrk) returned %s" retval)
            retval))
        (error "occ-org-operate-property-at-point: can not get property block to add property %s: %s"
               prop values))))


(cl-defmethod occ-prop-elem-to-org ((prop symbol)
                                    value)
  "Method convert value VALUE of property PROP from occ to org string representation."
  ;; (error "Implement method occ-prop-elem-to-org for prop %s" prop)
  (occ-debug :debug "occ-prop-elem-to-org: no method for prop %s using default." prop)
  value)
(cl-defmethod occ-prop-elem-from-org ((prop symbol)
                                      value)
  "Method convert value VALUE of property PROP from org string to occ representation."
  ;; (error "Implement method occ-prop-elem-from-org for prop %s" prop)
  (occ-debug :debug
             "occ-prop-elem-from-org: no method for prop %s using default." prop)
  value)


;; (cl-defgeneric occ-operate-obj-property-with (obj
;;                                               ctx
;;                                               prop
;;                                               operation
;;                                               values)
;;   "occ-operate-obj-property-with")

(cl-defgeneric occ-operate-obj-property (obj
                                         prop
                                         operation
                                         values)
  "occ-operate-obj-property-with")

(cl-defmethod occ-operate-obj-property ((obj occ-tsk)
                                        (prop symbol)
                                        operation
                                        values)
  (let ((values
         (mapcar #'(lambda (v)
                     (occ-prop-elem-from-org prop v))
                 values)))
    (occ-debug :debug "occ-operate-obj-property-with: operation %s values %s"
                      operation values)
    (case operation
      ((get)     (list (occ-org-entry-get nil prop)))
      ((put)     (occ-set-property obj prop (car values)))
      ((mget)    (occ-org-entry-get-multivalued-property nil prop))
      ((mput)    (occ-set-property obj prop values))
      ((madd)    (occ-set-property obj prop (nconc
                                             (occ-get-property obj prop)
                                             (list (car values)))))
      ((mremove) (occ-set-property obj prop (remove
                                             (car values)
                                             (occ-get-property obj prop))))
      ((member)  (occ-org-entry-member-in-from-multivalued-property nil prop (car values))))))

;; (cl-defmethod occ-operate-obj-property-with ((obj occ-tsk)
;;                                              (ctx occ-ctx)
;;                                              (prop symbol)
;;                                              operation
;;                                              values)
;;   (occ-operate-obj-property obj prop operation values))

;; (cl-defmethod occ-operate-obj-property ((obj occ-obj-ctx-tsk)
;;                                         (prop symbol)
;;                                         operation
;;                                         values)
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-operate-obj-property-with tsk ctx
;;                                    prop operation values)))


(cl-defmethod occ-operate-obj-property ((obj occ-obj-ctx-tsk)
                                        (prop symbol)
                                        operation
                                        values)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-operate-obj-property tsk prop operation values)))


;; (cl-defmethod occ-readprop-elem-from-user ((obj occ-tsk)
;;                                            (prop symbol))
;;   "Read value of element of list for property PROP from user for OCC-TSK OBJ."
;;   (error "Implement method occ-readprop-elem-from-user for prop %s" prop))

;; (cl-defmethod occ-readprop-from-user ((obj occ-tsk)
;;                                       (prop symbol))
;;   "Read value of element of list for property PROP from user for OCC-TSK OBJ."
;;   (error "Implement method occ-readprop-from-user for prop %s" prop))


(cl-defmethod occ-readprop-elem-from-user ((obj occ-obj-tsk)
                                           (prop symbol))
  "Read value of element of list for property PROP from user for OCC-TSK OBJ."
  (error "Implement method occ-readprop-elem-from-user for prop %s" prop))

(cl-defmethod occ-readprop-from-user ((obj occ-obj-tsk)
                                      (prop symbol))
  "Read value of element of list for property PROP from user for OCC-TSK OBJ."
  (error "Implement method occ-readprop-from-user for prop %s" prop))


;; (cl-defmethod occ-readprop-elem-from-user-with ((obj occ-tsk)
;;                                                 (ctx occ-ctx)
;;                                                 (prop symbol))
;;   "Read value of element of list for property PROP from user for OCC-TSK OBJ and OCC-CTX CTX."
;;   (occ-readprop-elem-from-user obj prop))

;; (cl-defmethod occ-readprop-from-user-with ((obj occ-tsk)
;;                                            (ctx occ-ctx)
;;                                            (prop symbol))
;;   "Read complete property list from user."
;;   (occ-readprop-from-user obj prop))


;; (cl-defmethod occ-readprop-elem-from-user ((obj occ-obj-ctx-tsk)
;;                                            (prop symbol))
;;   "Read value of element of list for property PROP from user for OCC-OBJ-CTX-TSK OBJ."
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-readprop-elem-from-user tsk prop)))

;; (cl-defmethod occ-readprop-from-user ((obj occ-obj-ctx-tsk)
;;                                       (prop symbol))
;;   "Read complete values list for property PROP from user for OCC-OBJ-CTX-TSK OBJ."
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-readprop-from-user tsk prop)))


(cl-defmethod occ-prop-get-operation ((prop symbol))
  "Return corresponding get option for property PROP based on type."
  (if (occ-prop-is-list prop)
      'mget
    'get))

(cl-defmethod occ-prop-put-operation ((prop symbol))
  "Return corresponding put option for property PROP based on type."
  (if (occ-prop-is-list prop)
      'mput
    'put))

(cl-defmethod occ-prop-add-operation ((prop symbol))
  "Return corresponding put option for property PROP based on type."
  (if (occ-prop-is-list prop)
      'madd
    'put))

(cl-defmethod occ-prop-remove-operation ((prop symbol))
  "Return corresponding put option for property PROP based on type."
  (if (occ-prop-is-list prop)
      'mremove
    'remove))


(cl-defmethod occ-rereadprop-value ((prop symbol)
                                    value)
  "Read org string property PROP to occ representation."
  (cl-assert (not (consp value)))
  (if (occ-prop-is-list prop)
      (let* ((values (and value (split-string value))))
        (mapcar #'(lambda (v) (occ-prop-elem-from-org prop v))
                (mapcar #'org-entry-restore-space values)))
    (occ-prop-elem-from-org prop value)))

(cl-defmethod occ-reread-props ((obj occ-tsk))
  "Read all org string properties for task TSK to occ representation."
  (let ((props-by-is-list (cl-method-param-case
                           '(occ-prop-is-list (`((eql ,val)) val))))
        (props-by-converter (cl-method-param-case
                             '(occ-prop-elem-from-org (`((eql ,val) t) val)))))
    (let ((props (-union props-by-is-list props-by-converter))) ;dash
      (dolist (p props)
        (occ-set-property obj p
                          (occ-rereadprop-value p (occ-get-property obj p)))))))


;; (cl-defmethod occ-readprop-org-with ((obj occ-tsk)
;;                                      (ctx occ-ctx)
;;                                      (prop symbol))
;;   "Read property PROP of OBJ-TSK OBJ and OBJ-CTX CTX from its corresponding org file entry."
;;   (let* ((mrk    (or (occ-obj-marker obj) (point)))
;;          (values (occ-org-operate-property-at-point mrk
;;                                                     prop
;;                                                     (occ-prop-get-operation prop))))
;;     (mapcar #'(lambda (v)
;;                 (occ-prop-elem-from-org prop v))
;;             values)))

;; (cl-defmethod occ-writeprop-org-with ((obj occ-tsk)
;;                                       (ctx occ-ctx)
;;                                       (prop symbol))
;;   "Write property PROP of OBJ-TSK TSK and OBJ-CTX CTXfrom its corresponding org file entry."
;;   (let* ((values (occ-get-property obj prop))
;;          (values (if (consp values) values (list values)))
;;          (values (mapcar #'(lambda (v)
;;                              (occ-prop-elem-to-org prop v))
;;                          values)))
;;     (occ-org-operate-property-at-point (point)
;;                                        prop
;;                                        (occ-prop-put-operation prop)
;;                                        values)))


;; (cl-defmethod occ-readprop-org ((obj occ-obj-ctx-tsk)
;;                                 (prop symbol))
;;   "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-readprop-org-with tsk ctx prop)))

;; (cl-defmethod occ-writeprop-org ((obj occ-obj-ctx-tsk)
;;                                  (prop symbol))
;;   "Write property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-writeprop-org-with tsk ctx prop)))

(cl-defmethod occ-readprop-org ((obj occ-obj-ctx-tsk)
                                (prop symbol))
  "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((mrk    (or (occ-obj-marker tsk) (point)))
           (values (occ-org-operate-property-at-point mrk
                                                      prop
                                                      (occ-prop-get-operation prop))))
      (mapcar #'(lambda (v)
                  (occ-prop-elem-from-org prop v))
              values))))

(cl-defmethod occ-writeprop-org ((obj occ-obj-ctx-tsk)
                                 (prop symbol))
  "Write property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((values (occ-get-property tsk prop))
           (values (if (consp values) values (list values)))
           (values (mapcar #'(lambda (v)
                               (occ-prop-elem-to-org prop v))
                           values)))
      (occ-org-operate-property-at-point (point)
                                         prop
                                         (occ-prop-put-operation prop)
                                         values))))


(cl-defgeneric occ-select-prop-list-operation (prop)
  "occ-select-prop-list-operation")

;; TODO: Add log not on property editing.
(cl-defmethod occ-select-prop-list-operation ((prop symbol))
  (if (occ-prop-is-list prop)
      (let ((actions '(("add" . madd)
                       ("del" . mremove)
                       ("put" . mput))))
        (cdr
         (assoc
          (completing-read (format "%s action: " prop) actions)
          actions)))
    'put))


;; (cl-defgeneric occ-editprop-with ((obj occ-tsk)
;;                                   (ctx occ-ctx)
;;                                   (prop symbol)
;;                                   &optional
;;                                   operation
;;                                   value)
;;   "occ-editprop-with")

(cl-defgeneric occ-editprop ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop symbol)
                             &optional
                             operation
                             value)
  "occ-editprop")

(cl-defmethod occ-editprop ((obj occ-tsk)
                            (prop symbol)
                            &optional
                            operation
                            value)
  (occ-debug :debug
             "occ-editprop: prop: %s, value: %s" prop value)
  (let ((mrk (occ-obj-marker obj)))
    (let ((operation  (or operation (occ-select-prop-list-operation prop)))
          (prop-value (or value     (occ-readprop-elem-from-user obj prop))))
      (let ((retval
             (occ-org-operate-property-at-point mrk
                                                prop
                                                operation
                                                (list prop-value))))
        (occ-debug :debug "occ-editprop: (occ-org-operate-property-at-point mrk) returnd %s" retval)
        (when retval
          (occ-operate-obj-property obj
                                    prop
                                    operation
                                    (list prop-value)))))))

;; (cl-defmethod occ-editprop-with ((obj occ-tsk)
;;                                  (ctx occ-ctx)
;;                                  (prop symbol)
;;                                  &optional
;;                                  operation
;;                                  value)
;;   (occ-debug :debug
;;              "occ-editprop-with: prop: %s, value: %s" prop value)
;;   (let ((mrk (occ-obj-marker obj)))
;;     (let ((operation  (or operation (occ-select-prop-list-operation prop)))
;;           (prop-value (or value     (occ-readprop-elem-from-user-with obj ctx prop))))
;;       (let ((retval
;;              (occ-org-operate-property-at-point mrk
;;                                                 prop
;;                                                 operation
;;                                                 (list prop-value))))
;;         (occ-debug :debug "occ-editprop-with: (occ-org-operate-property-at-point mrk) returnd %s" retval)
;;         (when retval
;;           (occ-operate-obj-property-with obj
;;                                          ctx
;;                                          prop
;;                                          operation
;;                                          (list prop-value)))))))

;; (cl-defmethod occ-editprop ((obj occ-obj-ctx-tsk)
;;                             (prop symbol)
;;                             &optional
;;                             operation
;;                             value)
;;   (occ-debug :debug
;;              "occ-editprop: prop: %s, value: %s" prop value)
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-editprop-with tsk ctx prop operation value)))

(cl-defmethod occ-editprop ((obj occ-obj-ctx-tsk)
                            (prop symbol)
                            &optional
                            operation
                            value)
  (occ-debug :debug
             "occ-editprop: prop: %s, value: %s" prop value)
  (let ((mrk (occ-obj-marker obj)))
    (let ((operation  (or operation (occ-select-prop-list-operation prop)))
          (prop-value (or value     (occ-readprop-elem-from-user obj prop))))
      (let ((retval
             (occ-org-operate-property-at-point mrk
                                                prop
                                                operation
                                                (list prop-value))))
        (occ-debug :debug "occ-editprop-with: (occ-org-operate-property-at-point mrk) returnd %s" retval)
        (when retval
          (occ-operate-obj-property obj
                                    prop
                                    operation
                                    (list prop-value)))))))


;; (cl-defgeneric occ-generate-operation-method-with (obj
;;                                                    ctx
;;                                                    prop
;;                                                    operation
;;                                                    value)
;;   "occ-generate-operation-method-with")

(cl-defgeneric occ-generate-operation-method (obj
                                              ctx
                                              prop
                                              operation
                                              value)
  "occ-generated-operation-method")

(cl-defmethod occ-generate-operation-method ((obj occ-tsk)
                                             (prop symbol)
                                             operation
                                             value)
  #'(lambda (obj)
      (occ-editprop obj
                    prop
                    operation
                    value)))

;; (cl-defmethod occ-generate-operation-method-with ((obj occ-tsk)
;;                                                   (ctx occ-ctx)
;;                                                   (prop symbol)
;;                                                   operation
;;                                                   value)
;;   #'(lambda (obj ctx)
;;       (occ-editprop-with obj ctx
;;                          prop
;;                          operation
;;                          value)))

(cl-defmethod occ-generate-operation-method ((obj occ-obj-ctx-tsk)
                                             (prop symbol)
                                             operation
                                             value)
  #'(lambda (obj)
      (occ-editprop obj
                    prop
                    operation
                    value)))


;; (cl-defgeneric occ-generate-prompt-operation-method-with (obj
;;                                                           ctx
;;                                                           prop
;;                                                           operation
;;                                                           value)
;;   "occ-generate-prompt-operation-method-with")

(cl-defgeneric occ-generate-prompt-operation-method (obj
                                                     prop
                                                     operation
                                                     value)
  "occ-generate-prompt-operation-method")

(cl-defmethod occ-generate-prompt-operation-method ((obj occ-tsk)
                                                    (prop symbol)
                                                    operation
                                                    value)
  (let ((value (occ-prop-elem-to-org prop value)))
    (cons
     (format "%s %s %s %s"
             (if (memq operation '(madd add))
                 (if (occ-prop-is-list prop) "Add" "Replace")
               "Remove")
             (if (occ-prop-is-list prop) "in" "from")
             value prop)
     (occ-generate-operation-method obj prop
                                    (occ-prop-add-operation prop)
                                    value))))

;; (cl-defmethod occ-generate-prompt-operation-method-with ((obj occ-tsk)
;;                                                          (ctx occ-ctx)
;;                                                          (prop symbol)
;;                                                          operation
;;                                                          value)
;;   (let ((value (occ-prop-elem-to-org prop value)))
;;     (cons
;;      (format "%s %s %s %s"
;;              (if (memq operation '(madd add))
;;                  (if (occ-prop-is-list prop) "Add" "Replace")
;;                "Remove")
;;              (if (occ-prop-is-list prop) "in" "from")
;;              value prop)
;;      (occ-generate-operation-method-with obj ctx prop
;;                                          (occ-prop-add-operation prop)
;;                                          value))))

;; (cl-defmethod occ-generate-prompt-operation-method ((obj occ-obj-ctx-tsk)
;;                                                     (prop symbol)
;;                                                     operation
;;                                                     value)
;;   (let ((value (occ-prop-elem-to-org prop value)))
;;     (cons
;;      (format "%s %s %s %s"
;;              (if (memq operation '(madd add))
;;                  (if (occ-prop-is-list prop) "Add" "Replace")
;;                "Remove")
;;              (if (occ-prop-is-list prop) "in" "from")
;;              value prop)
;;      (occ-generate-operation-method obj prop
;;                                     (occ-prop-add-operation prop)
;;                                     value))))

(cl-defmethod occ-generate-prompt-operation-method ((obj occ-obj-ctx-tsk)
                                                    (prop symbol)
                                                    operation
                                                    value)
  (let ((value (occ-prop-elem-to-org prop value)))
    (cons
     (format "%s %s %s %s"
             (if (memq operation '(madd add))
                 (if (occ-prop-is-list prop) "Add" "Replace")
               "Remove")
             (if (occ-prop-is-list prop) "in" "from")
             value prop)
     (occ-generate-operation-method obj prop
                                    (occ-prop-add-operation prop)
                                    value))))


;; (defmethod occ-generate-operation-for-add (obj occ-obj-tsk)
;;   (mapcar
;;    #'(lambda (prop)
;;        (occ-generate-prompt-operation-method obj prop
;;                                              (occ-prop-add-operation prop)
;;                                              (occ-get-property obj prop)))
;;    (occ-properties-to-edit obj)))

;; (defmethod occ-generate-operation-for-remove (obj occ-obj-tsk)
;;   (mapcar
;;    #'(lambda (prop)
;;        (occ-generate-prompt-operation-method obj prop
;;                                              (occ-prop-remove-operation prop)
;;                                              (occ-get-property obj prop)))
;;    (occ-properties-to-edit obj)))


(cl-defgeneric occ-property-membership-p (obj
                                          prop
                                          value)
  "occ-property-membership-p")

(cl-defmethod occ-property-membership-p ((obj occ-tsk)
                                         (prop symbol)
                                         value)
  (if (occ-prop-is-list prop)
      (memq value (occ-get-property obj prop))
    (equal value (occ-get-property obj prop))))

(cl-defmethod occ-property-membership-p ((obj occ-obj-ctx-tsk)
                                         (prop symbol)
                                         value)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-property-membership-p tsk prop value)))


(cl-defmethod occ-generate-add-method-if-not ((obj occ-obj-ctx-tsk)
                                              (prop symbol)
                                              operation
                                              value)
  (unless (occ-property-membership-p obj prop value)
    (occ-generate-prompt-operation-method obj prop
                                          (occ-prop-add-operation prop)
                                          value)))

(cl-defmethod occ-generate-remove-method-if ((obj occ-obj-ctx-tsk)
                                             (prop symbol)
                                             operation
                                             value)
  (when (occ-property-membership-p obj prop value)
    (occ-generate-prompt-operation-method obj prop
                                          (occ-prop-remove-operation prop)
                                          value)))



(cl-defmethod occ-generate-operation-for-add ((obj occ-obj-ctx-tsk))
  (let ((props (occ-properties-to-edit obj))
        (gen-add-action
         #'(lambda (prop)
             (let ((value (occ-prop-elem-from-org prop
                                                  (occ-get-property (occ-obj-ctx obj) prop))))
               (occ-generate-add-method-if-not obj prop
                                               (occ-prop-add-operation prop)
                                               value))))))
  (remove nil
          (mapcar #'(lambda (prop) (gen-add-action prop))
                  props)))

(cl-defmethod occ-generate-operation-for-remove ((obj occ-obj-ctx-tsk))
  (let ((props (occ-properties-to-edit obj))
        (gen-remove-action
         #'(lambda (prop)
             (let ((value (occ-prop-elem-from-org prop
                                                 (occ-get-property (occ-obj-ctx obj) prop))))
               (occ-generate-remove-method-if obj prop
                                              (occ-prop-remove-operation prop)
                                              value))))))
  (remove nil
          (mapcar #'(lambda (prop) (gen-remove-action prop))
                  props)))


(cl-defmethod occ-generate-operation-for-add-remove ((obj occ-obj-ctx-tsk))
  (let ((props (occ-properties-to-edit obj))
        (gen-add-action
         #'(lambda (prop)
             (let ((value (occ-prop-elem-from-org prop
                                                  (occ-get-property (occ-obj-ctx obj) prop))))
               (occ-generate-add-method-if-not obj prop
                                               (occ-prop-add-operation prop)
                                               value))))
        (gen-remove-action
         #'(lambda (prop)
             (let ((value (occ-prop-elem-from-org prop
                                                  (occ-get-property (occ-obj-ctx obj) prop))))
               (occ-generate-remove-method-if obj prop
                                              (occ-prop-remove-operation prop)
                                              value)))))
    (remove nil
            (apply #'append
                   (mapcar
                    #'(lambda (prop)
                        (list (funcall gen-add-action prop)
                              (funcall gen-remove-action prop)))
                    props)))))


(cl-defmethod occ-print-rank ((obj occ-tsk))
  (occ-message "Rank for %s is %d"
               (occ-format obj 'capitalize)
               (occ-rank obj)))

(cl-defmethod occ-print-rank ((obj occ-obj-ctx-tsk))
  (occ-message "Rank for %s is %d"
               (occ-format obj 'capitalize)
               (occ-rank obj)))


(cl-defmethod occ-increase-timeout ((obj occ-obj-ctx-tsk))
  (occ-generate-prompt-operation-method obj 'timeout add 100))

;;; occprop.el ends here
