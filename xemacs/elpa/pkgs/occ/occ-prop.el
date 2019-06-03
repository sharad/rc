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


(require 'org-misc-utils-lotus)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-rank)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


(defun occ-readprop-props ()
  (cl-method-param-case
   ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-readprop-elem-from-user-with (`(occ-tsk occ-ctx (eql ,val)) val))))

(defun occ-get-property-props ()
  (cl-method-param-case
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))))

;; (occ-readprop-props)
;; (occ-get-property-props)
(cl-defgeneric occ-match-prop-method-args (ctx)
  "occ-match-prop-method-args")

(cl-defmethod occ-match-prop-method-args ((ctx occ-ctx))
  (cl-method-sigs-matched-arg
   ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-readprop-elem-from-user-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))
   ctx))


(defun occ-org-entry-get (pom prop)
  (lotus-org-with-safe-modification
    (org-entry-get pom prop)))

(defun occ-org-entry-put (pom prop value)
  (lotus-org-with-safe-modification
    (org-entry-put pom prop value)))

(defun occ-org-entry-get-multivalued-property (pom prop)
  (lotus-org-with-safe-modification
    (org-entry-get-multivalued-property pom prop)))

(defun occ-org-entry-put-multivalued-property (pom prop values)
  (lotus-org-with-safe-modification
    (apply #'org-entry-put-multivalued-property pom prop values)))

(defun occ-org-entry-add-to-multivalued-property (pom prop value)
  (lotus-org-with-safe-modification
    (org-entry-add-to-multivalued-property pom prop value)))

(defun occ-org-entry-remove-from-multivalued-property (pom prop value)
  (lotus-org-with-safe-modification
    (org-entry-remove-from-multivalued-property pom prop value)))

(defun occ-org-entry-member-in-multivalued-property (pom prop values)
  (lotus-org-with-safe-modification
    (org-entry-member-in-multivalued-property pom prop values)))


(defun occ-org-operate-property (prop
                                 op
                                 values)
  (case op
    ((get)     (list (occ-org-entry-get nil prop)))
    ((put)     (occ-org-entry-put nil prop (car values)))
    ((mget)    (occ-org-entry-get-multivalued-property nil prop))
    ((mput)    (occ-org-entry-put-multivalued-property nil prop values))
    ((madd)    (occ-org-entry-add-to-multivalued-property nil prop (car values)))
    ((mremove) (occ-org-entry-remove-from-multivalued-property nil prop (car values)))
    ((member)  (occ-org-entry-member-in-from-multivalued-property nil prop (car values)))))

(cl-defmethod occ-org-operate-property-at-point ((mrk marker)
                                                 (prop symbol)
                                                 op
                                                 values)
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
                 prop value))))

    (if (org-get-property-block)
        (progn
          (occ-debug :debug
                     "occ-org-operate-property-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop value)
          (occ-org-operate-property (symbol-name prop) opt values))
        (error "occ-org-operate-property-at-point: can not get property block to add property %s: %s"
               prop value))))



(defun occ-operate-obj-property-with ((obj  occ-tsk)
                                      (ctx  occ-ctx)
                                      (prop symbol)
                                      op
                                      values)
  (let ((values (mapcar #'(lambda (v) (occ-prop-elem-from-org prop v)) values)))
    (case op
      ((get)     (list (occ-org-entry-get nil prop)))
      ((put)     (occ-set-property obj prop (car values)))
      ((mget)    (occ-org-entry-get-multivalued-property nil prop))
      ((mput)    (occ-set-property obj prop values))
      ((madd)    (occ-set-property obj prop (cons (car values)
                                                  (occ-get-property obj prop))))
      ((mremove) (error "remove from obj prop"))
      ((member)  (occ-org-entry-member-in-from-multivalued-property nil prop (car values))))))


(cl-defmethod occ-prop-is-list ((prop symbol))
  ;; 'list
  (error "Implement method occ-prop-is-list for prop %s" prop))


(cl-defmethod occ-prop-elem-to-org   ((prop symbol) value)
  (error "Implement method occ-prop-elem-to-org for prop %s" prop))
(cl-defmethod occ-prop-elem-from-org ((prop symbol) value)
  (error "Implement method occ-prop-elem-from-org for prop %s" prop))


(cl-defmethod occ-readprop-elem-from-user-with ((obj  occ-tsk)
                                                (ctx  occ-ctx)
                                                (prop symbol))
  "readprop-elem-from-user for org"
  (error "Implement method occ-readprop-elem-from-user-with for prop %s" prop))
(cl-defmethod occ-readprop-from-user-with ((obj  occ-tsk)
                                           (ctx  occ-ctx)
                                           (prop symbol))
  "readprop-from-user for org"
  (error "Implement method occ-readprop-from-user-with for prop %s" prop))


(cl-defmethod occ-prop-get-op ((prop symbol))
  (if (occ-prop-is-list prop)
      'mget
    'get))

(cl-defmethod occ-prop-put-op ((prop symbol))
  (if (occ-prop-is-list prop)
      'mput
    'put))


(cl-defmethod occ-readprop-org-with ((obj  occ-tsk)
                                     (ctx  occ-ctx)
                                     (prop symbol))
  "readprop-org"
  (let ((values (occ-org-operate-property-at-point (point)
                                                   prop
                                                   (occ-prop-get-op prop))))
    (mapcar #'(lambda (v) (occ-prop-elem-from-org prop v)) values)))

(cl-defmethod occ-writeprop-org-with ((obj  occ-tsk)
                                      (ctx  occ-ctx)
                                      (prop symbol))
  "readprop-org"
  (let* ((values (occ-get-property obj prop))
         (values (if (consp values) values (list values)))
         (values (mapcar #'(lambda (v) (occ-prop-elem-to-org prop va)) values)))
    (occ-org-operate-property-at-point (point)
                                       prop
                                       (occ-prop-put-op prop)
                                       values)))

(cl-defmethod occ-readprop-org ((obj  occ-obj-ctx-tsk)
                                (prop symbol))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-readprop-org-with tsk ctx prop)))

(cl-defmethod occ-writeprop-org ((obj  occ-obj-ctx-tsk))
                                (prop symbol)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-writeprop-org-with tsk ctx prop)))


(defun occ-select-prop-list-operation ()
  (let ((actions '(("add" . madd)
                   ("del" . mremove)
                   ("put" . mput))))
    (cdr
     (assoc
      (completing-read "action: " actions)
      actions))))

(cl-defmethod occ-editprop-list-with ((obj  occ-tsk)
                                      (ctx  occ-ctx)
                                      (prop symbol))
  (let ((mrk (occ-obj-marker obj)))
    (let ((op (occ-select-prop-list-operation))
          (prop-value (occ-readprop-elem-from-user-with obj ctx prop)))
      (when (occ-org-operate-property-at-point mrk
                                               prop
                                               op
                                               (list prop-value))
        (occ-operate-obj-property-with obj ctx prop op (list prop-value))))))


(cl-defmethod occ-editprop-elem-with ((obj  occ-tsk)
                                      (ctx  occ-ctx)
                                      (prop symbol))
  (occ-debug :debug
             "occ-editprop-with: prop: %s, value: %s" prop value)
  (let ((prop-value (occ-readprop-from-user-with obj ctx prop)))
    (when (occ-org-operate-property-at-point mrk
                                             prop
                                             'put
                                             (list prop-value))
      (occ-operate-obj-property-with obj ctx prop 'put (list prop-value)))))


(cl-defmethod occ-editprop-with ((obj  occ-tsk)
                                 (ctx  occ-ctx)
                                 (prop symbol))
  (if (occ-prop-is-list prop)
      (occ-editprop-list-with obj ctx prop)
    (occ-editprop-elem-with obj ctx prop)))

(cl-defmethod occ-editprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug
             "occ-editprop: prop: %s, value: %s" prop value)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-editprop-with tsk ctx prop)))


(cl-defmethod occ-print-rank ((obj occ-obj-ctx-tsk))
  (occ-message "Rank for %s is %d"
               (occ-format obj 'capitalize)
               (occ-rank obj)))

;;; occprop.el ends here
