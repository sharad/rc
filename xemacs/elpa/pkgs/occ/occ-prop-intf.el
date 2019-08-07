;;; occ-prop-intf.el --- occ property interface      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <>
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

(provide 'occ-prop-intf)


(require 'occ-util-common)
(require 'occ-obj)


(defvar occ-prop-rank-hierarchy '(t))

(cl-defmethod occ-set-rank-hierarchy ((prop symbol)
                                      &key
                                      pos)

  (cond))

;; (cl-defmethod)

(cl-defgeneric occ-rankprop (obj
                             prop)
  "occ-rankprop")

(cl-defmethod occ-rankprop (obj
                            prop)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
  0)

(cl-defmethod occ-rankprop ((obj  occ-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)"
             obj
             prop)
  0)

(cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
  (occ-rankprop obj prop))


(cl-defmethod occ-list-p ((prop symbol))
  "Method tell property represent list or not."
  ;; 'list
  ;; (error "Implement method occ-list-p for prop %s" prop)
  (occ-debug :debug "occ-list-p: no method for prop %s using default." prop)
  nil)


;; (cl-defmethod occ-increase-p ((prop symbol))
;;   "Method tell property represent list or not."
;;   ;; 'list
;;   ;; (error "Implement method occ-list-p for prop %s" prop)
;;   (occ-debug :debug "occ-list-p: no method for prop %s using default." prop)
;;   nil)

;; (cl-defmethod occ-list-p ((prop symbol))
;;   "Method tell property represent list or not."
;;   ;; 'list
;;   ;; (error "Implement method occ-list-p for prop %s" prop)
;;   (occ-debug :debug "occ-list-p: no method for prop %s using default." prop)
;;   nil)



(cl-defmethod occ-prop-elem-to-org ((prop symbol)
                                    value)
  "Method convert value VALUE of property PROP from occ to org string representation."
  ;; (error "Implement method occ-prop-elem-to-org for prop %s" prop)
  (occ-debug :debug "occ-prop-elem-to-org: no method for prop %s using default." prop)
  value)
(cl-defmethod occ-prop-elem-from-org ((prop symbol)
                                      (value string))
  "Method convert value VALUE of property PROP from org string to occ representation."
  ;; (error "Implement method occ-prop-elem-from-org for prop %s" prop)
  (occ-debug :debug
             "occ-prop-elem-from-org: no method for prop %s using default." prop)
  value)


;; TODO: should not we make them to be converted to OCC value here.
(cl-defmethod occ-readprop-elem-from-user ((obj occ-obj-tsk)
                                           (prop symbol))
  "Read value of element of list for property PROP from user for OCC-TSK OBJ, must return ORG compatible value."
  (error "Implement method occ-readprop-elem-from-user for prop %s " prop))

(cl-defmethod occ-readprop-from-user ((obj occ-obj-tsk)
                                      (prop symbol))
  "Read value of element of list for property PROP from user for OCC-TSK OBJ, must return ORG compatible value."
  (error "Implement method occ-readprop-from-user for prop %s" prop))

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

(cl-defmethod occ-get-property ((obj occ-ctx)
                                (prop symbol))
  "must return occ compatible value."
  (error "must return occ compatible value."))

(cl-defmethod occ-format-prop ((obj occ-obj-tsk)
                               (prop symbol)
                               value)
  "Should return format printable value"
  value)


(cl-defgeneric occ-has-p (obj
                          prop
                          value)
  "occ-has-p")

(cl-defgeneric occ-operation (obj
                              operation
                              prop
                              values)
  "occ-operation")

(cl-defgeneric occ-require-p (obj
                              operation
                              prop
                              values)
  "occ-require-p")


;; (cl-defmethod occ-operation ((obj occ-obj-tsk)
;;                              (operation (eql XYZ))
;;                              (prop      (eql x))
;;                              values)
;;   ())


(cl-defgeneric occ-prop-default-value (obj
                                       prop
                                       operation)
  "occ-prop-default-value")

(cl-defmethod occ-prop-default-value ((obj occ-obj-tsk)
                                      (prop symbol)
                                      (operation symbol))
  nil)

(cl-defmethod occ-prop-default-value ((obj occ-obj-ctx-tsk)
                                      (prop symbol)
                                      (operation symbol))
  (occ-get-property (occ-obj-ctx obj) prop))


(cl-defgeneric occ-checkout-prop (obj
                                  prop)
  "Checkout property in case of force clock-in.")

(cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                 (prop symbol))
  "Checkout property in case of force clock-in."
  (error "Implement it for %s: Checkout property in case of force clock-in." prop))


;;; occ-prop-intf.el ends here
