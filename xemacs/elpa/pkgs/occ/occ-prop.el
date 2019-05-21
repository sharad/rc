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
(require 'occ-prop)


(defun occ-readprop-props ()
  (cl-method-param-case
   '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))))

(defun occ-get-property-props ()
  (cl-method-param-case
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))))

;; (occ-readprop-props)
;; (occ-get-property-props)
(cl-defgeneric occ-match-prop-method-args (ctx)
  "occ-match-prop-method-args")

(cl-defmethod occ-match-prop-method-args ((ctx occ-ctx))
  (cl-method-sigs-matched-arg
   '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))
   ctx))


(defun occ-org-set-property (prop value)
  (lotus-org-with-safe-modification
    (org-set-property prop value)))


(cl-defgeneric occ-rank-with (obj
                              ctx)
  "occ-rank-with")

(cl-defgeneric occ-rank (obj)
  "occ-rank")

(cl-defgeneric occ-rankprop (obj
                             prop)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  "occ-rankprop")


(cl-defgeneric occ-rankprop-with (obj
                                  ctx
                                  prop)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  "occ-rankprop-with")


(cl-defmethod occ-rankprop (obj
                            prop)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  0)

;; ISSUE? should it return rank or occ-ctxual-tsk
(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx))
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk=%s ctx=%s)" tsk ctx)
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot) ;;TODO: check if method exist or not, or use some default method.
                             (occ-rank (cons slot obj) ctx))
                         (occ-class-slots obj)))))
    rank))

(cl-defmethod occ-rank ((tsk occ-obj-ctx-tsk))
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk=%s ctx=%s)" tsk ctx)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-rank-with tsk ctx)))

(cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-rankprop-with tsk ctx prop)))


(cl-defgeneric occ-writeprop-with (obj
                                   ctx
                                   prop
                                   value)
  "occ-writeprop-with")
(cl-defgeneric occ-readprop-with (obj
                                  ctx
                                  prop)
  "occ-readprop-with")
(cl-defgeneric occ-editprop-with (obj
                                  prop
                                  ctx)
  "occ-editprop-with")


(cl-defmethod occ-writeprop-with ((obj occ-tsk)
                                  (ctx occ-ctx)
                                  (prop symbol)
                                  value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (if value
      (progn
        (unless (org-get-property-block)
          ;;create property drawer
          (occ-debug :debug "occ-writeprop: property block not exist so creating it.")
          (let* ((range (org-get-property-block (point) 'force))
                 (start (when (consp range) (1- (car range)))))
            (when (numberp start)
              (goto-char start))))
        (occ-debug :debug
                   "occ-writeprop: adding prop: %s value: %s using (org-set-property)." prop value)
        (occ-org-set-property (symbol-name prop) value))
    (error "occ-writeprop value is nil")))
(cl-defmethod occ-readprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (occ-readprop prop obj ctx))
(cl-defmethod occ-editprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop symbol))
  (let ((value (occ-readprop prop obj ctx)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop-with prop obj ctx value)))


(cl-defgeneric occ-writeprop (obj
                              prop
                              value)
  "occ-writeprop")
(cl-defgeneric occ-readprop (obj
                             prop)
  "occ-readprop")
(cl-defgeneric occ-editprop (obj
                             prop)
  "occ-editprop")


(cl-defmethod occ-writeprop ((obj occ-obj-ctx-tsk)
                             (prop symbol)
                             value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-writeprop-with prop tsk ctx value)))
(cl-defmethod occ-readprop ((obj occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-readprop-with prop tsk ctx)))
(cl-defmethod occ-editprop ((obj occ-obj-ctx-tsk)
                            (prop symbol))
  (let ((value (occ-readprop prop obj)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop prop obj value)))

;;; occ-prop.el ends here
