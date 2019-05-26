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

(defun occ-get-from-org ())
(defun occ-put-to-org ())
(defun occ-readprop ())
(defun occ-editprop ())



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


;; (org-entry-get-multivalued-property (point) "ZEGMA")
;; (org-entry-put-multivalued-property (point) "ZEGMA" "T1" "T2")
;; (org-entry-add-to-multivalued-property (point) "ZEGMA" "TEST1")
;; (org-entry-get (point) "ZEGMA")
;; (org-entry-set (point) "ZEGMA" "VALUE")

(defun occ-org-set-property (prop
                             value)
  (lotus-org-with-safe-modification
    (org-set-property prop value)))


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


(defun occ-org-operate-property (op
                                 prop
                                 values)
  (case op
    ((get)     (occ-org-entry-get nil prop))
    ((put)     (occ-org-entry-put nil prop (car values)))
    ((mget)    (occ-org-entry-get-multivalued-property nil prop))
    ((mput)    (occ-org-entry-put-multivalued-property nil prop values))
    ((madd)    (occ-org-entry-add-to-multivalued-property nil prop (car values)))
    ((mremove) (occ-org-entry-remove-from-multivalued-property nil prop (car values)))
    ((member)  (occ-org-entry-member-in-from-multivalued-property nil prop (car values)))))

(cl-defmethod occ-org-operate-property-at-point ((mrk marker)
                                                 op
                                                 prop
                                                 &rest
                                                 values)
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug :debug "occ-org-set-property-at-point: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (car range)))))
        (if (and range start)
            (when (numberp start)
              (goto-char start))
          (error "occ-org-set-property-at-point: not able to create property block to add property %s: %s"
                 prop value))))

    (if (org-get-property-block)
        (progn
          (occ-debug :debug
                     "occ-org-set-property-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop value)
          (occ-org-operate-property opt (symbol-name prop) values))
        (error "occ-org-set-property-at-point: can not get property block to add property %s: %s"
               prop value))))


(cl-defmethod occ-org-set-property-at-point ((mrk marker)
                                             prop
                                             value)
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug :debug "occ-org-set-property-at-point: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (car range)))))
        (if (and range start)
            (when (numberp start)
              (goto-char start))
          (error "occ-org-set-property-at-point: not able to create property block to add property %s: %s"
                 prop value))))

    (if (org-get-property-block)
        (progn
          (occ-debug :debug
                     "occ-org-set-property-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop value)
          (occ-org-set-property (symbol-name prop) value))
        (error "occ-org-set-property-at-point: can not get property block to add property %s: %s"
               prop value))))


(cl-defmethod occ-writeprop-with ((obj  occ-tsk)
                                  (ctx  occ-ctx)
                                  (prop symbol)
                                  value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (if value
      (let ((mrk (occ-obj-marker obj)))
        (if (occ-valid-marker mrk)
            (when (occ-org-set-property-at-point mrk prop value)
              (occ-set-property obj prop value))
          (error "%s marker %s is not valid."
                 (occ-format obj 'capitalize)
                 mrk)))
    (error "occ-writeprop value is nil")))
(cl-defmethod occ-readprop-with ((obj  occ-tsk)
                                 (ctx  occ-ctx)
                                 (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (occ-readprop obj
                ctx
                prop))
(cl-defmethod occ-editprop-with ((obj  occ-tsk)
                                 (ctx  occ-ctx)
                                 (prop symbol))
  (let ((value (occ-readprop-with obj
                                  ctx
                                  prop)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop-with obj
                        ctx
                        prop
                        value)))


(defun occ-select-prop-edit-operation ()
  (let ((actions '(("add" . add)
                   ("del" . del)
                   ("put" . put))))
    (cdr
     (assoc (completing-read "action: " actions) actions))))

(cl-defmethod occ-writeprop ((obj  occ-obj-ctx-tsk)
                             (prop symbol)
                             value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-writeprop-with tsk ctx
                        prop value)))
(cl-defmethod occ-readprop ((obj occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-readprop-with tsk ctx prop)))
(cl-defmethod occ-editprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (error "can not call directly."))
(cl-defmethod occ-single-editprop ((obj  occ-obj-ctx-tsk)
                                   (prop symbol))
  (let ((value (occ-readprop obj prop)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop obj
                   prop value)))
(cl-defmethod occ-multi-editprop ((obj  occ-obj-ctx-tsk)
                                  (prop symbol))
  (let ((values (occ-org-entry-get-multivalued-property prop))
        (op     (occ-select-prop-edit-operation))
        (mrk    (occ-marker obj)))
    (cond
     ((eq op 'add)
      (occ-org-operate-property-at-point mrk 'madd prop
                                         (list (read-from-minibuffer (format "%s: " prop)))))
     ((eq op 'del)
      (occ-org-operate-property-at-point mrk 'mremove prop
                                         (list (completing-read (format "%s: " prop) values nil t))))
     ((eq op 'put)
      (occ-org-operate-property-at-point mrk 'mput prop
                                         (list (read-from-minibuffer (format "%s: " prop))))))))


(defun occ-edit-multi-valued-prop ())   ;add del clear put get

(defun occ-edit-single-valued-prop ())  ;overwite only



(cl-defmethod occ-print-rank ((obj occ-obj-ctx-tsk))
  (occ-message "Rank for %s is %d"
               (occ-format obj 'capitalize)
               (occ-rank obj)))


(defun zzz-select-list-operation ()
  (let ((actions '(("add" . add)
                   ("del" . del)
                   ("put" . put))))
    (cdr
     (assoc (completing-read "action: " actions) actions))))

(defun zzz-editprop (pom prop)
  (let ((values (org-entry-get-multivalued-property prop))
        (op (zzz-select-list-operation))))
  (cond
   ((eq op 'add)
    (org-entry-add-to-multivalued-property (point) prop (read-from-minibuffer (format "%s: " prop))))
   ((eq op 'del)
    (org-entry-remove-from-multivalued-property (point) prop (completing-read (format "%s: " prop) values nil t)))
   ((eq op 'add)
    (org-entry-put-multivalued-property (point) prop (read-from-minibuffer (format "%s: " prop))))))











;;; occ-prop.el ends here
