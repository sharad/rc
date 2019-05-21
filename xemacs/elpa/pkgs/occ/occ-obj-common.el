;;; occ-obj-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(provide 'occ-obj-common)


(require 'occ-obj)


;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; "org tsks accss common api"
(cl-defmethod occ-get-property ((obj occ-obj)
                                (prop symbol))
  ;; mainly used by occ-tsk only.
  (if (memq prop (cl-class-slots (cl-classname obj)))
      (cl-get-field obj prop)
    (let ((key (sym2key prop)))
      (if key
       (plist-get
           (cl-obj-plist-value obj)
           (sym2key prop))
       (error "occ-get-property: Can not make keyword for `'%s'" prop)))))

(cl-defmethod occ-set-property ((obj occ-obj)
                                prop
                                val)
  ;; mainly used by occ-tsk only
  (if (memq prop (cl-class-slots (cl-classname obj)))
      (setf (cl-struct-slot-value (cl-classname obj) prop obj) val)
    (let ((key (sym2key prop)))
      (if key
          (plist-put
           (cl-struct-slot-value (cl-classname obj) 'plist obj) ;TODO ??? (cl-obj-plist-value obj)
           key val)
        (error "occ-set-property: Can not make keyword for `'%s'" prop)))))

(cl-defmethod occ-get-properties ((obj occ-obj)
                                  (props list))
  ;; mainly used by occ-tsk only.
  (mapcar
   #'(lambda (prop)
       (cons prop (occ-get-property obj prop)))
   props))


(cl-defmethod occ-class-slots ((obj occ-obj))
  (let* ((plist (cl-obj-plist-value obj))
         (plist-keys (plist-get-keys plist))
         (slots (cl-class-slots (cl-classname obj))))
    (append slots
            (mapcar #'key2sym plist-keys))))
(cl-defmethod occ-obj-defined-slots ((obj occ-obj))
  (let* ((plist (cl-obj-plist-value obj))
         (plist-keys (plist-get-keys plist))
         (slots
          (append
           (cl-class-slots (cl-classname obj))
           (mapcar #'key2sym plist-keys))))
    slots))
(cl-defmethod occ-obj-defined-slots-with-value ((obj occ-obj))
  (let* ((slots (occ-obj-defined-slots obj)))
    (remove-if-not
     #'(lambda (slot)
         (occ-get-property obj slot))
     slots)))
(cl-defmethod cl-method-matched-arg ((method symbol)
                                     (ctx symbol))
  (cl-method-first-arg method))
(cl-defmethod cl-method-matched-arg ((method symbol)
                                     (ctx occ-ctx))
  (let ((slots (occ-obj-defined-slots-with-value ctx)))
    (remove-if-not
     #'(lambda (arg) (memq arg slots))
     (cl-method-first-arg method))))
(cl-defmethod cl-method-matched-arg ((method1 symbol)
                                     (method2 symbol)
                                     (ctx occ-ctx))
  (let ((slots (cl-method-first-arg-with-value method2 ctx)))
    (remove-if-not
     #'(lambda (arg) (memq arg slots))
     (cl-method-first-arg method1))))


(cl-defgeneric cl-method-sig-matched-arg (method-sig
                                          ctx)
  "test")
(cl-defmethod cl-method-sig-matched-arg ((method-sig cons)
                                         (ctx symbol))
  (cl-method-param-case method-sig))
(cl-defmethod cl-method-sig-matched-arg ((method-sig cons)
                                         (ctx occ-ctx))
  (let ((slots (occ-obj-defined-slots-with-value-new ctx)))
    (remove-if-not
     #'(lambda (arg) (memq arg slots))
     (cl-method-param-case method-sig))))
(cl-defmethod cl-method-sigs-matched-arg ((method-sig1 cons)
                                          (method-sig2 cons)
                                          (ctx occ-ctx))
  (let ((slots (cl-method-param-case-with-value-new method-sig2 ctx)))
    (remove-if-not
     #'(lambda (arg) (memq arg slots))
     (cl-method-param-case method-sig1))))



;;; occ-obj-common.el ends here
