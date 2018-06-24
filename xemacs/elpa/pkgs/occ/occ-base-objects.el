;;; occ-base-objects.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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


(require 'occ-common)
;; (require 'eieio)

;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; https://stackoverflow.com/questions/40884764/lisp-get-all-slot-names-from-an-class-instance

;; "org tsks accss common api"
;; (defvar org-)

(defvar occ-verbose 0)

(defstruct occ-obj
  name)

(defstruct (occ-prop (:include occ-obj))
  value)

(cl-defstruct (occ-tsk (:include occ-obj))
  heading
  heading-prop
  marker
  file
  point
  clock-sum
  plist)

(cl-defstruct (occ-tree-tsk (:include occ-tsk))
  subtree)

(cl-defstruct (occ-list-tsk (:include occ-tsk))
  )

(cl-defstruct (occ-ctx (:include occ-obj))
  buffer
  file)

(cl-defstruct (occ-ctxual-tsk (:include occ-obj))
  ctx
  tsk
  rank)

(cl-defstruct (occ-tsk-collection (:include occ-obj))
  root-files
  files)

(cl-defstruct (occ-tree-tsk-collection (:include occ-tsk-collection))
  tree)

(cl-defstruct (occ-list-tsk-collection (:include occ-tsk-collection))
  list)





(when nil

  (cl-defstruct xpoint
    x y)

  (setf zpoint (make-xpoint :x 5 :y 3))

  (setf (cl-struct-slot-value 'xpoint 'x point) 3)

  (cl--find-class 'xpoint)

  (cl-defstruct base
    baseattr)

  (cl-defstruct (drived (:include base))
    drivedattr)

  (setf baseobj1 (make-base :baseattr "xbaseattr"))



  (setf drivedobj1
        (make-drived
         :baseattr "xbaseattr"
         :drivedattr "xdrivedattr")))
(provide 'occ-base-objects)
;;; occ-base-objects.el ends here
