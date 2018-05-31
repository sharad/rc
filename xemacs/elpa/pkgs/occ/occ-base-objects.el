;;; occ-api-common.el --- occ-api               -*- lexical-binding: t; -*-
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
(require 'eieio)

;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; https://stackoverflow.com/questions/40884764/lisp-get-all-slot-names-from-an-class-instance

;; "org tasks accss common api"
;; (defvar org-)

(when nil
  (cl-defstruct base
    baseattr)

  (cl-defstruct (drived (:include base))
    drivedattr)

  (setf baseobj1 (make-base :baseattr "xbaseattr"))

  

  (setf drivedobj1
        (make-drived
         :baseattr "xbaseattr"
         :drivedattr "xdrivedattr")))

(defvar occ-verbose 0)

(defstruct occ-obj
  name)

(defstruct (occ-prop (:include occ-obj))
  value)

(cl-defstruct (occ-task (:include occ-obj))
  heading
  marker
  file
  point
  clock-sum
  plist)

(cl-defstruct (occ-tree-task (:include occ-task))
  subtree)

(cl-defstruct (occ-list-task (:include occ-task))
  )

(cl-defstruct (occ-context (:include occ-obj))
  )

(cl-defstruct (occ-contextual-task (:include occ-obj))
  context
  task)

(cl-defgeneric occ-matching-contextual-tasks (context)
  )

(cl-defstruct (occ-task-tree-task-collection (:include occ-obj))
  tree)

(cl-defstruct occ-task-tree-list-collection (occ-obj)
  list)

;; (mapcar #'slot-definition-name (class-slots occ-task))

(defun class-slots (class)
  (mapcar
   #'(lambda (slot) (aref slot 1))
   (cl--struct-class-slots
    (cl--struct-get-class class))))




(defun occ-tasks-associated-to-context (context)
  ;; (funcall occ-api-tasks-associated-to-context context)
  (funcall occ-matching-tasks context))

;; (defun occ-markers-associated-to-context (context)
(defun occ-markers-associated-to-context (context)
  (mapcar #'(lambda (e)
              (occ-task-get-property e :task-clock-marker))
          (occ-tasks-associated-to-context context)))

;; Dynamic plist task format
;; plist of :rank :marker :task etc

(defun occ-build-dyntaskpl (task context)
  (funcall occ-build-dyntaskpl task context))

(defun occ-dyntaskpl-get-task (dyntaskpl)
  (plist-get dyntaskpl :task))

(defun occ-dyntaskpl-get-marker (dyntaskpl)
  (let ((task (plist-get dyntaskpl :task)))
    (plist-get task :task-clock-marker)))

(defun occ-task-get-marker (task)
  (plist-get task :task-clock-marker))

(defun occ-dyntaskpls-associated-to-context (context)
  (funcall occ-matching-dyntaskpls context))

(defun occ-dyntaskpls-associated-to-context-filtered (context)
  ;; TODO Here do variance based filtering.
  (let* ((dyntaskpls (funcall occ-matching-dyntaskpls context))
         (rankslist  (mapcar #'(lambda (dyntaskpl) (plist-get dyntaskpl :rank))
                             dyntaskpls))
         (avgrank    (/
                      (reduce #'+ rankslist)
                      (length rankslist)))
         (varirank   (sqrt
                      (/
                       (reduce #'+
                               (mapcar #'(lambda (rank) (expt (- rank avgrank) 2)) rankslist))
                       (length rankslist)))))
    (remove-if-not
     #'(lambda (dyntaskpl)
         (>= (plist-get dyntaskpl :rank) avgrank))
     dyntaskpls)))

(defun occ-task-get-heading (task)
  (occ-task-get-property task :task-clock-heading))

(defun occ-dyntaskpl-print (dyntaskpl heading)
  (funcall occ-api-dyntaskpl-print dyntaskpl heading))

(provide 'occ-api-common)
;;; occ-api-common.el ends here
