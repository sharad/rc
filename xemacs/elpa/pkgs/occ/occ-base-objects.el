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
(defvar occ-verbose 0)

(defstruct occ-obj)

(defstruct occ-obj
  name)

(defstruct (occ-prop (:include occ-obj))
  prop)

(defstruct (occ-task (:include occ-obj))
  heading
  marker
  file
  point
  clock-sum
  plist)

(defstruct (occ-treetask (:include occ-task))
  subtree)

(defstruct (occ-listtask (:include occ-task))
  )

(defstruct (occ-context (:include occ-obj))
  )

(defstruct (occ-contextualtask (:include occ-obj))
  (context
   task))

(defgeneric occ-matching-contextualtasks (context)
  )

(defstruct (occ-task-tree-task-collection (:include occ-obj))
  (tree))

(defstruct occ-task-tree-list-collection (occ-obj)
  (list))

;; (mapcar #'slot-definition-name (class-slots occ-task))

(mapcar
 #'(lambda (slot) (aref slot 1))
 (cl--struct-class-slots
  (cl--struct-get-class 'occ-task)))

(defgeneric set (prop value task context)
  )
(defgeneric get (prop value task context)
  )
(defgeneric assoc (prop value task context)
  )

(defstruct occ-root-prop (occ-property)
  ((name
    :initarg :name
    :custom string)))

(defmethod set ((prop occ-root-prop) value task context)
  )
(defmethod get ((prop occ-root-prop) value task context)
  )
(defmethod assoc ((prop occ-root-prop) value task context)
  )


;; (defun org-Xtask-get-property (task property)
(defun occ-task-get-property (task property)
  (plist-get task property))

;; (defun org-Xtask-set-property (task property value)x50
(defun occ-task-set-property (task property value)
  (plist-put task property value))

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
