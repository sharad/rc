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

(cl-defstruct base
  :baseattr)

(cl-defstruct drived (:include base)
              :drivedattr)

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

(cl-defgeneric isassoc (obj context)
  "isassoc")

(cl-defmethod isassoc ((task occ-task) (context occ-context))
  (mapcar (task)
          (slot (class-slot 'occ-task))))

(cl-defmethod isassoc ((prop (head :root)) (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((root
          (org-context-clock-task-get-property task :root))
         (root (if root (file-truename root))))
    (let* ((file (plist-get context :file))
           (file (if file (file-truename file))))
      (if root
          (progn
            (org-context-clock-debug :debug "task %s root %s" (org-context-clock-task-get-heading task) root)
            (org-context-clock-debug :debug "task %s file %s" (org-context-clock-task-get-heading task) file))
        (org-context-clock-debug :debug "task %s root %s not present."
                                 (org-context-clock-task-get-heading task) root))
      (if (and root file
               (string-match root file))
          (length root)
        0))))

(cl-defmethod isassoc ((prop (head :currfile) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((currfile
          (org-context-clock-task-get-property task :currfile))
         (currfile (if currfile (file-truename currfile))))
    (let* ((file (plist-get context :file))
           (file (if file (file-truename file))))
      (if currfile
          (progn
            (org-context-clock-debug :debug "task %s currfile %s" (org-context-clock-task-get-heading task) currfile)
            (org-context-clock-debug :debug "task %s file %s" (org-context-clock-task-get-heading task) file))
        (org-context-clock-debug :debug "task %s currfile %s not present."
                                 (org-context-clock-task-get-heading task) currfile))
      (if (and currfile file
               (string-match currfile file))
          (* 2 (length currfile))     ;as exact match to file giving double matching points.
        0))))

(cl-defmethod isassoc ((prop (head :status) (context occ-context)))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((todo-type
         (org-context-clock-task-get-property task :todo-type))
        (closed
         (org-context-clock-task-get-property task :closed))
        (status
         (org-context-clock-task-get-property task :todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod isassoc ((prop (head :sub-tree) (context occ-context)))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((sub-tree
         (org-context-clock-task-get-property task :sub-tree)))
    (org-context-clock-debug :debug "task %s subtree %s" (org-context-clock-task-get-heading task) (null (null sub-tree)))
    (if sub-tree -30 0)))

(cl-defmethod isassoc ((prop (head :task-key) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((key (org-context-clock-task-get-property task :KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod isassoc ((prop (head :heading-level) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((level
          (org-context-clock-task-get-property task :task-clock-level)))
    (if level level 0)))

(cl-defmethod isassoc ((prop (head :timebeing) (context occ-context)))
  (let ((timebeing (org-context-clock-task-get-property task :timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (org-context-clock-task-get-property task :task-clock-clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod isassoc ((prop (head :current-clock) (context occ-context)))
  (let* ((task-marker
          (org-context-clock-task-get-property task :task-clock-marker)))
    (if (and
         (markerp org-clock-hd-marker)
         (markerp task-marker)
         (equal org-clock-hd-marker org-clock-hd-marker))
        100
      0)))

(when nil
  (isassoc '(:root 1) (make-occ-context))

  (cl-defmethod isassoc ((task occ-task)
                         (context occ-context))
    (message "match isassoc"))

  (isassoc (make-occ-tree-task) (make-occ-context))
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
