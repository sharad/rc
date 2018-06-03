;;; occ-object-methods.el --- occ-api               -*- lexical-binding: t; -*-
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
(require 'occ-base-objects)


(when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/

  (defun cl-get-field (object field)
    (cl-struct-slot-value (cl-classname object) field object))

  (defun cl-set-field (object field value)
    (setf (cl-struct-slot-value (cl-classname object) field object) value))

  (get-field dave 'name)
  (set-field dave 'name "Simon Smith"))

(cl-defmethod occ-get-property ((task occ-task) prop)
  (if (memq prop (class-slots (cl-classname task)))
      (cl-get-field task prop)
    (plist-get
     (cl-struct-slot-value (cl-classname task) 'plist task)
     (sym2key prop))))
(cl-defmethod occ-set-property ((task occ-task) prop val)
  (if (memq prop (class-slots (cl-classname task)))
      (setf (cl-struct-slot-value (cl-classname task) prop task) val)
    (plist-put
     (cl-struct-slot-value (cl-classname task) 'plist task)
     (sym2key prop) val)))

(cl-defmethod occ-class-slot ((obj occ-obj))
  (let* ((plist (cl-struct-slot-value (cl-classname obj) 'plist obj))
         (plist-keys (plist-get-keys plist))
         (slots (class-slot (cl-classname task))))
    (append slots
            (mapcar #'key2sym plist-keys))))

(cl-defmethod occ-fontify-like-in-org-mode ((task occ-task))
  (let* ((level   (or (occ-get-property task 'level) 0))
         (heading (occ-get-property task 'heading-prop))
         (prefix  (concat (make-string level ?\*) " ")))
    (if nil ;; if test without else with prefix
        (substring
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only)
         (1+ level))

      (org-fontify-like-in-org-mode
       (concat prefix heading)
       org-odd-levels-only))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defgeneric isassoc (obj context)
  "isassoc")

(cl-defmethod isassoc ((task occ-task) (context occ-context))
  (let ((rank
         (reduce #'+
                 (mapcar
                  #'(lambda (slot)
                      (isassoc (cons slot task) context))
                  (occ-class-slot (cl-classname task))))))
    (make-occ-contextual-task :task task :context context :rank rank)))

(cl-defmethod isassoc ((task occ-task-collection) (context occ-context))
  )

(cl-defmethod isassoc ((task-pair (head 'root)) (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((root
          (occ-get-property (cdr task-pair) 'root))
         (root (if root (file-truename root))))
    (let* ((file (plist-get context :file))
           (file (if file (file-truename file))))
      (if root
          (progn
            (occ-debug :debug "task %s root %s" (occ-task-heading (cdr task-pair)) root)
            (occ-debug :debug "task %s file %s" (occ-task-heading (cdr task-pair)) file))
        (occ-debug :debug "task %s root %s not present."
                   (occ-task-heading (cdr task-pair)) root))
      (if (and root file
               (string-match root file))
          (length root)
        0))))

(cl-defmethod isassoc ((task-pair (head 'currfile) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((currfile
          (occ-get-property (cdr task-pair) 'currfile))
         (currfile (if currfile (file-truename currfile))))
    (let* ((file (plist-get context :file))
           (file (if file (file-truename file))))
      (if currfile
          (progn
            (occ-debug :debug "task %s currfile %s" (occ-task-heading (cdr task-pair)) currfile)
            (occ-debug :debug "task %s file %s"     (occ-task-heading (cdr task-pair)) file))
        (occ-debug :debug "task %s currfile %s not present."
                   (occ-task-heading (cdr task-pair)) currfile))
      (if (and currfile file
               (string-match currfile file))
          (* 2 (length currfile))     ;as exact match to file giving double matching points.
        0))))

(cl-defmethod isassoc ((task-pair (head 'status) (context occ-context)))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((todo-type
         (occ-get-property (cdr task-pair) 'todo-type))
        (closed
         (occ-get-property (cdr task-pair) 'closed))
        (status
         (occ-get-property (cdr task-pair) 'todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod isassoc ((task-pair (head 'subtree) (context occ-context)))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((sub-tree
         (occ-get-property (cdr task-pair) 'subtree)))
    (occ-debug :debug "task %s subtree %s" (occ-task-heading (cdr task-pair)) (null (null sub-tree)))
    (if sub-tree -30 0)))

(cl-defmethod isassoc ((task-pair (head 'key) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((key (occ-get-property (cdr task-pair) 'KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod isassoc ((task-pair (head 'heading-level) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((level
          (occ-get-property (cdr task-pair) 'level)))
    (if level level 0)))

(cl-defmethod isassoc ((task-pair (head 'timebeing) (context occ-context)))
  (let ((timebeing (occ-get-property (cdr task-pair) 'timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property (cdr task-pair) 'clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod isassoc ((task-pair (head 'current-clock) (context occ-context)))
  (let* ((task-marker
          (occ-get-property (cdr task-pair) 'marker)))
    (if (and
         (markerp org-clock-hd-marker)
         (markerp task-marker)
         (equal org-clock-hd-marker org-clock-hd-marker))
        100
      0)))

(when nil
  (cl-defmethod isassoc ((task-pair (head 'root)) (context list))
    (message "%s" prop))

  (isassoc '(:root . 1) '())

  (cl-defmethod isassoc ((task occ-task)
                         (context occ-context))
    (message "match isassoc"))

  (isassoc (make-occ-tree-task) (make-occ-context))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric occ-matching-contextual-tasks (context)
  )

(provide 'occ-object-methods)
;;; occ-object-methods.el ends here
