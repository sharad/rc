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

(require 'occ-base-objects)


(when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/
  (defsubst get-index (object field)
    (cdr (assoc field (symbol-value (aref object 1)))))

  (defun get-field (object field)
    (aref object (get-index object field)))

  (defun set-field (object field value)
    (setf (aref object (get-index object field)) value))

  (get-field dave 'name)
  (set-field dave 'name "Simon Smith"))


(cl-defmethod occ-get-property ((task occ-task) prop)
  (if (memq prop (class-slots (aref task 0)))
      (aref task prop)
    (plist-get (aref task :plist) prop)))

(cl-defmethod occ-set-property ((task occ-task) prop val)
  (if (memq prop (class-slots (aref task 0)))
      (setf (aref task prop) val)
    (plist-put (aref task :plist) prop val)))

(cl-defmethod occ-fontify-like-in-org-mode ((task occ-task))
  (let* ((level   (or (occ-get-property task :level) 0))
         (heading (occ-get-property task :task-clock-heading-prop))
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
  (mapcar (task)
          (slot (class-slot 'occ-task))))

(cl-defmethod isassoc ((prop (head :root)) (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((root
          (occ-get-property task :root))
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
          (occ-get-property task :currfile))
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
         (occ-get-property task :todo-type))
        (closed
         (occ-get-property task :closed))
        (status
         (occ-get-property task :todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod isassoc ((prop (head :sub-tree) (context occ-context)))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((sub-tree
         (occ-get-property task :sub-tree)))
    (org-context-clock-debug :debug "task %s subtree %s" (org-context-clock-task-get-heading task) (null (null sub-tree)))
    (if sub-tree -30 0)))

(cl-defmethod isassoc ((prop (head :task-key) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((key (occ-get-property task :KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod isassoc ((prop (head :heading-level) (context occ-context)))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((level
          (occ-get-property task :task-clock-level)))
    (if level level 0)))

(cl-defmethod isassoc ((prop (head :timebeing) (context occ-context)))
  (let ((timebeing (occ-get-property task :timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property task :task-clock-clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod isassoc ((prop (head :current-clock) (context occ-context)))
  (let* ((task-marker
          (occ-get-property task :task-clock-marker)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'occ-object-methods)
;;; occ-object-methods.el ends here
