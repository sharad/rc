;;; occ-helm.el --- occ helm                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-helm)


(require 'lotus-tree-manager)
(require 'org-capture+-helm)
(require 'org-capture+-helm-dynamic)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-debug-method)
(require 'occ-obj-method)
(require 'occ-helm-method)


;; TODO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* MILESTONE %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable meeting) "MEETING"
                                   "* MEETING %? %^g\n %i\n [%a]\n")

;; NOTE
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable note) "NOTE"
                                   "* <NOTE> %? %^g\n %i\n [%a]\n")
;; INFO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable info) "INFO"
                                   "* <INFO> %? %^g\n %i\n [%a]\n")
;; EVENT
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable event) "EVENT"
                                   "* <EVENT> %? %^g\n %i\n [%a]\n")


(defun occ-capture+-helm-select-template ()
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk clockable todo)
                                              0)))
    (funcall selector)))


(occ-generate-plist-functions occ-helm action)
(progn
  (occ-helm-action-clear)
  (occ-helm-action-add :ignore                   "Ignore"                   #'ignore)
  (occ-helm-action-add :identity                 "Select"                   #'identity)
  (occ-helm-action-add :clock-in                 "Clock-in"                 #'occ-clock-in)
  (occ-helm-action-add :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in)
  (occ-helm-action-add :try-clock-in             "Try Clock-in"             #'occ-try-clock-in)
  (occ-helm-action-add :procreate-child          "Procreate Child"          #'occ-procreate-child)
  (occ-helm-action-add :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in)
  (occ-helm-action-add :goto                     "Goto"                     #'occ-goto)
  (occ-helm-action-add :set-to                   "Set To"                   #'occ-set-to)
  (occ-helm-action-add :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit) ;TODO: implement it.
  (occ-helm-action-add :proprty-edit-combined    "Proprtes Edit Combined"   #'occ-props-edit-combined) ;TODO: implement it.
  (occ-helm-action-add :call-with-obj            "Call with object"         #'occ-call-with-obj)
  (occ-helm-action-add :set-debug-obj            "Set debug obj"            #'occ-set-debug-obj)
  (occ-helm-action-add :rank                     "Get Rank"                 #'occ-print-rank)
  (occ-helm-action-add :tsk                      "Get Task"                 #'occ-print-tsk))


(cl-defgeneric occ-get-helm-actions-plist (obj
                                           name-action-key)
  "occ-get-helm-actions-plist")

(cl-defmethod occ-get-helm-actions-plist ((obj null)
                                          name-action-key)
  "occ-get-helm-actions-plist"
  (cond
   ((eql (car name-action-key) 'normal)
    (apply #'occ-helm-actions-get (cdr name-action-key)))
   ((eql (car name-action-key) 'generator) nil)))

(cl-defmethod occ-get-helm-actions-plist ((obj occ-obj)
                                          name-action-key)
  "occ-get-helm-actions-plist"
  (occ-get-helm-actions-plist nil name-action-key))

(cl-defmethod occ-get-helm-actions-plist ((obj occ-obj-tsk)
                                          name-action-key)
  "occ-get-helm-actions-plist"
  (cond
   ((eql (car name-action-key) 'normal)
    (apply #'occ-helm-actions-get (cdr name-action-key)))
   ((eql (car name-action-key) 'generator)
    (apply #'append
           (mapcar #'(lambda (generator)
                       (funcall (cdr generator) obj :param-only nil))
                   (apply #'occ-helm-actions-get (cdr name-action-key)))))))


(defvar occ-helm-actions-tree '(t))

(setq occ-helm-actions-tree '(t))

(defun occ-add-helm-actions-tree (keys class type &rest actions)
  (apply #'tree-add-class-item
         occ-helm-actions-tree
         keys
         class
         (mapcar #'(lambda (action) (cons type action))
                 actions)))

(occ-add-helm-actions-tree '(actions general)
                           "Simple"
                           'normal
                           :procreate-child
                           :procreate-child-clock-in
                           :call-with-obj
                           :set-debug-obj
                           :try-clock-in
                           :goto
                           :rank
                           :tsk)

(occ-add-helm-actions-tree '(actions edit)
                           "Editing"
                           'normal
                           :proprty-window-edit
                           :proprty-edit-combined)


(occ-helm-action-add :fast-edits-gen     "Fast Edits" #'occ-gen-helm-fast-edits)
(occ-helm-action-add :edits-gen          "Edit"       #'occ-gen-helm-edits)
(occ-helm-action-add :misc-gen           "Misc"       #'occ-gen-helm-misc)
(occ-helm-action-add :fast-checkouts-gen "Checkouts"  #'occ-gen-helm-checkouts)

(occ-add-helm-actions-tree '(actions general)
                           "Simple"
                           'generator
                           :misc-gen)

(occ-add-helm-actions-tree '(actions edit)
                           "Editing"
                           'generator
                           :fast-edits-gen
                           :edits-gen)

(occ-add-helm-actions-tree '(actions checkout)
                           "Editing"
                           'generator
                           :fast-checkouts-gen)


(cl-defgeneric occ-get-helm-actions-tree (obj keys)
  "occ-get-helm-actions-tree")

(cl-defmethod occ-get-helm-actions-tree ((obj null) keys)
  ;; (occ-message "occ-get-helm-actions-tree: called with obj = %s, keys = %s" obj keys)
  (apply #'append
         (mapcar #'(lambda (name-action-key)
                     (occ-get-helm-actions-plist obj name-action-key))
                 (collect-alist (tree-collect-items occ-helm-actions-tree nil keys 0)))))

(cl-defmethod occ-get-helm-actions-tree ((obj occ-obj) keys)
  ;; (occ-message "occ-get-helm-actions-tree: called with obj = %s, keys = %s" obj keys)
  (apply #'append
         (mapcar #'(lambda (name-action-key)
                     (occ-get-helm-actions-plist obj name-action-key))
                 (collect-alist (tree-collect-items occ-helm-actions-tree nil keys 0)))))


(cl-defmethod occ-get-helm-actions-tree-genertator ((obj null) keys)
  #'(lambda (action candidate)
      (occ-get-helm-actions-tree candidate keys)))

(cl-defmethod occ-get-helm-actions-tree-genertator ((obj occ-obj) keys)
  #'(lambda (action candidate)
      (occ-get-helm-actions-tree candidate keys)))

;;; occ-helm.el ends here
