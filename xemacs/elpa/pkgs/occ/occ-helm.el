;;; occ-helm.el --- occ helm                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d _at_ GMail>
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


(require 'org-capture+-helm)
(require 'org-capture+-helm-dynamic)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-debug-method)
(require 'occ-obj-method)
(require 'occ-helm-method)


;;;###autoload
(org-capture+-add-heading-template '(occ tsk todo) "TODO"
                                   "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk todo) "TODO"
                                   "* MILESTONE %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk meeting) "MEETING"
                                   "* MEETING %? %^g\n %i\n [%a]\n")


(defun occ-capture+-helm-select-template ()
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk todo)
                                              0)))
    (funcall selector)))


;; (occ-action-generator-clear)
;; (occ-action-generator-set :fast-edits "Fast Edits" #'occ-gen-helm-fast-edits)

;; (defvar occ-helm-actions-plist nil)

;; (defun occ-helm-action-add (key name action)
;;   (setq occ-helm-actions-plist
;;         (plist-put
;;          occ-helm-actions-plist
;;          key (cons name action))))

;; (defun occ-helm-action-get (key)
;;   (plist-get occ-helm-actions-plist key))

;; (defun occ-helm-actions-get (&rest keys)
;;   (let ((actions nil))
;;     (dolist (key keys)
;;       (let ((name-action (occ-helm-action-get key)))
;;         (when name-action
;;           (setf actions (nconc actions (list name-action))))))
;;     actions))

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
  (occ-helm-action-add :rank                     "Get Rank"                 #'occ-print-rank)
  (occ-helm-action-add :tsk                      "Get Task"                 #'occ-print-tsk))


(defun occ-helm-general-actions ()
  (occ-helm-actions-get :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :proprty-edit-combined
                        :call-with-obj
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))

(defun occ-helm-intractive-command-actions ()
  (occ-helm-actions-get :try-clock-in
                        :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :proprty-edit-combined
                        :call-with-obj
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))

(defun occ-helm-intractive-launch-actions ()
  (occ-helm-actions-get :ignore
                        :identity
                        :try-clock-in
                        :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :proprty-edit-combined
                        :call-with-obj
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))


(cl-defmethod occ-helm-actions ((obj null))
  (occ-helm-general-actions))

(cl-defmethod occ-helm-actions ((obj occ-obj))
  (occ-helm-general-actions))


(cl-defmethod occ-helm-action-transformer ((obj null) actions)
  (append
   (occ-helm-actions obj)
   (occ-gen-edits-if-required obj nil nil)))

(cl-defmethod occ-helm-action-transformer ((obj occ-obj-tsk) actions)
  (append
   (occ-helm-actions obj)
   (occ-gen-edits-if-required obj nil nil)))


(cl-defun occ-helm-action-transformer-fun (action candidate)
  (occ-helm-action-transformer candidate action))


(cl-defmethod occ-props-edit-helm-actions ((obj null))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-props-edit-helm-actions ((obj occ-obj))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj null) actions)
  (occ-props-edit-helm-actions obj))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj occ-obj-tsk) actions)
  (occ-props-edit-helm-actions obj))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj occ-obj-ctx-tsk) actions)
  (occ-props-edit-helm-actions obj))

(cl-defun occ-props-edit-helm-action-transformer-fun (action candidate)
  (occ-props-edit-helm-action-transformer candidate action))

;;; occ-helm.el ends here
