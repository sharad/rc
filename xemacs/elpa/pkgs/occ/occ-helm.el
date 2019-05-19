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

(defvar occ-helm-actions-plist nil)

(defun occ-helm-action-add (key name action)
  (setq occ-helm-actions-plist
        (plist-put occ-helm-actions-plist key (cons name action))))

(defun occ-helm-action-get (key)
  (plist-get occ-helm-actions-plist key))

(defun occ-helm-actions-get (&rest keys)
  (let ((actions nil))
    (dolist (key keys)
      (let ((name-action (occ-helm-action-get key)))
        (when name-action
          (push name-action actions))))
    (reverse actions)))

(occ-helm-action-add :identity                     "Select"                       #'identity)
(occ-helm-action-add :clock-in                     "Clock-in"                     #'occ-clock-in)
(occ-helm-action-add :try-clock-in                 "Try Clock-in"                 #'occ-try-clock-in)
(occ-helm-action-add :procreate-child              "Procreate Child"              #'occ-procreate-child)
(occ-helm-action-add :procreate-child-clock-in     "Procreate Child Clock-in"     #'occ-procreate-child-clock-in)
(occ-helm-action-add :goto                         "Goto"                         #'occ-goto)
(occ-helm-action-add :set-to                       "Set To"                       #'occ-set-to)
(occ-helm-action-add :proprty-window-edit                 "Proprtes Window Edit"                 #'occ-props-window-edit) ;TODO: implement it.

(cl-defmethod occ-helm-actions ((obj null))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-helm-actions ((obj occ-ctx))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))


(cl-defmethod occ-helm-action-transformer ((obj null) actions)
  (occ-helm-actions-get :procreate-child :proprty-window-edit))

(cl-defmethod occ-helm-action-transformer ((obj occ-tsk) actions)
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

;; (cl-defmethod occ-helm-action-transformer ((obj occ-ctx) actions)
;;   (list
;;    (cons "Clock-in" #'occ-clock-in)
;;    (cons "Child"    #'occ-procreate-child)))

(cl-defmethod occ-helm-action-transformer ((obj occ-ctsk) actions)
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-helm-action-transformer ((obj occ-ctxual-tsk) actions)
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defun occ-helm-action-transformer-fun (action candidate)
  (occ-helm-action-transformer candidate action))


(cl-defun occ-helm-build-candidates-source (candidates &key action action-transformer)
  (when candidates
    (helm-build-sync-source (concat
                             "Select matching "
                             (symbol-name (cl-classname (car candidates))))
      :candidates (mapcar #'occ-candidate candidates)
      ;; :action actions
      :action-transformer action-transformer
      :history 'org-refile-history)))
;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source ()
  (helm-build-dummy-source "Create tsk"
    :action (helm-make-actions
             "Create tsk"
             'sacha/helm-org-create-tsk)))

(defun occ-helm-build-obj-source (obj &optional actions)
  (occ-helm-build-candidates-source
   (occ-list obj)
   actions))


(cl-defgeneric occ-helm-select (obj collector)
  "occ-helm-select")

(cl-defmethod occ-helm-select ((obj occ-ctx) &key collector action action-transformer timeout)
  (let ((ctx-tsk (occ-select obj
                                :collector collector
                                :action action
                                :action-transformer action-transformer
                                :timeout timeout)))
    (when (occ-obj-ctx-tsk-p ctx-tsk)
      (occ-debug :debug "Selected ctxual-tsk %s" (occ-format ctx-tsk 'capitalize))
      ctx-tsk)))


(defun occ-helm-select-XYZ (obj
                            selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
  (let (helm-sources)
    (push
     (occ-helm-build-obj-source obj (list (cons "Clock in and track" selector)))
     helm-sources)

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-candidate
                            (occ-build-obj (occ-current-tsk) obj)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))
    (funcall action (helm helm-sources))))




;; (cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
;;   "occ-sacha-helm-action")

;; (cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
;;   ;; (message "sacha marker %s" (car dyntskpls))
;;   ;; (setq sacha/helm-org-refile-locations tbl)
;;   (progn
;;     (helm
;;      (list
;;       (helm-build-sync-source "Select matching tsks"
;;         :candidates (mapcar 'occ-candidate ctxask)
;;         :action (list ;; (cons "Select" 'identity)
;;                  (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
;;         :history 'org-refile-history)))))
;; ;; (helm-build-dummy-source "Create tsk"
;; ;;   :action (helm-make-actions
;; ;;            "Create tsk"
;; ;;            'sacha/helm-org-create-tsk))


;;; occ-helm.el ends here
