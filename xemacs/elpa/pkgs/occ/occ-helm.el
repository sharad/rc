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


(cl-defmethod occ-helm-actions ((obj null))
  (list
   (cons "Child"  #'occ-child)))

(cl-defmethod occ-helm-actions ((obj occ-ctx))
  (list
   (cons "Clock-in" #'occ-clock-in)
   (cons "Child"    #'occ-child)))


(defun occ-helm-build-candidates-source (candidates &optional name-action-cons)
  (when candidates
    (helm-build-sync-source (concat "Select matching " (symbol-name
                                                        (cl-classname (car candidates))))
      :candidates (mapcar #'occ-candidate candidates)
      :action (append
               (list
                (cons "Select" #'identity))
               (if (consp (car name-action-cons))
                   name-action-cons
                 (list name-action-cons)))
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

(defun occ-helm-build-obj-source (obj &optional name-action-cons)
  (occ-helm-build-candidates-source
   (occ-list obj)
   name-action-cons))


(defun occ-helm-select (obj
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


(cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
  "occ-sacha-helm-action")

(cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
  ;; (message "sacha marker %s" (car dyntskpls))
  ;; (setq sacha/helm-org-refile-locations tbl)
  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching tsks"
        :candidates (mapcar 'occ-candidate ctxask)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
        :history 'org-refile-history)))))
;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))


;;; occ-helm.el ends here
