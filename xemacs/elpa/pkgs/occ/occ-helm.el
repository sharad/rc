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


(defun occ-helm-build-candidates-source (candidates &optional name-action-cons)
  (when candidates
    (helm-build-sync-source (concat
                             "Select matching "
                             (symbol-name (cl-classname (car candidates))))
      :candidates (mapcar #'occ-candidate candidates)
      :action (list (or name-action-cons
                        (cons "Select" #'identity)))
      :history 'org-refile-history)))
;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))


(defun occ-helm-build-obj-source (obj &optional name-action-cons)
  (occ-helm-build-source
   (occ-list obj)
   name-action-cons))



;;; occ-helm.el ends here
