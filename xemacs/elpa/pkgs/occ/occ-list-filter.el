;;; occ-list-filter.el --- list filter               -*- lexical-binding: t; -*-

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


(provide 'occ-list-filter)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)


(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj occ-ctx)
                                       &key
                                       builder)
  "return CTSKs list"
  (let ((builder (or builder #'occ-build-ctsk)))
    (let ((ctsks
            (run-unobtrusively
              (let ((tsks (occ-collect-list collection))) ;;????TODO
                (when tsks
                  (mapcar
                   #'(lambda (tsk) (funcall builder tsk obj))
                   tsks))))))
       (unless (eq t ctsks)
         ctsks))))

(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj null)
                                       &key builder)
  "return CTSKs list"
  (occ-collection-obj-list collection
                           (occ-make-ctx-at-point)
                           :builder builder))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-list (obj
                         &key
                         builder)
  "occ-list")

(cl-defmethod occ-list ((obj occ-ctx)
                        &key
                        builder)
  "return CTXUAL-TSKs container"
  (occ-collection-obj-list (occ-collection-object)
                           obj
                           :builder builder))

(cl-defmethod occ-list ((obj null)
                        &key builder)
  "return TSKs container"
  (occ-list (occ-make-ctx-at-point)
            :builder builder))


(defvar occ-filters-plist nil)

(defun occ-filter-add (key fun)
  (setq occ-helm-actions-plist
        (plist-put
         occ-filters-plist
         key fun)))

(defun occ-filter-get (key)
  (plist-get occ-filters-plist key))

(defun occ-filters-get (&rest keys)
  (let ((funs nil))
    (dolist (key keys)
      (let ((fun (occ-filter-get key)))
        (when fun
          (setf funs (nconc funs (list fun))))))
    funs))

(defun occ-apply-recursively (methods result)
  (let ((fun (car methods)))
    (if fun
        (occ-apply-recursively (cdr methods)
                               (funcall fun result))
      result)))

(cl-defmethod occ-filter ((obj occ-ctx)
                          methods
                          &key builder)
  (occ-apply-recursively (occ-filters-get methods)
                         (occ-list obj
                                   :builder builder)))


(defun occ-filter-mutual-deviation (objs) ;TODO: make it after method
  "Return matched Objs for context CTX"
  (if (occ-collection-object)
      (let* ((rankslist  (mapcar
                          #'occ-rank
                          objs))
             (avgrank    (if (= 0 (length rankslist))
                             0
                           (/
                            (reduce #'+ rankslist)
                            (length rankslist))))
             (varirank   (if (= 0 (length rankslist))
                             0
                           (sqrt
                            (/
                             (reduce #'+
                                     (mapcar #'(lambda (rank) (expt (- rank avgrank) 2)) rankslist))
                             (length rankslist))))))
        ;; (occ-debug :debug "occ-collection-obj-matches :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length objs))
        (remove-if-not
         #'(lambda (obj)
             (>= (occ-rank obj) avgrank))
         objs))
    (error "(occ-collection-object) returned nil")))

(occ-filter-add :mutual-deviation #'occ-filter-mutual-deviation)

(defun occ-filter-positive (objs)
  (remove-if-not #'(lambda (obj) (> (occ-rank obj) 0)) objs))

(occ-filter-add :positive #'occ-filter-positive)


(defun occ-list-filters () nil)

(defun occ-match-filters () '(:positive :mutual-deviation))

;;; occ-list-filter.el ends here
