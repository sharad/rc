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
(require 'occ-statistics)


(defvar occ-filters-plist nil)

(defun occ-filter-add (key fun)
  (setq occ-filters-plist
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


(cl-defmethod occ-apply-recursively ((obj occ-ctx)
                                     methods
                                     seq)
  (let ((fun (car methods)))
    (if fun
        (occ-apply-recursively obj
                               (cdr methods)
                               (funcall fun obj seq))
      seq)))

(cl-defmethod occ-filter ((obj occ-ctx)
                          methods
                          &key builder)
  (occ-apply-recursively obj
                         (apply #'occ-filters-get methods)
                         (occ-list obj :builder builder)))


(cl-defmethod occ-filter-mutual-deviation ((obj occ-ctx)
                                           seq) ;TODO: make it after method
  "Return matched Seq for context CTX"
  (if (occ-collection-object)
      (let* ((rankslist  (mapcar #'occ-rank seq))
             (avgrank    (apply #'occ-stats-average rankslist))
             (varirank   (apply #'occ-stats-variance rankslist)))
        ;; (occ-debug :debug "occ-collection-obj-matches :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length seq))
        (occ-debug :debug "occ-filter-mutual-deviation: avgrank = %d varirank = %d"
                          avgrank varirank)
        (remove-if-not
         #'(lambda (obj)
             (>= (occ-rank obj) avgrank))
         seq))
    (error "(occ-collection-object) returned nil")))

(occ-filter-add :mutual-deviation #'occ-filter-mutual-deviation)

(cl-defmethod occ-filter-positive ((obj occ-ctx)
                                   seq)
  (remove-if-not #'(lambda (obj) (> (occ-rank obj) 0))
                 seq))

(occ-filter-add :positive #'occ-filter-positive)


(cl-defmethod occ-filter-nonnegative ((obj occ-ctx)
                                      seq)
  (remove-if-not #'(lambda (obj) (>= (occ-rank obj) 0))
                 seq))

(occ-filter-add :nonnegative #'occ-filter-nonnegative)

(defvar occ-filter-min 0)
(cl-defmethod occ-filter-min ((obj occ-ctx)
                              seq)
  (remove-if-not #'(lambda (obj) (>= (occ-rank obj) occ-filter-min))
                 seq))

(occ-filter-add :min #'occ-filter-min)

(defvar occ-filter-max 0)
(cl-defmethod occ-filter-max ((obj occ-ctx)
                              seq)
  (remove-if-not #'(lambda (obj) (>= (occ-rank obj) occ-filter-max))
                 seq))

(occ-filter-add :max #'occ-filter-max)


(defun occ-list-filters () '(:nonnegative))

(defun occ-match-filters () '(:positive :mutual-deviation))


(apply #'occ-filters-get (occ-match-filters))
(apply #'occ-filters-get (occ-list-filters))


;;; occ-list-filter.el ends here
