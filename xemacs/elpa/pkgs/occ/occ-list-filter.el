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
(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-statistics)


(cl-defmethod occ-average ((obj occ-filter) sequence)
  (unless (occ-filter-average obj)
    (setf (occ-filter-average obj) (apply #'occ-stats-average sequence)))
  (occ-filter-average obj))

(cl-defmethod occ-stddev ((obj occ-filter) sequence)
  (unless (occ-filter-stddev obj)
    (setf (occ-filter-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-filter-stddev obj))

(cl-defmethod occ-variance ((obj occ-filter) sequence)
  (unless (occ-filter-variance obj)
    (setf (occ-filter-variance obj) (apply #'occ-stats-variance sequence)))
  (occ-filter-variance obj))


(cl-defmethod occ-ctx-filter ((obj occ-obj-ctx) filter)
  (unless (plist-get (occ-obj-ctx-filter-plist obj) filter)
    (plist-put (occ-obj-ctx-filter-plist obj) filter (occ-make-filter)))
  (plist-get (occ-obj-ctx-filter-plist obj) filter))


(occ-generate-plist-functions occ filter)

;; (occ-helm-actions-get )

;; (defvar occ-filters-plist nil)

;; (defun occ-filter-add (key fun)
;;   (setq occ-filters-plist
;;         (plist-put
;;          occ-filters-plist
;;          key fun)))

;; (defun occ-filter-get (key)
;;   (plist-get occ-filters-plist key))

(defun occ-filters-get (&rest keys)
  (let ((funs nil))
    (dolist (key keys)
      (let ((funkw-rank keys))
        (let ((funkw      (if (consp funkw-rank) (car funkw-rank) funkw-rank))
              (rank       (if (consp funkw-rank) (cadr funkw-rank) rank)))
          (when funkw
              (let ((fun (or (occ-filter-get funkw) funkw #'identity)))
                (setf funs (nconc funs
                                  (list (if (consp fun-rank)
                                            (list fun rank)
                                          fun)))))))))
    funs))


(defun occ-internal-get-filter-method (methods)
  (cond
   ((functionp methods)
    (occ-internal-get-filter-method (funcall methods)))
   ((and (symbolp   methods)
         (listp (symbol-value methods)))
    (occ-internal-get-filter-method (symbol-value methods)))
   ((and (symbolp   methods)
         (functionp (symbol-value methods)))
    (occ-internal-get-filter-method (functionp (symbol-value methods))))
   ((listp methods) methods)
   (t (error "Wrong %s methods" methods))))


(cl-defmethod occ-apply-recursively ((obj occ-ctx)
                                     methods
                                     sequence
                                     &key rank)
  (let* ((funkw-rank (car methods)))
    (let ((funkw      (if (consp funkw-rank) (car funkw-rank) funkw-rank))
          (rank       (if (consp funkw-rank) (cadr funkw-rank) rank)))
     ;; (occ-message "occ-apply-recursively: trying funkw-rank= %s funkw= %s" funkw-rank funkw)
     (if funkw
         (let ((fun  (or (cdr (occ-filter-get funkw))
                         funkw
                         #'identity)))
           (occ-apply-recursively obj
                                  (cdr methods)
                                  (funcall fun obj sequence :rank rank)
                                  :rank rank))
       sequence))))

(cl-defmethod occ-filter ((obj occ-ctx)
                          methods
                          sequence
                          &key rank)
  (let ((rank    (or rank #'occ-rank)))
    (occ-apply-recursively obj
                           methods
                           sequence
                           :rank rank)))


(cl-defmethod occ-filter-mutual-deviation ((obj occ-ctx)
                                           sequence
                                           &key rank) ;TODO: make it after method
  "Return matched Sequence for context CTX"
  (if (occ-collection-object)
      (let* ((rankslist  (mapcar #'occ-rank sequence))
             (avgrank    (apply #'occ-stats-average rankslist))
             (varirank   (apply #'occ-stats-variance rankslist)))
        ;; (occ-debug :debug "occ-collection-obj-matches :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length sequence))
        (occ-debug :debug "occ-filter-mutual-deviation: avgrank = %d varirank = %d"
                          avgrank varirank)
        (remove-if-not
         #'(lambda (obj)
             (>= (funcall rank obj) avgrank))
         sequence))
    (error "(occ-collection-object) returned nil")))

(occ-filter-add :mutual-deviation "Mutual Deviation" #'occ-filter-mutual-deviation)

(cl-defmethod occ-filter-positive ((obj occ-ctx)
                                   sequence
                                   &key rank)
  (remove-if-not #'(lambda (obj) (> (funcall rank obj) 0))
                 sequence))

(occ-filter-add :positive "Positive" #'occ-filter-positive)


(cl-defmethod occ-filter-nonnegative ((obj occ-ctx)
                                      sequence
                                      &key rank)
  (remove-if-not #'(lambda (obj) (>= (funcall rank obj) 0))
                 sequence))

(occ-filter-add :nonnegative "Non negative" #'occ-filter-nonnegative)

(defvar occ-filter-min 0)
(cl-defmethod occ-filter-min ((obj occ-ctx)
                              sequence
                              &key rank)
  (remove-if-not #'(lambda (obj) (>= (funcall rank obj) occ-filter-min))
                 sequence))

(occ-filter-add :min "Minimum" #'occ-filter-min)

(defvar occ-filter-max 0)
(cl-defmethod occ-filter-max ((obj occ-ctx)
                              sequence
                              &key rank)
  (remove-if-not #'(lambda (obj) (>= (funcall rank obj) occ-filter-max))
                 sequence))

(occ-filter-add :max "Maximum" #'occ-filter-max)


(defun occ-list-filters ()
  '(:nonnegative))

(defun occ-match-filters ()
  (list :positive
        :mutual-deviation
        (list :positive         #'occ-member-tsk-rank)))
        ;; (list :mutual-deviation #'occ-member-tsk-rank)

(defun occ-never-filters ()
  "Used to filter mainly non-tsk"
  '(:nonnegative))

;;; occ-list-filter.el ends here
