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
                                       (obj occ-ctx))
  "return CTSKs list"
  (let ((ctsks
         (run-unobtrusively
           (let ((tsks (occ-collect-list collection))) ;;????TODO
             (when tsks
               (mapcar
                #'(lambda (tsk) (occ-build-ctsk tsk obj))
                tsks))))))
    (unless (eq t ctsks)
      ctsks)))

(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj null))
  "return CTSKs list"
  (occ-collection-obj-list collection (occ-make-ctx-at-point)))


(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj null))
  "Return all TSKs for context nil OBJ"
  ;; (occ-debug :debug "occ-collection-obj-matches list")
  (occ-collection-list collection))

;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj occ-ctx))
  "Return matched CTXUAL-TSKs for context CTX"
  ;; (occ-debug :debug "occ-collection-obj-matches list")
  (let ((tsks (occ-collection collection))
        (obj obj))
    (mapcar #'(lambda (tsk)
                (occ-build-ctxual-tsk tsk obj))
            tsks)))

;; ISSUE? should it return rank or occ-ctxual-tsks map
(cl-defmethod occ-collection-obj-matches ((collection occ-tree-collection)
                                          (obj occ-ctx))
  ;; (occ-debug :debug "occ-collection-obj-matches tree")
  "Return matched CTXUAL-TSKs for context CTX"
  (let ((tsks (occ-collection collection))
        (matched '()))
    (when tsks
      (occ-debug :debug "occ-collection-obj-matches BEFORE matched %s[%d]" matched (length matched))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args)
           (let ((ctxual-tsk (occ-build-ctxual-tsk tsk args)))
             (setf matched (nconc matched
                                  (list ctxual-tsk)))))
           ;; (occ-debug :debug "occ-rank heading = %s" (occ-tsk-heading tsk))
           ;; (let* ((ctxual-tsk (occ-build-ctxual-tsk tsk args))
           ;;        (rank       (occ-rank ctxual-tsk)))
           ;;   (unless rank (error "occ-collection-obj-matches[lambda]: rank is null"))
           ;;   (when (> (occ-rank ctxual-tsk) 0)
           ;;     (push ctxual-tsk matched)
           ;;     (occ-debug :debug "occ-collection-obj-matches[lambda]: tsk %s MATCHED RANK %d"
           ;;                (occ-tsk-heading tsk)
           ;;                (length matched))))
       tsks
       obj))
    (occ-debug :debug "occ-collection-obj-matches: AFTER matched %s[%d]" "matched" (length matched))
    matched))

;;TODO: make it after method
(cl-defmethod occ-collection-obj-matches :around  ((collection occ-collection)
                                                   (obj occ-ctx)) ;TODO: make it after method
  "Return matched CTXUAL-TSKs for context CTX"
  (if (occ-collection-object)
      (let* ((ctxual-tsks (cl-call-next-method))
             (rankslist  (mapcar
                          #'(lambda (ctxual-tsk)
                              (occ-rank ctxual-tsk))
                          ctxual-tsks))
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
        (occ-debug :debug "matched ctxtsks %s" (length ctxual-tsks))
        (remove-if-not
         #'(lambda (ctxual-tsk)
             (>= (occ-rank ctxual-tsk) avgrank))
         ctxual-tsks))
    (error "(occ-collection-object) returned nil")))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-list (obj)
  "occ-list")

(cl-defmethod occ-list ((obj occ-ctx))
  "return CTXUAL-TSKs container"
  (occ-collection-obj-list (occ-collection-object)
                           obj))

(cl-defmethod occ-list ((obj null))
  "return TSKs container"
  (occ-list (occ-make-ctx-at-point)))


(cl-defmethod occ-matches ((obj null))
  "return TSKs matches"
  (let ((collection (occ-collection-object)))
    (occ-collection-obj-matches collection
                                obj)))

(cl-defmethod occ-matches ((obj occ-ctx))
  "return CTXUAL-TSKs matches"
  (let ((collection (occ-collection-object)))
    (occ-collection-obj-matches collection
                                obj)))

;;; occ-list-filter.el ends here
