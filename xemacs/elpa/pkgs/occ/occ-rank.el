;;; occ-rank.el --- occ rank                         -*- lexical-binding: t; -*-

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

(provide 'occ-rank)


(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-prop-intf)

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

(cl-defgeneric occ-calculate-rank (obj)
  "occ-rank")


(cl-defmethod occ-calculate-rank ((obj occ-tsk))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)"
             obj)
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot)
                             (occ-rankprop obj (downcase-sym slot)))
                         (occ-properties-to-calculate-rank obj)))))
    rank))


(cl-defmethod occ-calculate-rank ((obj occ-obj-ctx-tsk))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)" obj)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (reduce #'+
            (mapcar #'(lambda (slot)
                        (occ-rankprop obj (downcase-sym slot)))
                    (occ-properties-to-calculate-rank obj)))))


(cl-defmethod occ-calculate-avgrank ((obj occ-ctx))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)"
             obj)
  (let* ((objs      (occ-list obj #'occ-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-rank objs))
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-calculate-varirank ((obj occ-ctx))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)"
             obj)
  (let* ((objs      (occ-list obj #'occ-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-rank objs))
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


(cl-defmethod occ-calculate-avgrank ((obj occ-collection))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)"
             obj)
  (let* ((objs      (occ-collect-list obj))
         (rankslist (mapcar #'occ-rank objs))
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-calculate-varirank ((obj occ-collection))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)"
             obj)
  (let* ((objs      (occ-collect-list obj))
         (rankslist (mapcar #'occ-rank objs))
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


;; (occ-avgrank (occ-collection-object))
;; (occ-varirank (occ-collection-object))

;;; occ-rank.el ends here
