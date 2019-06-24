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

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

;; (cl-defgeneric occ-rankprop-with (obj
;;                                   ctx
;;                                   prop)
;;   "occ-rankprop-with")

(cl-defgeneric occ-rankprop (obj
                             prop)
  "occ-rankprop")

;; keep
;; (cl-defgeneric occ-calculate-rank-with (obj
;;                                         ctx)
;;   "occ-rank-with")

(cl-defgeneric occ-calculate-rank (obj)
  "occ-rank")


;; (cl-defmethod occ-rankprop-with (obj
;;                                  ctx
;;                                  prop)
;;   (occ-debug :debug "occ-rankprop-with(obj=%s ctx=%s symbol=%s)" obj ctx prop)
;;   0)

(cl-defmethod occ-rankprop (obj
                            prop)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
  0)


;; * tsk rank
(cl-defmethod occ-rankprop ((obj  occ-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)"
             obj
             prop)
  0)

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


;; * ctx-tsk rank
;; ;; keep
;; (cl-defmethod occ-calculate-rank-with ((obj occ-tsk)
;;                                        (ctx occ-ctx))
;;   ;; too much output
;;   (occ-debug :debug "occ-rank-with(obj=%s ctx=%s)" obj ctx)
;;   (let ((tsk-rank (occ-rank obj))
;;         (rank
;;          (reduce #'+
;;                  (mapcar #'(lambda (slot)
;;                              (occ-rankprop-with obj ctx (downcase-sym slot)))
;;                          (occ-properties-to-calculate-rank-with obj ctx)))))
;;     (+ rank tsk-rank)))


;; (cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
;;                             (prop symbol))
;;   (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-rankprop-with tsk ctx prop)))

(cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
  (occ-rankprop obj prop))

;; (cl-defmethod occ-calculate-rank ((obj occ-obj-ctx-tsk))
;;   ;; too much output
;;   (occ-debug :debug "occ-rank(obj=%s)" obj)
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (let ((tsk-rank (occ-rank obj))
;;           (rank
;;            (reduce #'+
;;                    (mapcar #'(lambda (slot)
;;                                (occ-rankprop-with tsk ctx (downcase-sym slot)))
;;                            (occ-properties-to-calculate-rank-with obj ctx)))))
;;       (+ rank tsk-rank))))

(cl-defmethod occ-calculate-rank ((obj occ-obj-ctx-tsk))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)" obj)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((tsk-rank (occ-rank obj))
          (rank
           (reduce #'+
                   (mapcar #'(lambda (slot)
                               (occ-rankprop obj (downcase-sym slot)))
                           (occ-properties-to-calculate-rank obj)))))
      (+ rank tsk-rank))))


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
