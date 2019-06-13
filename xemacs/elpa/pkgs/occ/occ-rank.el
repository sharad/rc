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


(cl-defgeneric occ-rankprop-with (obj
                                  ctx
                                  prop)
  "occ-rankprop-with")

(cl-defgeneric occ-rankprop (obj
                             prop)
  "occ-rankprop")

(cl-defgeneric occ-calculate-rank-with (obj
                                        ctx)
  "occ-rank-with")

(cl-defgeneric occ-calculate-rank (obj)
  "occ-rank")


(cl-defmethod occ-rankprop-with (obj
                                 ctx
                                 prop)
  (occ-debug :debug "occ-rankprop-with(obj=%s ctx=%s symbol=%s)" obj ctx prop)
  0)

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
                         (occ-class-slots obj)))))
    rank))


;; * ctx-tsk rank
(cl-defmethod occ-calculate-rank-with ((obj occ-tsk)
                                       (ctx occ-ctx))
  ;; too much output
  (occ-debug :debug "occ-rank-with(obj=%s ctx=%s)" obj ctx)
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot)
                             (occ-rankprop-with obj ctx (downcase-sym slot)))
                         (occ-class-slots obj)))))
    rank))


(cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj prop)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-rankprop-with tsk ctx prop)))

(cl-defmethod occ-calculate-rank ((obj occ-obj-ctx-tsk))
  ;; too much output
  (occ-debug :debug "occ-rank(obj=%s)" obj)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-calculate-rank-with tsk ctx)))


;;; occ-rank.el ends here
