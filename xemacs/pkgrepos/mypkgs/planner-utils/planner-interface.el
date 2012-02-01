;;; planner-interface.el --- Planner interface

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad at home>
;; Keywords: lisp, convenience

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

(defun planner-today-ensure-exists ()
  (unless (file-exists-p (concat planner-directory (planner-today) ".muse"))
                        (save-excursion
                          (save-window-excursion
                            (plan))))
    (planner-today))

(defun planner-task-lists (plan)
  (planner-extract-tasks
   (list (cons plan (concat planner-directory "/" plan ".muse")))))

;;test
;;
(testing
 (planner-task-lists (planner-today-ensure-exists)))


(defun planner-plans-on-task-lists-main (task-lists)
  (remove-duplicates
   (apply
    'append
    (mapcar
     '(lambda (task)
        (split-string (nth 6 task) ","))
     task-lists))
   :test 'equal))

(defun planner-plans-on-task-lists (task-lists)
  (mapcar
   '(lambda (str)
     (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
         (replace-match "\\1" t nil str)))
   (planner-plans-on-task-lists-main task-lists)))

;; (defun planner-plans-on-task-lists (task-lists)
;;   (mapcar
;;    '(lambda (str)
;;      (let ((task
;;             (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
;;                 (replace-match "\\1" t nil str))))
;;      (cons task task)))
;;    (planner-plans-on-task-lists-main task-lists)))

;; test
(testing
 (let ((str "[[TaskBy]]"))
   (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
       (replace-match "\\1" t nil str))))

;;test
(testing
 (planner-plans-on-task-lists
  (planner-task-lists (planner-today-ensure-exists))))

(defun planner-plans-on-page (page)
  (planner-plans-on-task-lists
   (planner-task-lists page)))

(defun planner-plans-on-today ()
  (planner-plans-on-page (planner-today-ensure-exists)))

(testing
 (planner-plans-on-today))

;; (defun planner-task-lists-if (test task-lists &key fun)
;;   (loop for task in task-lists          ;lisp is beautiful !!
;;         when (funcall test task)
;;         collect (if fun (funcall fun task) task)))

(defun planner-task-lists-if (test task-lists &key fun)
  (mapcar fun
          (remove-if-not test task-lists)))

;;start: one way to get tasks of plan from one page
(defun task-lists-of-plan-with-status-p (task plan status)
  (and
   (member (concat "[[" plan "]]") (nth 5 task))
   (member (nth 3 task) status)))


(defun extract-task-name (task)
  ;; (if (string-match "^\\(.\+\}\}\\)\s\+[[][[]" task)
  ;; (if (string-match "^\\(.\+\}\}\\)\s\+" task)
  (if (string-match "^\\(.\+\}\}\\)\\(\s\+[[][[]\\)\?" task)
      (match-string 1 task)))


(defun extract-task-name-from-list (task-list)
  (extract-task-name (nth 4 task-list)))

(defun planner-tasks-of-plan-from-page (page plan status) ;should be fault tolrent.
  (planner-task-lists-if                         ;else face lot of time waste.
   '(lambda (task)
     (task-lists-of-plan-with-status-p task plan status))
   (planner-task-lists page)
   :fun #'extract-task-name-from-list))
;;end

;;test
(testing
 (planner-task-lists-if
  '(lambda (task) (task-list-of-plan-with-status-p task (planner-today-ensure-exists) '("_" "o")))
  (planner-task-lists (planner-today-ensure-exists))
  :fun #'extract-task-name-from-list))

;;start: another way to get tasks of plan from one page
(defun task-lists-with-status-p (task status)
  (member (nth 3 task) status))

(defun planner-tasks-from-page (page status)
  (planner-task-lists-if
   '(lambda (task)
      (task-lists-with-status-p task plan status))
   (planner-task-lists page)
   :fun '(lambda (task-list) (nth 4 task-list))))

(defun planner-tasks-of-pages-intersection (plan1 plan2 status)
  (intersection
   (planner-tasks-from-page page1 status)
   (planner-tasks-from-page page2 status)))
;;end

;;test
(testing
 (planner-tasks-of-plan-from-page (planner-today-ensure-exists) "MyMIS" '("_" "o")))

;;should be fault tolrent. else face lot of time waste.
(defun planner-tasks-of-plan-today (plan status)
  (planner-tasks-of-plan-from-page
   (planner-today-ensure-exists) plan '("_" "o")))

;;test
(testing
 (planner-tasks-of-plan-today (planner-today-ensure-exists) '("_" "o")))

(defun normalize-task (task)
  (replace-regexp-in-string
   "\\([]\\[]\\)" "\\\\\\1" task))


(provide 'planner-interface)
;;; planner-interface.el ends here
