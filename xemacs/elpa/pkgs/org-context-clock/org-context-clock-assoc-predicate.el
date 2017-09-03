;;; org-context-clock-assoc-predicate.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

;; "Org tasks associated to context predicate functions"

(defvar org-task-associated-context-predicate-fns nil)

(progn ;; api

  ;; (defun org-tasks-associated-to-context-by-predicate (context)
  ;;   (let ((tasks (org-task-list-update-tasks))
  ;;         (matched '()))
  ;;     (dolist (fn org-task-associated-context-predicate-fns matched)
  ;;       (let ((partitions
  ;;              (reduce (lambda (task result)
  ;;                        (if (funcall fn context task)
  ;;                            (push task (first  result))
  ;;                            (push task (second result)))
  ;;                        result)
  ;;                      tasks
  ;;                      :initial-value (list nil nil)
  ;;                      :from-end t)))
  ;;         (setq
  ;;          tasks (second partitions)
  ;;          matched    (append matched (first partitions)))))))

  ;; (defun org-tasks-associated-to-context-by-predicate (context)
  ;;   (org-context-clock-list-matching-tasks context))

  (defun org-task-associated-to-context-by-predicate-p (task context)
    (if context
        (some
         '(lambda (fn) (funcall fn context task))
         org-task-associated-context-predicate-fns)))

  ;; (org-context-clock-api-set :predicate :tasks 'org-tasks-associated-to-context-by-predicate) ;will be in api
  (org-context-clock-assoc-api-set :predicate :taskp   'org-task-associated-to-context-by-predicate-p)
  ;; (org-context-clock-api-set :predicate :update  'org-task-list-update-tasks) ;will be in api
  )


;; (progn ;; functions
;;   (setq org-task-associated-context-predicate-fns nil)

;;   (defun org-tasks-register-associated-to-context-predicate-function (fn)
;;     (add-to-list
;;      'org-task-associated-context-predicate-fns
;;      fn))

;;   (defun org-task-associated-context-org-file-predicate (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let ((org-file (org-context-clock-task-task-get-property task :task-clock-file)))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;        (if (and file org-file)
;;            (string-equal
;;             (file-truename file)
;;             (file-truename org-file))))))
;;   (org-tasks-register-associated-to-context-predicate-function 'org-task-associated-context-org-file-predicate)

;;   (defun org-task-associated-context-root-dir-predicate (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let ((root
;;            (org-context-clock-task-task-get-property task :ROOT)))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;         (if (and root file)
;;             (string-match
;;              (file-truename root)
;;              (file-truename file))))))
;;   (org-tasks-register-associated-to-context-predicate-function 'org-task-associated-context-root-dir-predicate)

;;   ;; (defun org-task-associated-context-xx-p (context task)
;;   ;;   )
;;   ;; (org-tasks-register-associated-to-context-predicate-function 'org-task-associated-context-xx-p)
;;   ;; )
;;   )


(provide 'org-context-clock-assoc-predicate)
;;; org-context-clock-assoc-predicate.el ends here
