;;; org-context-clock-assoc-rank.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

;; "Org tasks associated to file rank functions"
;; TODO: matching should be merit based.
;; TODO: logical AND OR method should be possible in match-fn results
;; TODO: exclusion fecelities also should be present.
'(
  '(matches
    '(file based)x
    '(dir based -merit) x
    '(status based) x
    '(user input based)
    '(config based) x
    '(time based recently opened)
    '(heading level based)))




;; (defvar org-context-clock-task-associated-context-rank-fns nil)

(progn ;; api

  ;; (defun org-context-clock-tasks-associated-to-context-by-rank (context)
  ;;   (let ((tasks (org-task-list-update-tasks))
  ;;         (matched '()))
  ;;     (dolist (fn org-context-clock-task-associated-context-rank-fns matched)
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

  (defun org-context-clock-task-associated-to-context-by-rank-p (task context)
    (if context
        (apply '+
               (mapcar
                '(lambda (fn)
                  (funcall fn context task))
                org-context-clock-task-associated-context-rank-fns))
        0))

  ;; (org-context-clock-api-set :rank :tasks 'org-context-clock-tasks-associated-to-context-by-rank) ;will be in api
  (org-context-clock-assoc-api-set :rank :taskp   'org-context-clock-task-associated-to-context-by-rank-p)
  ;; (org-context-clock-api-set :rank :update  'org-task-list-update-tasks) ;will be in api
  )

;; (progn ;; functions
;;   (setq org-context-clock-task-associated-context-rank-fns nil)

;;   (defun org-tasks-register-associated-to-context-rank-function (fn)
;;     (add-to-list
;;      'org-context-clock-task-associated-context-rank-fns
;;      fn))

;;   (defun org-context-clock-task-associated-context-org-file-rank (context task)
;;     "Predicate funtion to check if file matches to task's file attribute."
;;     (let ((org-file (org-context-clock-task-get-property task :task-clock-file)))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;       (if (string-equal (file-truename file) (file-truename
;;             org-file))
;;           10
;;           0))))
;;   (org-tasks-register-associated-to-context-rank-function 'org-context-clock-task-associated-context-org-file-rank)

;;   (defun org-context-clock-task-associated-context-root-dir-rank (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((root
;;             (org-context-clock-task-get-property task :ROOT))
;;            (root (if root (file-truename root))))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;         (if (and
;;              root
;;              (string-match root file))
;;             (length root)
;;             0))))
;;   (org-tasks-register-associated-to-context-rank-function 'org-context-clock-task-associated-context-root-dir-rank)

;;   (defun org-context-clock-task-associated-context-status-rank (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((status
;;             (org-context-clock-task-get-property task 'status)))
;;       (if (string-equal status "CLOSED") -30 0)))
;;   (org-tasks-register-associated-to-context-rank-function 'org-context-clock-task-associated-context-status-rank)

;;   (defun org-context-clock-task-associated-context-task-rank (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((rank
;;             (org-context-clock-task-get-property task :RANK)))
;;       (if rank (string-to-number rank) 0)))
;;   (org-tasks-register-associated-to-context-rank-function 'org-context-clock-task-associated-context-task-rank)

;;   (defun org-context-clock-task-associated-context-level-rank (context task)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((level
;;             (org-context-clock-task-get-property task :task-clock-level)))
;;       level))
;;   (org-tasks-register-associated-to-context-rank-function 'org-context-clock-task-associated-context-level-rank))



(provide 'org-context-clock-assoc-rank)
;;; org-context-clock-assoc-rank.el ends here
