;;; org-context-clocking-assoc-rank.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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

;; "Org entries associated to file rank functions"
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




(defvar org-entry-associated-file-rank-fns nil)

(progn ;; api
  (defun org-entries-associated-to-file-by-rank (file)
    (let ((task-infos (org-entry-list-update-task-infos))
          (matched '()))
      (dolist (fn org-entry-associated-file-rank-fns matched)
        (let ((partitions
               (reduce (lambda (task-info result)
                         (if (funcall fn file task-info)
                             (push task-info (first  result))
                             (push task-info (second result)))
                         result)
                       task-infos
                       :initial-value (list nil nil)
                       :from-end t)))
          (setq
           task-infos (second partitions)
           matched    (append matched (first partitions)))))))
  (defun org-entry-associated-to-file-by-rank-p (task-info file)
    (if file
        (apply '+
               (mapcar
                '(lambda (fn)
                  (funcall fn file task-info))
                org-entry-associated-file-rank-fns))
        0))

  (org-context-clocking-api-set :rank :entries 'org-entries-associated-to-file-by-rank)
  (org-context-clocking-api-set :rank :entryp   'org-entry-associated-to-file-by-rank-p)
  (org-context-clocking-api-set :rank :update  'org-entry-list-update-task-infos))

(progn ;; functions
  (setq org-entry-associated-file-rank-fns nil)

  (defun org-entries-register-associated-to-file-rank-function (fn)
    (add-to-list
     'org-entry-associated-file-rank-fns
     fn))

  (defun org-entry-associated-file-org-file-rank (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (if (string-equal
         (file-truename file)
         (file-truename
          (org-entry-task-info-get-property task-info :task-clock-file)))
        10
        0))
  (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-org-file-rank)

  (defun org-entry-associated-file-root-dir-rank (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((root
            (org-entry-task-info-get-property task-info :ROOT))
           (root (if root (file-truename root))))
      (if (and
           root
           (string-match root file))
          (length root)
          0)))
  (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-root-dir-rank)

  (defun org-entry-associated-file-status-rank (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((status
            (org-entry-task-info-get-property task-info 'status)))
      (if (string-equal status "CLOSED") -30 0)))
  (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-status-rank)

  (defun org-entry-associated-file-task-rank (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((rank
            (org-entry-task-info-get-property task-info :RANK)))
      (if rank (string-to-number rank) 0)))
  (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-task-rank)

  (defun org-entry-associated-file-level-rank (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((level
            (org-entry-task-info-get-property task-info :task-clock-level)))
      level))
  (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-level-rank))



(provide 'org-context-clocking-assoc-rank)
;;; org-context-clocking-assoc-rank.el ends here
