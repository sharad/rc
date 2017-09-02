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




(defvar org-context-clock-entry-associated-context-plist-rank-fns nil)

(progn ;; api

  ;; (defun org-context-clock-entries-associated-to-context-plist-by-rank (context-plist)
  ;;   (let ((task-infos (org-entry-list-update-task-infos))
  ;;         (matched '()))
  ;;     (dolist (fn org-context-clock-entry-associated-context-plist-rank-fns matched)
  ;;       (let ((partitions
  ;;              (reduce (lambda (task-info result)
  ;;                        (if (funcall fn context-plist task-info)
  ;;                            (push task-info (first  result))
  ;;                            (push task-info (second result)))
  ;;                        result)
  ;;                      task-infos
  ;;                      :initial-value (list nil nil)
  ;;                      :from-end t)))
  ;;         (setq
  ;;          task-infos (second partitions)
  ;;          matched    (append matched (first partitions)))))))

  (defun org-context-clock-entry-associated-to-context-plist-by-rank-p (task-info context-plist)
    (if context-plist
        (apply '+
               (mapcar
                '(lambda (fn)
                  (funcall fn context-plist task-info))
                org-context-clock-entry-associated-context-plist-rank-fns))
        0))

  (org-context-clocking-api-set :rank :entries 'org-context-clock-entries-associated-to-context-plist-by-rank) ;will be in api
  (org-context-clocking-api-set :rank :entryp   'org-context-clock-entry-associated-to-context-plist-by-rank-p)
  (org-context-clocking-api-set :rank :update  'org-entry-list-update-task-infos) ;will be in api
  )

(progn ;; functions
  (setq org-context-clock-entry-associated-context-plist-rank-fns nil)

  (defun org-entries-register-associated-to-context-plist-rank-function (fn)
    (add-to-list
     'org-context-clock-entry-associated-context-plist-rank-fns
     fn))

  (defun org-context-clock-entry-associated-context-plist-org-file-rank (context-plist task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let ((org-file (org-context-clocking-entry-task-info-get-property task-info :task-clock-file)))
      (let* ((file (plist-get context-plist :file))
             (file (if file (file-truename file))))
      (if (string-equal (file-truename file) (file-truename
            org-file))
          10
          0))))
  (org-entries-register-associated-to-context-plist-rank-function 'org-context-clock-entry-associated-context-plist-org-file-rank)

  (defun org-context-clock-entry-associated-context-plist-root-dir-rank (context-plist task-info)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((root
            (org-context-clocking-entry-task-info-get-property task-info :ROOT))
           (root (if root (file-truename root))))
      (let* ((file (plist-get context-plist :file))
             (file (if file (file-truename file))))
        (if (and
             root
             (string-match root file))
            (length root)
            0))))
  (org-entries-register-associated-to-context-plist-rank-function 'org-context-clock-entry-associated-context-plist-root-dir-rank)

  (defun org-context-clock-entry-associated-context-plist-status-rank (context-plist task-info)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((status
            (org-context-clocking-entry-task-info-get-property task-info 'status)))
      (if (string-equal status "CLOSED") -30 0)))
  (org-entries-register-associated-to-context-plist-rank-function 'org-context-clock-entry-associated-context-plist-status-rank)

  (defun org-context-clock-entry-associated-context-plist-task-rank (context-plist task-info)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((rank
            (org-context-clocking-entry-task-info-get-property task-info :RANK)))
      (if rank (string-to-number rank) 0)))
  (org-entries-register-associated-to-context-plist-rank-function 'org-context-clock-entry-associated-context-plist-task-rank)

  (defun org-context-clock-entry-associated-context-plist-level-rank (context-plist task-info)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((level
            (org-context-clocking-entry-task-info-get-property task-info :task-clock-level)))
      level))
  (org-entries-register-associated-to-context-plist-rank-function 'org-context-clock-entry-associated-context-plist-level-rank))



(provide 'org-context-clocking-assoc-rank)
;;; org-context-clocking-assoc-rank.el ends here
