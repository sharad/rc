;;; org-context-clocking-assoc-predicate.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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

;; "Org entries associated to file predicate functions"

(defvar org-entry-associated-file-predicate-fns nil)

(progn ;; api

  (defun org-entries-associated-to-file-by-predicate-p (file)
    (let ((task-infos (org-entry-list-update-task-infos))
          (matched '()))
      (dolist (fn org-entry-associated-file-predicate-fns matched)
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

  (defun org-entry-associated-to-file-by-predicate-p (task-info file)
    (if file
        (some
         '(lambda (fn) (funcall fn file task-info))
         org-entry-associated-file-predicate-fns)))

  (org-context-clocking-api-set :predicate :entries 'org-entries-associated-to-file-by-predicate-p)
  (org-context-clocking-api-set :predicate :entry   'org-entry-associated-to-file-by-predicate-p)
  (org-context-clocking-api-set :predicate :update  'org-entry-list-update-task-infos))


(progn ;; functions
  (setq org-entry-associated-file-predicate-fns nil)

  (defun org-entries-register-associated-to-file-predicate-function (fn)
    (add-to-list
     'org-entry-associated-file-predicate-fns
     fn))

  (defun org-entry-associated-file-org-file-predicate (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let ((org-file (org-entry-task-info-get-property task-info :task-clock-file)))
      (if (and file org-file)
          (string-equal
           (file-truename file)
           (file-truename org-file)))))
  (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-org-file-predicate)

  (defun org-entry-associated-file-root-dir-predicate (file task-info)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let ((root
           (org-entry-task-info-get-property task-info :ROOT)))
      (if (and root file)
          (string-match
           (file-truename root)
           (file-truename file)))))
  (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-root-dir-predicate)

  ;; (defun org-entry-associated-file-xx-p (file task-info)
  ;;   )
  ;; (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-xx-p)
  ;; )
  )


(provide 'org-context-clocking-assoc-predicate)
;;; org-context-clocking-assoc-predicate.el ends here
