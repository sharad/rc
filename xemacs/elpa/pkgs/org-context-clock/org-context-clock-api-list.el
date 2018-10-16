;;; org-context-clock-api-list.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(defvar org-context-clock-entry-list-files nil "org entry task files")

;;;###autoload
(defun org-context-clock-setup-entry-list-files (files)
  (setq
   org-context-clock-entry-list-files files))

;; "org tasks access api for list org"
(defvar org-context-clock-entry-list-tasks nil "org entry task infos")

(defun org-context-clock-entry-list-build (collector files)
  (let ()
    (remove nil
            (org-map-entries
             collector
             t
             files))))

(defun org-context-clock-entry-list-collect-tasks (files)
  (unless org-context-clock-entry-list-files
    (setq
     org-context-clock-entry-list-files (remove nil (delete-dups files))))
  (org-context-clock-entry-list-build 'org-entry-collect-task files))

;; API (org-context-clock-api-set :predicate :update  'org-entry-list-update-tasks)
(defun org-context-clock-entry-list-update-tasks (&optional force)
  (interactive "P")
  (unless (and (not force)
               org-context-clock-entry-list-tasks)
    (setq org-context-clock-entry-list-tasks
          ;; (org-context-clock-entry-list-collect-tasks (org-all-task-files))
          (org-context-clock-entry-list-collect-tasks org-context-clock-entry-list-files)))
  org-context-clock-entry-list-tasks)
(org-context-clock-access-api-set :list :update  'org-context-clock-entry-list-update-tasks)


;; API (org-context-clock-api-set :predicate :update  'org-entry-list-update-tasks)
(defun org-context-clock-entry-list-update-files (&optional force)
  (interactive "P")
  (unless (and (not force)
               org-context-clock-entry-list-tasks)
    (setq org-context-clock-entry-list-files
          (org-context-clock-entry-list-collect-files (org-all-task-files))))
  org-context-clock-entry-list-files)
(org-context-clock-access-api-set :list :files  'org-context-clock-entry-list-update-files)


;; API (org-context-clock-api-set :predicate :entryp   'org-entry-associated-to-context-by-predicate-p)
(defun org-context-clock-list-matching-tasks (context)
  (lexical-let ((tasks (org-context-clock-entry-list-update-tasks))
                (context context))
    (remove-if-not
     #'(lambda (task)
         (> (funcall org-context-clock-api-task-associated-to-context-p task context) 0))
     tasks)))
(org-context-clock-access-api-set :list :tasks  'org-context-clock-list-matching-tasks)

(defun org-context-clock-list-build-dyntaskpl (task context)
  (list
   :rank (funcall org-context-clock-api-task-associated-to-context-p task context)
   :task task
   :marker (org-context-clock-task-get-property task :task-clock-marker)))
(org-context-clock-access-api-set :list :dyntaskpl  'org-context-clock-list-build-dyntaskpl)

(defun org-context-clock-list-matching-dyntaskpls (context)
  (lexical-let ((tasks (org-context-clock-entry-list-update-tasks))
                (context context))
    (remove-if-not #'(lambda (dyntaskpl) (> (car dyntaskpl) 0))
                   (mapcar #'(lambda (task)
                               (org-context-clock-build-dyntaskpl task context))
                           tasks))))
(org-context-clock-access-api-set :list :dyntaskpls  'org-context-clock-list-matching-dyntaskpls)

(defun org-context-clock-list-dyntaskpl-print (dyntaskpl heading)
  (let ((task (plist-get dyntaskpl :task)))
    (format "[%4d] %s"
            (plist-get dyntaskpl :rank)
            (org-context-clock-fontify-like-in-org-mode task))))
(org-context-clock-access-api-set :list :dyntaskplprint  'org-context-clock-list-dyntaskpl-print)

(provide 'org-context-clock-api-list)
;;; org-context-clock-api-list.el ends here
