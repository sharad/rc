;;; org-context-clock-api-list.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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
  (org-context-clock-entry-list-build 'org-entry-collect-task files))

;; API (org-context-clock-api-set :predicate :update  'org-entry-list-update-tasks)
(defun org-context-clock-entry-list-update-tasks (&optional force)
  (interactive "P")
  (unless (and (not force)
               org-context-clock-entry-list-tasks)
    (setq org-context-clock-entry-list-tasks
          (org-context-clock-entry-list-collect-tasks (org-all-task-files))))
  org-context-clock-entry-list-tasks)
(org-context-clock-access-api-set :list :update  'org-context-clock-entry-list-update-tasks)

;; API (org-context-clock-api-set :predicate :entryp   'org-entry-associated-to-context-by-predicate-p)
(defun org-context-clock-list-matching-tasks (context)
  (lexical-let ((tasks (org-context-clock-entry-list-update-tasks))
                (context context))
    (remove-if-not
     #'(lambda (task)
         (funcall org-context-clock-api-entry-associated-to-context-p task context))
     tasks)))
(org-context-clock-access-api-set :list :match  'org-context-clock-list-matching-tasks)

(provide 'org-context-clock-api-list)
;;; org-context-clock-api-list.el ends here
