;;; org-context-clocking-api-list.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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


;; "org entries access api for list org"
(defvar org-entry-list-task-infos nil "org entry task infos")

(defun org-entry-list-build (collector files)
  (let ()
    (remove nil
            (org-map-entries
             collector
             t
             files))))

(defun org-entry-list-collect-task-infos (files)
  (org-entry-list-build 'org-entry-collect-task-info files))

(defun org-entry-list-update-task-infos (&optional force)
  (interactive "P")
  (unless (and (not force)
               org-entry-list-task-infos)
    (setq org-entry-list-task-infos
          (org-entry-list-collect-task-infos (org-all-task-files))))
  org-entry-list-task-infos)

(provide 'org-context-clocking-api-list)
;;; org-context-clocking-api-list.el ends here
