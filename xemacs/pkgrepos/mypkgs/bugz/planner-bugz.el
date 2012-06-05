;;; planner-bugz.el --- Planner Bugzilla Interface

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
;; Keywords: lisp

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


(require 'planner-interface)
(require 'bugz)


'(open inprogress completed cancelled delegated pending )

(task-status-add-maps bugz
                      ((inprogress . "ASSIGNED")
                       (open     . "OPENED")
                       (completed  . "RESOLVED")
                       (cancelled  . "WONTFIX")))

(defvar planner-bugz-regex "^\\(\\(b[0-9]\+\\) .\+\\) \{\{Tasks:[0-9]\+\}\}\\(\s\+[[][[]\\)\?" "Planner Bugz Regex")
(defvar planner-bugz-format "b%d %s [[%s][url]]" "Planner Bugz Regex")


(defun bugz-to-planner-status (status)
  (task-src-status-to-trg-status 'bugz status 'planner))

(defun planner-bugzilla-bug-to-task-name (bug)
  (let ((id      (cdr (assoc "id" bug)))
        (summary (cdr (assoc "summary" bug)))
        (url (or (cdr (assoc "_bugz-url" bug)) bugz-url)))
    (format planner-bugz-format id summary url)))

(defun planner-bugzilla-bugtask-exist-in-page (bug &optional page)
  (find (cdr (assoc "id" bug))
        (planner-bugzilla-find-bugtasks-in-page page)
        :key #'planner-bugzilla-task-to-bugid
        :test #'equal))

(defun planner-bugzilla-bugtask-differ (bug &optioanl page)
  (let ((task (planner-bugzilla-bugtask-exist-in-page bug page)))
    (if (and task
             (string-equal
              (bugz-to-planner-status (cdr (assoc "status" bug)))
              (planner-task-status task)))
        task)))

(defun planner-bugzilla-bugtask-update (bug &optioanl page)
  (let ((differed-task (planner-bugzilla-bugtask-differ bug page)))
    (if (nth 4 differed-task)
        (planner-task-change-status
         (nth 4 differed-task)
         (lambda ()
           (planner-mark-task (bugz-to-planner-status (cdr (assoc "status" bug)))))
         page))))

(defun planner-bugzilla-create-bug-to-task (bug &optional page)
  (unless (planner-bugzilla-bugtask-exist-in-page bug page)
    (planner-create-task
     (planner-bugzilla-bug-to-task-name bug)
     nil nil page
     (bugz-to-planner-status (cdr (assoc "status" bug))))))

(defun planner-bugzilla-task-to-bugid (task)
  (let ((description (nth 4 task)))
   (if (string-match planner-bugz-regex description)
      (list (cons "id" (match-string 2 description)))
      (message "Not a bugzilla bug."))))

;; bulk.

(defun planner-bugzilla-find-bugtasks-in-page (page)
  (remove-if-not
   '(lambda (task)
     (string-match planner-bugz-regex (nth 4 task)))
   (planner-task-lists page)))

(defun planner-bugzilla-update-existing-bugtasks (&optional how)
  ;; how could be 'ask, 'force
  (dolist (task (planner-bugzilla-find-bugtasks-in-page page))
    (planner-bugzilla-bugtask-update task page)))

;;;###autoload
(defun planner-bugzilla-fetch-new-bugtask (&optional page)
  ;; add url username used in bug list.
  (interactive
   (list
    (planner-read-non-date-page (planner-file-alist))))
  (dolist (bug (bugzilla-search-bugs '("id" "summary" "status") t))
    (planner-bugzilla-create-bug-to-task bug page)))



(provide 'planner-bugz)
;;; planner-bugz.el ends here


;; (nth 4 (car (planner-task-lists "MyMIS")))


;; (planner-tasks-from-page "MyMIS")


;; ("MyMIS" "B" "1" "_" "Test {{Tasks:16}}" (" *temp*" "[[2012.06.02]]" "[[MyMIS]]" "[[TasksByProject]]") "[[2012.06.02]],[[MyMIS]],[[TasksByProject]]" "MyMIS" "2012.06.02")



