;;; planner-bugz.el --- Planner Bugzilla Interface

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <yes>
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

(eval-when-compile
  '(require 'cl))


;; '(open inprogress completed cancelled delegated pending )

(task-status-add-maps 'bugz
                      '((inprogress . "ASSIGNED")
                        (open         "OPENED" "NEW" "REOPENED")
                        (pending    . "NEEDINFO")
                        (completed  . "RESOLVED")
                        (cancelled  . "WONTFIX")))

(defvar planner-bugz-regex "^\\(\\(b[0-9]\+\\) .\+\\) \{\{Tasks:[0-9]\+\}\}\\(\s\+[[][[]\\)\?" "Planner Bugz Regex")
(defvar planner-bugz-format "b%d %s [[%s][url]]" "Planner Bugz Format")


(defun bugz-to-planner-status (status)
  (task-src-status-to-trg-status 'bugz status 'planner))

(defun planner-bugzilla-bug-to-task-name (bug)
  (let* ((id      (cdr (assoc "id" bug)))
         (summary (cdr (or
                        (assoc "summary" bug)
                        (assoc "short_desc" bug))))
         (url (or (concat bugz-showbug-url (number-to-string id))
                  (cdr (assoc "_bugz-url" bug)) bugz-url)))
    (format planner-bugz-format id summary url)))

(defun planner-bugzilla-bugtask-exist-in-page (bug &optional page)
  (find (cdr (assoc "id" bug))
        (planner-bugzilla-find-bugtasks-in-page page)
        :key #'planner-bugzilla-task-to-bugid
        :test #'equal))

(defun planner-bugzilla-bugtask-differ (bug &optional page)
  (let ((task (planner-bugzilla-bugtask-exist-in-page bug page)))
    (if (and task
             (string-equal
              (bugz-to-planner-status (cdr
                                       (or
                                        (assoc "bug_status" bug)
                                        (assoc "status" bug))))
              (planner-task-status task)))
        task)))

(defun planner-bugzilla-bugtask-update (bug &optional page)
  (let ((differed-task (planner-bugzilla-bugtask-differ bug page)))
    (if (nth 4 differed-task)
        (planner-task-change-status
         (nth 4 differed-task)
         (lambda ()
           (planner-mark-task (bugz-to-planner-status (cdr
                                                       (or
                                                        (assoc "bug_status" bug)
                                                        (assoc "status" bug))))))
         page))))

(defun planner-bugzilla-create-bug-to-task (bug &optional plan-page date)
  (unless (planner-bugzilla-bugtask-exist-in-page bug plan-page)
    (planner-create-task
     (planner-bugzilla-bug-to-task-name bug)
     (let ((planner-expand-name-favor-future-p
            (or planner-expand-name-favor-future-p
                planner-task-dates-favor-future-p)))
       (planner-read-date))
     nil plan-page
     (bugz-to-planner-status (cdr (or
                                   (assoc "bug_status" bug)
                                   (assoc "status" bug)))))))

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

(defun planner-bugzilla-update-existing-bugtasks (page &optional how)
  ;; how could be 'ask, 'force
  (dolist (task (planner-bugzilla-find-bugtasks-in-page page))
    (planner-bugzilla-bugtask-update task page)))

;;;###autoload
(defun planner-bugzilla-fetch-new-bugtask (&optional criteria page)
  ;; add url username used in bug list.
  (interactive
   (list
    t
    (planner-read-non-date-page (planner-file-alist))))
  (dolist (bug (bugzilla-search-bugs '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url") criteria))
    (planner-bugzilla-create-bug-to-task bug page t)))


;;;###autoload
(defun planner-bugzilla-get-new-bugtask (&optional criteria page)
  ;; add url username used in bug list.
  (interactive
   (list
    t
    (planner-read-non-date-page (planner-file-alist))))
  (dolist (bug (bugzilla-get-bugs '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url") criteria))
    (planner-bugzilla-create-bug-to-task bug page t)))

(when nil
 (let ((bug (bugz-method 'Bug.get "bugs" '(("ids" 37026 )))))
  (planner-bugzilla-bug-to-task-name bug)
  (bugz-to-planner-status (cdr (assoc "bug_status" bug)))
  ;; (setq testbug bug)
  ;; (assoc "bug_status" bug)
  ;; (kill-new (planner-bugzilla-bug-to-task-name (first (bugzilla-get-bugs '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url") '(("ids" 39437))))))
  )

;; (tree-node testbug "AAAinternals" "bug_status" :test 'string-equal)
;; (tree-node testbug "AAAinternals" "bug_status")
;; (tree-leaves testbug 1)
)

(provide 'planner-bugz)
;;; planner-bugz.el ends here
