;;; org-context-clock-assoc-key.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

;; "Org tasks associated to context key functions on recursive taskinfos"
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


(require 'org-context-clock-api-recursive)

;; (defvar org-context-clock-task-associated-context-key-fns nil
;;   "property list of KEY and API FUNCTIONS for key based association")

(progn ;; api
  ;; (defun org-context-clock-tasks-associated-to-context-by-keys (context)
  ;;   "Retun org TASK-INFO tasks for FILE which are associated based on list of functions for keys applied by org-context-clock-task-associated-TO-FILE-BY-KEYS-P"
  ;;   (let ((tasks (org-context-clock-task-tree-update-tasks))
  ;;         (matched '()))
  ;;     (org-context-clock-debug "org-context-clock-tasks-associated-to-context-by-keys: BEFORE matched %s[%d]" matched (length matched))
  ;;     (org-context-clock-tree-mapc-tasks
  ;;      #'(lambda (task args)
  ;;          (let ((rank
  ;;                 (org-context-clock-task-associated-to-context-by-keys-p task args)))
  ;;            (unless rank (error "org-context-clock-tasks-associated-to-context-by-keys[lambda]: rank is null"))
  ;;            (when (> rank 0)
  ;;              (push task matched)
  ;;              (org-context-clock-debug "org-context-clock-tasks-associated-to-context-by-keys[lambda]: task %s MATCHED RANK %d"
  ;;                       (org-context-clock-task-get-heading task)
  ;;                       (length matched)))))
  ;;      tasks
  ;;      context)

  ;;     (org-context-clock-debug "org-context-clock-tasks-associated-to-context-by-keys: AFTER matched %s[%d]" "matched" (length matched))

  ;;     matched))


  (defun org-context-clock-task-associated-to-context-by-keys-p (task context)
    "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
    (if context
        (if (> (org-context-clock-tasks-associated-key-fn-value :status task context) -20)
            (let ((rank
                   (+
                    (org-context-clock-tasks-associated-key-fn-value :timebeing task context)
                    (org-context-clock-tasks-associated-key-fn-value :root task context)
                    ;; (org-context-clock-tasks-associated-key-fn-value :org-file task context)
                    (org-context-clock-tasks-associated-key-fn-value :task-key task context)
                    (org-context-clock-tasks-associated-key-fn-value :heading-level task context))))
              rank)
            -20)
        0))

  ;; (org-context-clock-api-set :keys :tasks 'org-context-clock-tasks-associated-to-context-by-keys) ;will be in api
  (org-context-clock-assoc-api-set :keys :taskp  'org-context-clock-task-associated-to-context-by-keys-p)
  ;; (org-context-clock-api-set :keys :update  'org-context-clock-task-tree-update-tasks) ;will be in api
  )



;; (progn ;; REGISTRATION MACRO To add key property and functions list to ORG-CONTEXT-CLOCKING-TASK-ASSOCIATED-FILE-KEY-FNS
;;   (setq org-context-clock-task-associated-context-key-fns nil)
;;   (message "NOTE: org-context-clock-task-associated-context-key-fns made to nil")

;;   (progn ;; macros and accessor

;;     (defun org-context-clock-tasks-register-associated-to-context-key-function (key fn)
;;       (setq
;;        org-context-clock-task-associated-context-key-fns
;;        (plist-put
;;         org-context-clock-task-associated-context-key-fns key fn)))

;;     (eval-when-compile                  ;; TODO: auto generate name from KEY
;;       (defmacro defassoc-context-key (name key args &rest body)
;;         "Registration macro to add key property and functions list to ORG-TASK-ASSOCIATED-FILE-KEY-FNS"
;;         `(progn
;;            (defun ,name ,args
;;              ,@body)
;;            (org-context-clock-tasks-register-associated-to-context-key-function ,key ',name))))

;;     (put 'defassoc-context-key 'lisp-indent-function 3)
;;     (defun org-context-clock-tasks-associated-key-function (key)
;;       (plist-get org-context-clock-task-associated-context-key-fns key))
;;     (defun org-context-clock-tasks-associated-key-fn-value (key task context)
;;       (let ((keyfn (org-context-clock-tasks-associated-key-function key)))
;;         (if keyfn
;;             (let ((rank (funcall keyfn task context)))
;;               (unless (numberp rank)
;;                 (error "org-context-clock-tasks-associated-key-fn-value: fun %s returning nonnumeric %s for context %s for task %s"
;;                        keyfn
;;                        rank
;;                        context
;;                        (org-context-clock-task-get-heading task)))
;;               (org-context-clock-debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s MATCHED %d rank"
;;                        (org-context-clock-task-get-heading task)
;;                        key
;;                        rank)
;;               rank)
;;             (progn
;;               (org-context-clock-debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s kyfn is %s so how can match %d rank"
;;                        (org-context-clock-task-get-heading task)
;;                        key
;;                        keyfn
;;                        0)
;;               0)))))

;;   (defassoc-context-key org-task-associated-context-org-file-key :org-file (task context)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let ((org-file (org-context-clock-task-get-property task :task-clock-file)))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;         (if (and file org-file
;;                  (string-equal
;;                   (file-truename file)
;;                   (file-truename org-file)))
;;             10
;;             0))))

;;   (defassoc-context-key org-task-associated-context-root-dir-key :root (task context)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((root
;;             (org-context-clock-task-get-property task :ROOT))
;;            (root (if root (file-truename root))))
;;       (let* ((file (plist-get context :file))
;;              (file (if file (file-truename file))))
;;        (if root
;;            (progn
;;              (org-context-clock-debug "task %s root %s" (org-context-clock-task-get-heading task) root)
;;              (org-context-clock-debug "task %s file %s" (org-context-clock-task-get-heading task) file))
;;            (org-context-clock-debug "task %s root %s not present."
;;                                     (org-context-clock-task-get-heading task) root))
;;        (if (and root file
;;                 (string-match root file))
;;            (length root)
;;            0))))

;;   (defassoc-context-key org-task-associated-context-status-key :status (task context)
;;     "Predicate funtion to check if context matches to task's status attribute."
;;     (let* ((status
;;             (org-context-clock-task-get-property task 'status)))
;;       (if (string-equal status "CLOSED") -30 0)))

;;   (defassoc-context-key org-task-associated-context-task-key :task-key (task context)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((key (org-context-clock-task-get-property task :KEY)))
;;       (if key (string-to-number key) 0)))

;;   (defassoc-context-key org-task-associated-context-level-key :heading-level (task context)
;;     "Predicate funtion to check if context matches to task's file attribute."
;;     (let* ((level
;;             (org-context-clock-task-get-property task :task-clock-level)))
;;       (if level level 0)))

;;   (defassoc-context-key org-task-associated-context-timebeing-key :timebeing (task context)
;;     (let ((timebeing (org-context-clock-task-get-property task :TIMEBEING)))
;;       (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
;;             (clocked-time   (org-context-clock-task-get-property task :task-clock-clock-sum)))
;;         (if (and
;;              (numberp clocked-time)
;;              (numberp timebeing-time)
;;              (> timebeing-time clocked-time))
;;             (- timebeing-time clocked-time)
;;             0))))

;;   ;; (defassoc-context-key org-task-associated-context-current-clock-key :current-clock (task context)
;;   ;;   "Predicate funtion to check if context matches to task's file attribute."
;;   ;;   (let* ((task-marker
;;   ;;           (org-context-clock-task-get-property task :task-clock-marker)))
;;   ;;     (if (and
;;   ;;          org-clock-marker
;;   ;;          task-marker
;;   ;;          (equal
;;   ;;           (marker-buffer org-clock-marker)
;;   ;;           (marker-buffer task-marker))
;;   ;;          (equal
;;   ;;           (marker-position org-clock-marker)
;;   ;;           (marker-position task-marker)))
;;   ;;         100
;;   ;;         0)))
;;   )

(provide 'org-context-clock-assoc-key)
;;; org-context-clock-assoc-key.el ends here
