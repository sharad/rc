;;; org-context-clock-assoc-common.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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


(defvar org-context-clock-task-associated-context-key-fns nil
  "Assoc api func")


(setq org-context-clock-task-associated-context-key-fns nil)
(message "NOTE: org-context-clock-task-associated-context-key-fns made to nil")



(defun org-context-clock-tasks-register-associated-to-context-key-function (key fn)
  (setq
   org-context-clock-task-associated-context-key-fns
   (plist-put
    org-context-clock-task-associated-context-key-fns key fn)))

(eval-when-compile                  ;; TODO: auto generate name from KEY
  (defmacro defassoc-context-key (name key args &rest body)
    "Registration macro to add key property and functions list to ORG-TASK-ASSOCIATED-FILE-KEY-FNS"
    `(progn
       (defun ,name ,args
         ,@body)
       (org-context-clock-tasks-register-associated-to-context-key-function ,key ',name))))
(put 'defassoc-context-key 'lisp-indent-function 3)

(defun org-context-clock-tasks-associated-key-function (key)
  (plist-get org-context-clock-task-associated-context-key-fns key))
(defun org-context-clock-tasks-associated-key-fn-value (key task context)
  (let ((keyfn (org-context-clock-tasks-associated-key-function key)))
    (if keyfn
        (let ((rank (funcall keyfn task context)))
          (unless (numberp rank)
            (error "org-context-clock-tasks-associated-key-fn-value: fun %s returning nonnumeric %s for context %s for task %s"
                   keyfn
                   rank
                   context
                   (org-context-clock-task-get-heading task)))
          (org-context-clock-debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s MATCHED %d rank"
                                   (org-context-clock-task-get-heading task)
                                   key
                                   rank)
          rank)
        (progn
          (org-context-clock-debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s kyfn is %s so how can match %d rank"
                                   (org-context-clock-task-get-heading task)
                                   key
                                   keyfn
                                   0)
          0))))


(provide 'org-context-clock-assoc-common)
;;; org-context-clock-assoc-common.el ends here
