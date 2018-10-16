;;; org-context-clock-assoc-key.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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
(require 'org-context-clock-assoc-api)





;; HERE only create dyntaskpl by adding per associator key rank in org-context-clock-task-associated-to-context-by-keys-p
(defun org-context-clock-task-associated-to-context-by-keys-p (task context)
  "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
  ;; TODO add :current-clock also
  (if context
      (if (and
           (>= (org-context-clock-tasks-associated-key-fn-value :sub-tree task context) 0) ;IMPORTANT NOTE: only node tasks
           (>= (org-context-clock-tasks-associated-key-fn-value :status task context) 0))
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
(org-context-clock-assoc-api-set :keys :taskp  'org-context-clock-task-associated-to-context-by-keys-p)




(defun org-context-clock-task-associated-to-context-by-keys-p (task context)
  "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
  (if context
      (apply '+
             (mapcar
              #'(lambda (rankkey)
                  (org-context-clock-tasks-associated-key-fn-value rankkey task context))
              (mapcar 'car org-context-clock-key-operation-functions)))
      0))

(defun org-context-clock-task-associated-to-context-by-keys-newalt-p (task context)
  "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
  (if context
      (apply '+
             (mapcar
              #'(lambda (rankkey)
                 (org-context-clock-tasks-associated-key-fn-value rankkey task context))
              (mapcar 'car org-context-clock-key-operation-functions)))
      0))
(org-context-clock-assoc-api-set :keys :taskp  'org-context-clock-task-associated-to-context-by-keys-newalt-p)


(provide 'org-context-clock-assoc-key)
;;; org-context-clock-assoc-key.el ends here
