;;; org-context-clock-assoc-rank.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

;; "Org tasks associated to file rank functions"
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



;; (defun org-context-clock-task-associated-to-context-by-rank-p (task context)
;;   (if context
;;       (apply '+
;;              (mapcar
;;               '(lambda (fn)
;;                 (funcall fn context task))
;;
;;               ;; BUG TODO below one is free variable
;;               org-context-clock-task-associated-context-rank-fns
;;               ))
;;       0))


(defun org-context-clock-task-associated-to-context-by-rank-p (task context)
  (if context
      (apply '+
             (mapcar
              #'(lambda (rankkey)
                  (org-context-clock-tasks-associated-key-fn-value rankkey task context))
              (mapcar 'car org-context-clock-key-operation-functions)))
      0))

(org-context-clock-assoc-api-set :rank :taskp   'org-context-clock-task-associated-to-context-by-rank-p)

(provide 'org-context-clock-assoc-rank)
;;; org-context-clock-assoc-rank.el ends here
