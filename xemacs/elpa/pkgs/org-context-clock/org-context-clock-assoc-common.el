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


'(
  (defmacro defassoc-context-key (name key context task args task-key-conext-associator key-value-getter)
    "Registration macro to add key property and functions list to ORG-TASK-ASSOCIATED-FILE-KEY-FNS"
    (let* ((name-string ,(symbol-name name))
           (task-key-conext-matcher-fn (intern (concat name-string "-associate")))
           (key-value-getter-fn (intern (concat name-string "-get"))))
      `(progn
         (defun ,task-key-conext-matcher-fn (,key ,task ,context)
           ,task-key-conext-matcher)
         (defun ,key-value-getter-fn (,key ,task ,context)
           ,key-value-getter)
         (org-context-clock-tasks-register-associated-to-context-key-function ,key ',name))))

  (lambda (prop context &rest args)
    (let* ((file (if context (plist-get context :file)))
           (dir (if (stringp file) (file-name-directory file) default-directory))
           (prompt (concat prop ": ")))
      (ido-read-directory-name
       prompt
       dir dir)))

  (defassoc-context-key org-task-associated-context-root-dir :root context task ()
                        "Predicate funtion to check if context matches to task's file attribute."
                        (let* ((root
                                (org-context-clock-task-get-property task :ROOT))
                               (root (if root (file-truename root))))
                          (let* ((file (plist-get context :file))
                                 (file (if file (file-truename file))))
                            (if root
                                (progn
                                  (org-context-clock-debug "task %s root %s" (org-context-clock-task-get-heading task) root)
                                  (org-context-clock-debug "task %s file %s" (org-context-clock-task-get-heading task) file))
                                (org-context-clock-debug "task %s root %s not present."
                                                         (org-context-clock-task-get-heading task) root))
                            (if (and root file
                                     (string-match root file))
                                (length root)
                                0)))

                        (let* ((file (if context (plist-get context :file)))
                               (dir (if (stringp file) (file-name-directory file) default-directory))
                               (prompt (concat prop ": ")))
                          (ido-read-directory-name
                           prompt
                           dir dir))))


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

'(testing

  (defmacro mtest (x)
    `(list ',(intern (concat (symbol-name x) "-key"))))


  (defmacro mtest1 (x)
    (symbol-name x))

  (mtest y))
