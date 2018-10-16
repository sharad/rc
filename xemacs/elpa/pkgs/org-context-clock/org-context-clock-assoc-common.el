;;; org-context-clock-assoc-common.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

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


(defvar org-context-clock-key-operation-functions nil
  "Alist of operation plist of key functions accepting (key task context args) arguments.")

(defmacro define-keyop-functions (name key args &rest bindings)
  (let (binds
        (name-string (symbol-name name)))
    (dolist (binding bindings)
      (message "binding %s" binding)
      (let* ((operation (car binding))
             (fn (intern (concat name-string "-" (symbol-name operation)))))
        (message "fn %s" fn)
        (push
         `(progn
            (defun ,fn (key ,@args)
              ,@(cdr binding))
            (unless (assoc ,key org-context-clock-key-operation-functions)
              (push (cons ,key nil) org-context-clock-key-operation-functions))
            (setf
             (cdr (assoc ,key org-context-clock-key-operation-functions))
             (plist-put
              (cdr (assoc ,key org-context-clock-key-operation-functions))
              ,(intern (concat ":" (symbol-name operation)))
              #',fn)))
         binds)))
    `(progn
       ,@binds)))
(put 'def-task-functions 'lisp-indent-function 3)

(defun org-context-clock-key-fun (key operation)
  (let ((keyfns (assoc key org-context-clock-key-operation-functions)))
    (plist-get (cdr keyfns) operation)))

(defun org-context-clock-keys-with-operation (operation context)
  (remove-if-not
   #'(lambda (key)
       ;; (message "%s: %s" key operation)
       (org-context-clock-key-fun key operation))
   (mapcar 'car org-context-clock-key-operation-functions)))

;; TODO rename it with proper name.
(defun org-context-clock-tasks-associated-key-fn-value (key task context)
  (let ((keyfn (org-context-clock-key-fun key :associator)))
    (if keyfn
        (let ((rank (funcall keyfn key task context)))
          (unless (numberp rank)
            (error "org-context-clock-tasks-associated-key-fn-value: fun %s returning nonnumeric %s for context %s for task %s"
                   keyfn
                   rank
                   context
                   (org-context-clock-task-get-heading task)))
          (org-context-clock-debug :debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s MATCHED %d rank"
                                   (org-context-clock-task-get-heading task)
                                   key
                                   rank)
          rank)
        (progn
          (org-context-clock-debug :debug "org-context-clock-tasks-associated-key-fn-value: task %s key %s kyfn is %s so how can match %d rank"
                                   (org-context-clock-task-get-heading task)
                                   key
                                   keyfn
                                   0)
          0))))

;; (define-keyop-functions test :file (task context args)
;;   (associator
;;    "Test"
;;    (let ()
;;      "Hello"))
;;   (getter
;;    "Test"
;;    (let ()
;;      "Yes")))

(provide 'org-context-clock-assoc-common)
;;; org-context-clock-assoc-common.el ends here
