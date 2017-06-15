;;; org-context-clocking-assoc-key.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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

;; "Org entries associated to file key functions on recursive taskinfos"
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


(require 'org-context-clocking-api-recursive)

(defvar org-entry-associated-file-key-fns nil
  "property list of KEY and API FUNCTIONS for key based association")

(progn ;; api
  (defun org-entries-associated-to-file-by-keys (file)
    "Retun org TASK-INFO entries for FILE which are associated based on list of functions for keys applied by ORG-ENTRY-ASSOCIATED-TO-FILE-BY-KEYS-P"
    (let ((task-infos (org-entry-tree-update-task-infos))
          (matched '()))
      (message "org-entries-associated-to-file-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (tree-mapc-task-infos
       #'(lambda (task args)
           (let ((rank
                  (org-entry-associated-to-file-by-keys-p task args)))
             (unless rank (error "org-entries-associated-to-file-by-keys[lambda]: rank is null"))
             (when (> rank 0)
               (push task matched)
               (message "org-entries-associated-to-file-by-keys[lambda]: task %s MATCHED RANK %d"
                        (org-entry-task-info-get-heading task)
                        (length matched)))))
       task-infos
       file)

      (message "org-entries-associated-to-file-by-keys: AFTER matched %s[%d]" "matched" (length matched))

      matched))


  (defun org-entry-associated-to-file-by-keys-p (task-info file)
    "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
    (if file
        (if (> (org-entries-associated-key-fn-value :status task-info file) -20)
            (let ((rank
                   (+
                    (org-entries-associated-key-fn-value :timebeing task-info file)
                    (org-entries-associated-key-fn-value :root task-info file)
                    ;; (org-entries-associated-key-fn-value :org-file task-info file)
                    (org-entries-associated-key-fn-value :task-info-key task-info file)
                    (org-entries-associated-key-fn-value :heading-level task-info file))))
              rank)
            -20)
        0))

  (org-context-clocking-api-set :keys :entries 'org-entries-associated-to-file-by-keys)
  (org-context-clocking-api-set :keys :entryp  'org-entry-associated-to-file-by-keys-p)
  (org-context-clocking-api-set :keys :update  'org-entry-tree-update-task-infos))



(progn ;; Registration macro to add key property and functions list to ORG-ENTRY-ASSOCIATED-FILE-KEY-FNS
  (setq org-entry-associated-file-key-fns nil)
  (message "NOTE: org-entry-associated-file-key-fns made to nil")

  (progn ;; macros and accessor

    (defun org-entries-register-associated-to-file-key-function (key fn)
      (setq
       org-entry-associated-file-key-fns
       (plist-put
        org-entry-associated-file-key-fns key fn)))

    (eval-when-compile                  ;; TODO: auto generate name from KEY
      (defmacro defassoc-file-key (name key args &rest body)
        "Registration macro to add key property and functions list to ORG-ENTRY-ASSOCIATED-FILE-KEY-FNS"
        `(progn
           (defun ,name ,args
             ,@body)
           (org-entries-register-associated-to-file-key-function ,key ',name))))

    (put 'defassoc-file-key 'lisp-indent-function 3)
    (defun org-entries-associated-key-function (key)
      (plist-get org-entry-associated-file-key-fns key))
    (defun org-entries-associated-key-fn-value (key task-info file)
      (let ((keyfn (org-entries-associated-key-function key)))
        (if keyfn
            (let ((rank (funcall keyfn task-info file)))
              (unless (numberp rank)
                (error "org-entries-associated-key-fn-value: fun %s returning nonnumeric %s for file %s for task %s"
                       keyfn
                       rank
                       file
                       (org-entry-task-info-get-heading task-info)))
              (message "org-entries-associated-key-fn-value: task %s key %s MATCHED %d rank"
                       (org-entry-task-info-get-heading task-info)
                       key
                       rank)
              rank)
            (progn
              (message "org-entries-associated-key-fn-value: task %s key %s kyfn is %s so how can match %d rank"
                       (org-entry-task-info-get-heading task-info)
                       key
                       keyfn
                       0)
             0)))))

  (defassoc-file-key org-entry-associated-file-org-file-key :org-file (task-info file)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let ((org-file (org-entry-task-info-get-property task-info :task-clock-file)))
      (if (and file org-file
               (string-equal
                (file-truename file)
                (file-truename org-file)))
          10
          0)))

  (defassoc-file-key org-entry-associated-file-root-dir-key :root (task-info file)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((root
            (org-entry-task-info-get-property task-info :ROOT))
           (root (if root (file-truename root)))
           (file (if file (file-truename file))))
      (if root
          (progn
            (message "task %s root %s" (org-entry-task-info-get-heading task-info) root)
            (message "task %s file %s" (org-entry-task-info-get-heading task-info) file))
          (message "task %s root %s not present."
                   (org-entry-task-info-get-heading task-info) root))
      (if (and root file
               (string-match root file))
          (length root)
          0)))

  (defassoc-file-key org-entry-associated-file-status-key :status (task-info file)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((status
            (org-entry-task-info-get-property task-info 'status)))
      (if (string-equal status "CLOSED") -30 0)))

  (defassoc-file-key org-entry-associated-file-task-key :task-key (task-info file)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((key (org-entry-task-info-get-property task-info :KEY)))
      (if key (string-to-number key) 0)))

  (defassoc-file-key org-entry-associated-file-level-key :heading-level (task-info file)
    "Predicate funtion to check if file matches to task-info's file attribute."
    (let* ((level
            (org-entry-task-info-get-property task-info :task-clock-level)))
      (if level level 0)))

  (defassoc-file-key org-entry-associated-file-timebeing-key :timebeing (task-info file)
    (let ((timebeing (org-entry-task-info-get-property task-info :TIMEBEING)))
      (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
            (clocked-time   (org-entry-task-info-get-property task-info :task-clock-clock-sum)))
        (if (and
             (numberp clocked-time)
             (numberp timebeing-time)
             (> timebeing-time clocked-time))
            (- timebeing-time clocked-time)
            0))))

  ;; (defassoc-file-key org-entry-associated-file-current-clock-key :current-clock (task-info file)
  ;;   "Predicate funtion to check if file matches to task-info's file attribute."
  ;;   (let* ((task-marker
  ;;           (org-entry-task-info-get-property task-info :task-clock-marker)))
  ;;     (if (and
  ;;          org-clock-marker
  ;;          task-marker
  ;;          (equal
  ;;           (marker-buffer org-clock-marker)
  ;;           (marker-buffer task-marker))
  ;;          (equal
  ;;           (marker-position org-clock-marker)
  ;;           (marker-position task-marker)))
  ;;         100
  ;;         0)))
  )

(provide 'org-context-clocking-assoc-key)
;;; org-context-clocking-assoc-key.el ends here
