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

;; "Org entries associated to context-plist key functions on recursive taskinfos"
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

(defvar org-context-clocking-entry-associated-context-plist-key-fns nil
  "property list of KEY and API FUNCTIONS for key based association")

(progn ;; api
  (defun org-context-clocking-entries-associated-to-context-plist-by-keys (context-plist)
    "Retun org TASK-INFO entries for FILE which are associated based on list of functions for keys applied by org-context-clocking-entry-associated-TO-FILE-BY-KEYS-P"
    (let ((task-infos (org-context-clocking-entry-tree-update-task-infos))
          (matched '()))
      (org-context-clocking-debug "org-context-clocking-entries-associated-to-context-plist-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (org-context-clocking-tree-mapc-task-infos
       #'(lambda (task args)
           (let ((rank
                  (org-context-clocking-entry-associated-to-context-plist-by-keys-p task args)))
             (unless rank (error "org-context-clocking-entries-associated-to-context-plist-by-keys[lambda]: rank is null"))
             (when (> rank 0)
               (push task matched)
               (org-context-clocking-debug "org-context-clocking-entries-associated-to-context-plist-by-keys[lambda]: task %s MATCHED RANK %d"
                        (org-context-clocking-entry-task-info-get-heading task)
                        (length matched)))))
       task-infos
       context-plist)

      (org-context-clocking-debug "org-context-clocking-entries-associated-to-context-plist-by-keys: AFTER matched %s[%d]" "matched" (length matched))

      matched))


  (defun org-context-clocking-entry-associated-to-context-plist-by-keys-p (task-info context-plist)
    "Test whether association of org TASK-INFO for FILE using list of functions for keys,
using algorithm in this function, return RANK"
    (if context-plist
        (if (> (org-context-clocking-entries-associated-key-fn-value :status task-info context-plist) -20)
            (let ((rank
                   (+
                    (org-context-clocking-entries-associated-key-fn-value :timebeing task-info context-plist)
                    (org-context-clocking-entries-associated-key-fn-value :root task-info context-plist)
                    ;; (org-context-clocking-entries-associated-key-fn-value :org-file task-info context-plist)
                    (org-context-clocking-entries-associated-key-fn-value :task-info-key task-info context-plist)
                    (org-context-clocking-entries-associated-key-fn-value :heading-level task-info context-plist))))
              rank)
            -20)
        0))

  (org-context-clocking-api-set :keys :entries 'org-context-clocking-entries-associated-to-context-plist-by-keys) ;will be in api
  (org-context-clocking-api-set :keys :entryp  'org-context-clocking-entry-associated-to-context-plist-by-keys-p)
  (org-context-clocking-api-set :keys :update  'org-context-clocking-entry-tree-update-task-infos) ;will be in api
  )



(progn ;; REGISTRATION MACRO To add key property and functions list to ORG-CONTEXT-CLOCKING-ENTRY-ASSOCIATED-FILE-KEY-FNS
  (setq org-context-clocking-entry-associated-context-plist-key-fns nil)
  (message "NOTE: org-context-clocking-entry-associated-context-plist-key-fns made to nil")

  (progn ;; macros and accessor

    (defun org-context-clocking-entries-register-associated-to-context-plist-key-function (key fn)
      (setq
       org-context-clocking-entry-associated-context-plist-key-fns
       (plist-put
        org-context-clocking-entry-associated-context-plist-key-fns key fn)))

    (eval-when-compile                  ;; TODO: auto generate name from KEY
      (defmacro defassoc-context-plist-key (name key args &rest body)
        "Registration macro to add key property and functions list to ORG-ENTRY-ASSOCIATED-FILE-KEY-FNS"
        `(progn
           (defun ,name ,args
             ,@body)
           (org-context-clocking-entries-register-associated-to-context-plist-key-function ,key ',name))))

    (put 'defassoc-context-plist-key 'lisp-indent-function 3)
    (defun org-context-clocking-entries-associated-key-function (key)
      (plist-get org-context-clocking-entry-associated-context-plist-key-fns key))
    (defun org-context-clocking-entries-associated-key-fn-value (key task-info context-plist)
      (let ((keyfn (org-context-clocking-entries-associated-key-function key)))
        (if keyfn
            (let ((rank (funcall keyfn task-info context-plist)))
              (unless (numberp rank)
                (error "org-context-clocking-entries-associated-key-fn-value: fun %s returning nonnumeric %s for context-plist %s for task %s"
                       keyfn
                       rank
                       context-plist
                       (org-context-clocking-entry-task-info-get-heading task-info)))
              (org-context-clocking-debug "org-context-clocking-entries-associated-key-fn-value: task %s key %s MATCHED %d rank"
                       (org-context-clocking-entry-task-info-get-heading task-info)
                       key
                       rank)
              rank)
            (progn
              (org-context-clocking-debug "org-context-clocking-entries-associated-key-fn-value: task %s key %s kyfn is %s so how can match %d rank"
                       (org-context-clocking-entry-task-info-get-heading task-info)
                       key
                       keyfn
                       0)
              0)))))

  (defassoc-context-plist-key org-entry-associated-context-plist-org-file-key :org-file (task-info context-plist)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let ((org-file (org-context-clocking-entry-task-info-get-property task-info :task-clock-file)))
      (let* ((file (plist-get context-plist :file))
             (file (if file (file-truename file))))
        (if (and file org-file
                 (string-equal
                  (file-truename file)
                  (file-truename org-file)))
            10
            0))))

  (defassoc-context-plist-key org-entry-associated-context-plist-root-dir-key :root (task-info context-plist)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((root
            (org-context-clocking-entry-task-info-get-property task-info :ROOT))
           (root (if root (file-truename root))))
      (let* ((file (plist-get context-plist :file))
             (file (if file (file-truename file))))
       (if root
           (progn
             (org-context-clocking-debug "task %s root %s" (org-context-clocking-entry-task-info-get-heading task-info) root)
             (org-context-clocking-debug "task %s file %s" (org-context-clocking-entry-task-info-get-heading task-info) file))
           (org-context-clocking-debug "task %s root %s not present."
                                    (org-context-clocking-entry-task-info-get-heading task-info) root))
       (if (and root file
                (string-match root file))
           (length root)
           0))))

  (defassoc-context-plist-key org-entry-associated-context-plist-status-key :status (task-info context-plist)
    "Predicate funtion to check if context-plist matches to task-info's status attribute."
    (let* ((status
            (org-context-clocking-entry-task-info-get-property task-info 'status)))
      (if (string-equal status "CLOSED") -30 0)))

  (defassoc-context-plist-key org-entry-associated-context-plist-task-key :task-key (task-info context-plist)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((key (org-context-clocking-entry-task-info-get-property task-info :KEY)))
      (if key (string-to-number key) 0)))

  (defassoc-context-plist-key org-entry-associated-context-plist-level-key :heading-level (task-info context-plist)
    "Predicate funtion to check if context-plist matches to task-info's file attribute."
    (let* ((level
            (org-context-clocking-entry-task-info-get-property task-info :task-clock-level)))
      (if level level 0)))

  (defassoc-context-plist-key org-entry-associated-context-plist-timebeing-key :timebeing (task-info context-plist)
    (let ((timebeing (org-context-clocking-entry-task-info-get-property task-info :TIMEBEING)))
      (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
            (clocked-time   (org-context-clocking-entry-task-info-get-property task-info :task-clock-clock-sum)))
        (if (and
             (numberp clocked-time)
             (numberp timebeing-time)
             (> timebeing-time clocked-time))
            (- timebeing-time clocked-time)
            0))))

  ;; (defassoc-context-plist-key org-entry-associated-context-plist-current-clock-key :current-clock (task-info context-plist)
  ;;   "Predicate funtion to check if context-plist matches to task-info's file attribute."
  ;;   (let* ((task-marker
  ;;           (org-context-clocking-entry-task-info-get-property task-info :task-clock-marker)))
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
