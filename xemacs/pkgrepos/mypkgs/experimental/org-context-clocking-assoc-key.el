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
(progn
  '(
    '(matches
      '(file based)x
      '(dir based -merit) x
      '(status based) x
      '(user input based)
      '(config based) x
      '(time based recently opened)
      '(heading level based)))

(defvar org-entry-associated-file-key-fns nil)

(progn ;; api
  (defun org-entries-associated-to-file-by-keys-p (file)
    (let ((task-infos (org-entry-tree-update-task-infos))
          (matched '()))
      (tree-mapc-task-infos
       '(lambda (task args)
         (let ((result
                (org-entry-associated-to-file-by-keys-p task args)))
           (when result
             (push task matched))))
       task-infos
       file)))


  (defun org-entry-associated-to-file-by-keys-p (task-info file)
    (if file
        (if (> (org-entries-associated-key-fn-value :status task-info file) -20)
            (>
             (+
              (org-entries-associated-key-fn-value :timebeing task-info file)
              (org-entries-associated-key-fn-value :root task-info file)
              ;; (org-entries-associated-key-fn-value :org-file task-info file)
              (org-entries-associated-key-fn-value :task-info-key task-info file)
              (org-entries-associated-key-fn-value :heading-level task-info file))
             0)))))

(org-context-clocking-api-set :keys :entries 'org-entries-associated-to-file-by-keys-p)
(org-context-clocking-api-set :keys :entry   'org-entry-associated-to-file-by-keys-p)
(org-context-clocking-api-set :keys :update  'org-entry-tree-update-task-infos))

(progn ;; functions
  (progn
    (setq org-entry-associated-file-key-fns nil)

    (defun org-entries-register-associated-to-file-key-function (key fn)
      (setq
       org-entry-associated-file-key-fns
       (plist-put
        org-entry-associated-file-key-fns key fn)))

    (eval-when-compile
      (defmacro defassoc-file-key (name key args &rest body)
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
            (funcall keyfn task-info file)
            0))))

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
