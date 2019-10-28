;;; org-context-clock-assoc-api.el --- org-context-clock               -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'org-context-clock-assoc-common))

(require 'org-context-clock-assoc-common)

;;; REGISTRATION MACRO To add key property and functions list to ORG-CONTEXT-CLOCKING-TASK-ASSOCIATED-FILE-KEY-FNS

(define-keyop-functions org-task-associated-context-org-file :org-file (task context &rest args)
  (associator
   "Predicate funtion to check if context matches to task's file attribute."
   (let ((org-file (org-context-clock-task-get-property task :task-clock-file)))
     (let* ((file (plist-get context :file))
            (file (if file (file-truename file))))
       (if (and file org-file
                (string-equal
                 (file-truename file)
                 (file-truename org-file)))
           10
           0)))))

(define-keyop-functions org-task-associated-context-root-dir :root (task context &rest args)
  (associator
   "Predicate funtion to check if context matches to task's file attribute."
   (let* ((root
           (org-context-clock-task-get-property task :root))
          (root (if root (file-truename root))))
     (let* ((file (plist-get context :file))
            (file (if file (file-truename file))))
       (if root
           (progn
             (org-context-clock-debug :debug "task %s root %s" (org-context-clock-task-get-heading task) root)
             (org-context-clock-debug :debug "task %s file %s" (org-context-clock-task-get-heading task) file))
           (org-context-clock-debug :debug "task %s root %s not present."
                                    (org-context-clock-task-get-heading task) root))
       (if (and root file
                (string-match root file))
           (length root)
           0))))
  (getter
   "Get root dir"
   (let* ((file (if context (plist-get context :file)))
          (dir (if (stringp file) (file-name-directory file) (dirname-of-file (plist-get :file context))))
          (prompt (concat key ": ")))
     (ido-read-directory-name
      prompt
      dir dir))))

(define-keyop-functions org-task-associated-context-currfile-dir :currfile (task context &rest args)
  (associator
   "Predicate funtion to check if context matches to task's file attribute."
   (let* ((currfile
           (org-context-clock-task-get-property task :currfile))
          (currfile (if currfile (file-truename currfile))))
     (let* ((file (plist-get context :file))
            (file (if file (file-truename file))))
       (if currfile
           (progn
             (org-context-clock-debug :debug "task %s currfile %s" (org-context-clock-task-get-heading task) currfile)
             (org-context-clock-debug :debug "task %s file %s" (org-context-clock-task-get-heading task) file))
           (org-context-clock-debug :debug "task %s currfile %s not present."
                                    (org-context-clock-task-get-heading task) currfile))
       (if (and currfile file
                (string-match currfile file))
           (* 2 (length currfile))     ;as exact match to file giving double matching points.
           0)))))

(define-keyop-functions org-task-associated-context-status :status (task context &rest args)
  (associator
   ;; task closed criteria
   "Predicate funtion to check if context matches to task's status attribute."
   (let ((todo-type
          (org-context-clock-task-get-property task :todo-type))
         (closed
          (org-context-clock-task-get-property task :closed))
         (status
          (org-context-clock-task-get-property task :todo-keyword)))
     (if (or
          closed
          (eql todo-type 'done)
          (string-equal status "HOLD"))
         -30 0))))

(define-keyop-functions org-task-associated-context-sub-tree :sub-tree (task context &rest args)
                        (associator
                         ;; task closed criteria
                         "Predicate funtion to check if context matches to task's status attribute."
                         (let ((sub-tree
                                (org-context-clock-task-get-property task :sub-tree)))
                           (org-context-clock-debug :debug "task %s subtree %s" (org-context-clock-task-get-heading task) (null (null sub-tree)))
                           (if sub-tree -30 0))))

(define-keyop-functions org-task-associated-context-task :task-key (task context &rest args)
  (associator
   "Predicate funtion to check if context matches to task's file attribute."
   (let* ((key (org-context-clock-task-get-property task :KEY)))
     (if key (string-to-number key) 0))))

(define-keyop-functions org-task-associated-context-level :heading-level (task context &rest args)
  (associator
   "Predicate funtion to check if context matches to task's file attribute."
   (let* ((level
           (org-context-clock-task-get-property task :task-clock-level)))
     (if level level 0))))

(define-keyop-functions org-task-associated-context-timebeing :timebeing (task context &rest args)
  (associator
   (let ((timebeing (org-context-clock-task-get-property task :timebeing)))
     (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
           (clocked-time   (org-context-clock-task-get-property task :task-clock-clock-sum)))
       (if (and
            (numberp clocked-time)
            (numberp timebeing-time)
            (> timebeing-time clocked-time))
           (- timebeing-time clocked-time)
           0)))))

(define-keyop-functions org-task-associate-subtree :subtreefile (task context &rest args)
  (getter
   (let ((prompt (concat key ": ")))
     (file-relative-name
      (ido-read-file-name ;; org-iread-file-name
       prompt
       default-directory default-directory)))))

(define-keyop-functions org-task-associated-context-current-clock :current-clock (task context &rest args)
                        ;; "Predicate funtion to check if context matches to task's file attribute."
                        (associator
                         (let* ((task-marker
                                 (org-context-clock-task-get-property task :task-clock-marker)))
                           (if (and
                                (markerp org-clock-hd-marker)
                                (markerp task-marker)
                                (equal org-clock-hd-marker org-clock-hd-marker))
                               100
                               0))))




(provide 'org-context-clock-assoc-api)
;;; org-context-clock-assoc-api.el ends here
