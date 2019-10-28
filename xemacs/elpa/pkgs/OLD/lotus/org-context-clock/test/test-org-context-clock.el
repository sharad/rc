;;; org-context-clock.el --- org-context-clock               -*- lexical-binding: t; -*-

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


(require 'ert)


(require 'org-context-clock-api-common)
(require 'org-context-clock)


(ert-deftest test-task-run-associated-clock ()
  (should
   (org-context-clock-task-run-associated-clock
    (org-context-clock-build-context)))
  (should
   (org-context-clock-task-run-associated-clock
    (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))))

;; (ert-deftest )




(org-context-clock-markers-associated-to-context
 (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

(org-context-clock-task-associated-to-context-p
 (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"))
 (org-context-clock-build-context))

(org-context-clock-markers-associated-to-context (org-context-clock-build-context))

;; (org-context-clock-task-associated-to-context-p (org-context-clock-build-context))

;; sharad
(when (and org-clock-marker
           (marker-buffer org-clock-marker))
  (setq test-info-task
        (let ((xcontext
               (list
                :file (buffer-file-name)
                :buffer (current-buffer))))
          (org-with-clock-position (list org-clock-marker)
            (org-previous-visible-heading 1)
            (let ((info (org-context-clock-collect-task)))
              (if (funcall org-context-clock-api-task-associated-to-context-p info xcontext)
                  info)))))


  (funcall org-context-clock-api-task-associated-to-context-p
           test-info-task
           (org-context-clock-build-context))

  ;; org-clock-marker
  (org-tasks-associated-key-fn-value
   :current-clock test-info-task
   (org-context-clock-build-context)))

(funcall org-context-clock-api-task-associated-to-context-p
         (org-context-clock-task-current-task)
         (org-context-clock-build-context))




;; (test-info-task)

;; (org-context-clock-task-associated-to-context-p
;;  (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

;; (org-context-clock-task-associated-to-context-p
;;  (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org")))

;; (org-task-associated-context-org-context-p
;;  "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
;;  (cadr org-task-list-tasks)))


(length
 (funcall org-context-clock-api-tasks-associated-to-context
          (org-context-clock-build-context)))

(length
 (funcall org-context-clock-api-tasks-associated-to-context
          (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile"))))

(org-context-clock-markers-associated-to-context (org-context-clock-build-context))

(length
 (funcall org-context-clock-api-tasks-associated-to-context
          (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"))))

;; (length
;;  (org-context-clock-tasks-associated-to-context-by-keys
;;   (org-context-clock-build-context)))


;; (length
;;  (org-context-clock-tasks-associated-to-context-by-keys
;;   (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile"))))

;; (org-context-clock-task-associated-to-context-p
;;  (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile")))

;; (org-context-clock-task-associated-to-context-by-keys "/home/s/paradise/releases/global/patch-upgrade/Makefile")

;; (if (org-context-clock-task-associated-to-context-p
;;      (org-context-clock-build-context))
;;     (message "current clock is with current context or file"))

(provide 'test-org-context-clock)
;;; org-context-clock.el ends here
