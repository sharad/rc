;;; org-context-clock-api.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)


(require 'org-clock)

(progn ;; "org-context-clock-task-clocking-assoc-api interface"

  (defvar org-context-clock-task-clocking-assoc-api nil)

  (defun org-context-clock-assoc-api-set (name api fn)
    (let ((pl (plist-get org-context-clock-task-clocking-assoc-api name)))
      (setq org-context-clock-task-clocking-assoc-api
            (plist-put
             org-context-clock-task-clocking-assoc-api
             name
             (plist-put pl api fn)))))
  (defun org-context-clock-assoc-api-get (name api)
    (plist-get
     (plist-get org-context-clock-task-clocking-assoc-api name)
     api)))

(progn ;; "org-context-clock-task-clocking-access-api interface"

  (defvar org-context-clock-task-clocking-access-api nil)

  (defun org-context-clock-access-api-set (name api fn)
    (let ((pl (plist-get org-context-clock-task-clocking-access-api name)))
      (setq org-context-clock-task-clocking-access-api
            (plist-put
             org-context-clock-task-clocking-access-api
             name
             (plist-put pl api fn)))))
  (defun org-context-clock-access-api-get (name api)
    (plist-get
     (plist-get org-context-clock-task-clocking-access-api name)
     api)))


;; "org tasks clocking's APIs' API"
(require 'org-context-clock-api-common) ;; "org tasks accss common api"

(defun org-context-clock-debug (level message &rest args)
  (apply 'lwarn 'org-context-clock level message args)
  ;; (apply #'message message args)
  )

;;;###autoload
(defun org-context-clock-start-debug ()
  (interactive)
  (setq
   warning-minimum-log-level :debug
   warning-minimum-level :debug))

;;;###autoload
(defun org-context-clock-stop-debug ()
  (interactive)
  (setq
   warning-minimum-log-level :warning
   warning-minimum-level :warning))



;; (org-context-clock-debug :debug "hello %s[%d]" "test" 11)


;; (require 'org-context-clock-api-list) ;; "org tasks access api for list org"

;; (require 'org-context-clock-api-recursive) ;; "org tasks access api for recursive task"

;; (require 'org-context-clock-api-interaction) ;; "Interactive utitlity API's for adding root subtree etc"


;; ;; "org tasks clocking's API"
;; (require 'org-context-clock-assoc-predicate) ;; "Org tasks associated to context predicate functions"

;; (require 'org-context-clock-assoc-rank) ;; "Org tasks associated to context rank functions"

;; (require 'org-context-clock-assoc-key) ;; "Org tasks associated to context key functions on recursive taskinfos"

;; API end here

(provide 'org-context-clock-api)
;;; org-context-clock-api.el ends here
