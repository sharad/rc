;;; org-context-clocking-api.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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

(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)


(require 'org-clock)

;; (org-context-clocking-api-entries-associated-to-context-plist-p context-plist)
;; (org-context-clocking-api-entry-associated-to-context-plist-p task-info context-plist)
;; (org-context-clocking-api-entry-update-task-infos &optional force)

(progn ;; "org-context-clocking-entry-clocking-api interface"

  (defvar org-context-clocking-entry-clocking-api nil)

  (defun org-context-clocking-api-set (name api fn)
    (let ((pl (plist-get org-context-clocking-entry-clocking-api name)))
      ;; (message "org-context-clocking-entry-clocking-api: %s, pl: %s " org-context-clocking-entry-clocking-api pl)
      (setq org-context-clocking-entry-clocking-api
            (plist-put
             org-context-clocking-entry-clocking-api
             name
             (plist-put pl api fn)))))
  (defun org-context-clocking-api-get (name api)
    (plist-get
     (plist-get org-context-clocking-entry-clocking-api name)
     api)))


;; "org entries clocking's APIs' API"
(require 'org-context-clocking-api-common) ;; "org entries accss common api"

(defun org-context-clocking-debug (message &rest args)
  (apply 'lwarn 'org-context-clock :debug message args))

;; (org-context-clock-debug "hello %s[%d]" "test" 11)


(require 'org-context-clocking-api-list) ;; "org entries access api for list org"

(require 'org-context-clocking-api-recursive) ;; "org entries access api for recursive task"

(require 'org-context-clocking-api-interaction) ;; "Interactive utitlity API's for adding root subtree etc"


;; "org entries clocking's API"
(require 'org-context-clocking-assoc-predicate) ;; "Org entries associated to context-plist predicate functions"

(require 'org-context-clocking-assoc-rank) ;; "Org entries associated to context-plist rank functions"

(require 'org-context-clocking-assoc-key) ;; "Org entries associated to context-plist key functions on recursive taskinfos"

;; API end here

(provide 'org-context-clocking-api)
;;; org-context-clocking-api.el ends here
