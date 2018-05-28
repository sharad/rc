;; Preamble


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:1]]
;;; org-context-clock.el --- org-context-clock               -*- lexical-binding: t; -*-

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
;; Preamble:1 ends here

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:2]]
(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)
;; Preamble:2 ends here

;; Required libraries


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Required%20libraries][Required libraries:1]]
(require 'org-clock)

 (require 'timer-utils-lotus)
 (eval-when-compile
   (require 'timer-utils-lotus))
 (require 'org-misc-utils-lotus)
 (eval-when-compile
   (require 'org-misc-utils-lotus))
 (require 'lotus-misc-utils)
 (eval-when-compile
   (require 'lotus-misc-utils))

(require 'org-onchange)
;; Required libraries:1 ends here

;; It is divided into multiple files for different functionality


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*It%20is%20divided%20into%20multiple%20files%20for%20different%20functionality][It is divided into multiple files for different functionality:1]]
(require 'org-context-clock-api)
(require 'org-context-clock-api-list) ;; "org tasks access api for list org"
(require 'org-context-clock-api-recursive) ;; "org tasks access api for recursive task"
(require 'org-context-clock-api-interaction) ;; "Interactive utitlity API's for adding root subtree etc" ;; "org tasks clocking's API"
(require 'org-context-clock-assoc-predicate) ;; "org tasks associated to context predicate functions"
(require 'org-context-clock-assoc-rank) ;; "Org tasks associated to context rank functions"
(require 'org-context-clock-assoc-key) ;; "org tasks associated to context key functions on recursive taskinfos"
;; It is divided into multiple files for different functionality:1 ends here

;; Global variables

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Global%20variables][Global variables:1]]
(defvar *org-context-clock-task-current-context*  nil)
(defvar *org-context-clock-task-previous-context* nil)
(defvar *org-context-clock-clocked-dyntaskpl-context-history*  nil)
(defvar *org-context-clock-task-current-context-time-interval* 7)
(defvar *org-context-clock-last-buffer-select-time* (current-time))
(defvar *org-context-clock-buffer-select-timer* nil)
(defvar *org-context-clock-update-current-context-msg* "")
;; (defvar org-context-clock-api-name :predicate "API")
(defvar org-context-clock-access-api-name :recursive "Aceess API")
(defvar org-context-clock-assoc-api-name :keys "Assoc API")
(defvar org-context-clock-api-dyntaskpl-print                  (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskplprint))

;; deprecated
(defvar org-context-clock-api-dyntaskpls-associated-to-context (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpls))
(defvar org-context-clock-api-tasks-associated-to-context      (org-context-clock-access-api-get org-context-clock-access-api-name :tasks))
(defvar org-context-clock-build-dyntaskpl                      (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpl))
(defvar org-context-clock-matching-dyntaskpls                  (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpls))
(defvar org-context-clock-matching-tasks                       (org-context-clock-access-api-get org-context-clock-access-api-name :tasks))
(defvar org-context-clock-api-task-associated-to-context-p     (org-context-clock-assoc-api-get  org-context-clock-assoc-api-name :taskp))
(defvar org-context-clock-api-task-update-tasks                (org-context-clock-access-api-get org-context-clock-access-api-name :update))
(defvar org-context-clock-api-task-update-files                (org-context-clock-access-api-get org-context-clock-access-api-name :files))
;; Global variables:1 ends here

;; Simple function


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Simple%20function][Simple function:1]]
(defun custom-plist-keys (in-plist)
  (if (null in-plist)
      in-plist
      (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))
;; Simple function:1 ends here

;; Disable for some time

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Disable%20for%20some%20time][Disable for some time:1]]
(defun org-context-clock-disable-for (time)
  "Disable context clocking for TIME period."
  ;; Implement
  )
;; Disable for some time:1 ends here

;; Context clock API


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Context%20clock%20API][Context clock API:1]]
;;;###autoload
(defun org-context-clock-api ()
  "org task clocking select api to use."
  (interactive)
  (let* ((assoc-api-keys (custom-plist-keys org-context-clock-task-clocking-assoc-api))
         (assoc-api-name (ido-completing-read
                          "org task clocking api name: "
                          (mapcar 'symbol-name assoc-api-keys)
                          nil
                          t
                          (symbol-name org-context-clock-assoc-api-name)))
         (assoc-api-key (intern assoc-api-name))

         (access-api-keys (custom-plist-keys org-context-clock-task-clocking-access-api))
         (access-api-name (ido-completing-read
                          "org task clocking api name: "
                          (mapcar 'symbol-name access-api-keys)
                          nil
                          t
                          (symbol-name org-context-clock-access-api-name)))
         (access-api-key (intern access-api-name)))
    (setq
     org-context-clock-assoc-api-name assoc-api-key
     org-context-clock-access-api-name access-api-key)
    (if (and
         (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskplprint)
         (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpl)
         (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpls)
         (org-context-clock-access-api-get org-context-clock-access-api-name :tasks)
         (org-context-clock-assoc-api-get org-context-clock-assoc-api-name :taskp)
         (org-context-clock-access-api-get org-context-clock-access-api-name :update))
        (setq
         org-context-clock-api-dyntaskpl-print                  (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskplprint)

         ;; deprecated
         org-context-clock-api-dyntaskpls-associated-to-context (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpls)
         org-context-clock-api-tasks-associated-to-context      (org-context-clock-access-api-get org-context-clock-access-api-name :tasks)
         org-context-clock-build-dyntaskpl                      (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpl)
         org-context-clock-matching-dyntaskpls                  (org-context-clock-access-api-get org-context-clock-access-api-name :dyntaskpls)
         org-context-clock-matching-tasks                       (org-context-clock-access-api-get org-context-clock-access-api-name :tasks)
         org-context-clock-api-task-associated-to-context-p     (org-context-clock-assoc-api-get org-context-clock-assoc-api-name :taskp)
         org-context-clock-api-task-update-tasks                (org-context-clock-access-api-get org-context-clock-access-api-name :update)))))
;; Context clock API:1 ends here
