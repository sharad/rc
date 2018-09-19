;; Preamble


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:1]]
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

;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:2]]
(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)
;; Preamble:2 ends here

;; Required libraries


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Required%20libraries][Required libraries:1]]
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


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*It%20is%20divided%20into%20multiple%20files%20for%20different%20functionality][It is divided into multiple files for different functionality:1]]
(require 'org-context-clock-api)
(require 'org-context-clock-api-list) ;; "org tasks access api for list org"
(require 'org-context-clock-api-recursive) ;; "org tasks access api for recursive task"
(require 'org-context-clock-api-interaction) ;; "Interactive utitlity API's for adding root subtree etc" ;; "org tasks clocking's API"
(require 'org-context-clock-assoc-predicate) ;; "org tasks associated to context predicate functions"
(require 'org-context-clock-assoc-rank) ;; "Org tasks associated to context rank functions"
(require 'org-context-clock-assoc-key) ;; "org tasks associated to context key functions on recursive taskinfos"
;; It is divided into multiple files for different functionality:1 ends here

;; Global variables

;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Global%20variables][Global variables:1]]
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


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Simple%20function][Simple function:1]]
(defun custom-plist-keys (in-plist)
  (if (null in-plist)
      in-plist
      (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))
;; Simple function:1 ends here

;; Disable for some time

;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Disable%20for%20some%20time][Disable for some time:1]]
(defun org-context-clock-disable-for (time)
  "Disable context clocking for TIME period."
  ;; Implement
  )
;; Disable for some time:1 ends here

;; Context clock API


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Context%20clock%20API][Context clock API:1]]
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

;; Update tasks


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Update%20tasks][Update tasks:1]]
;;;###autoload
(defun org-context-clock-task-update-tasks (&optional force)
  "Update task infos"
  (interactive "P")
  (message "calling org-context-clock-task-update-tasks")
  (funcall org-context-clock-api-task-update-tasks force))

;;;###autoload
(defun org-context-clock-task-update-files (&optional force)
  "Update task infos"
  (interactive "P")
  (funcall org-context-clock-api-task-update-files force))

(defun org-context-clock-build-tasks (file)
  (when (member*
              file
              (org-context-clock-task-update-files)
              :test #'(lambda (f1 f2)
                        (string-equal
                         (file-truename f1)
                         (file-truename f2))))
    (org-context-clock-task-update-tasks t)))

(defun org-context-clock-after-save-hook ()
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (org-context-clock-build-tasks (buffer-file-name))))
;; Update tasks:1 ends here

;; Build context


;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Build%20context][Build context:1]]
(defun org-context-clock-build-context (&optional buff)
  (let* ((buff (if buff
                   (if (bufferp buff)
                       buff
                       (if (stringp buff)
                           (or
                            (get-buffer buff)
                            (if (file-exists-p buff)
                                (get-file-buffer buff)))))
                   (window-buffer)))
         (buf (org-base-buffer buf))
         (file (buffer-file-name buff))
         (context (list :file file :buffer buff)))
    context))
;; Build context:1 ends here

;; Unnamed task related global variable

;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Unnamed%20task%20related%20global%20variable][Unnamed task related global variable:1]]
(defvar *org-context-clock-unassociate-context-start-time* nil)
(defvar *org-context-clock-swapen-unnamed-threashold-interval* (* 60 2)) ;2 mins
;; Unnamed task related global variable:1 ends here

;; Unnamed task functions

;; [[file:~/.xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Unnamed%20task%20functions][Unnamed task functions:1]]
(defun org-context-clock-unassociate-context-start-time-reset ()
  (setq *org-context-clock-unassociate-context-start-time* nil))

(defun org-context-clock-can-create-unnamed-task-p ()
  (unless *org-context-clock-unassociate-context-start-time*
    (setq *org-context-clock-unassociate-context-start-time* (current-time)))
  (let ((unassociate-context-start-time *org-context-clock-unassociate-context-start-time*))
    (prog1
        (>
         (float-time (time-since unassociate-context-start-time))
         *org-context-clock-swapen-unnamed-threashold-interval*))))

(defun org-clock-marker-is-unnamed-clock-p (&optional clock)
  (let ((clock (or clock org-clock-marker)))
    (when (and
           clock
           (lotus-org-unnamed-task-clock-marker))
     (equal
      (marker-buffer org-clock-marker)
      ;; id:x11 make org-context-clock version
      (marker-buffer (lotus-org-unnamed-task-clock-marker))))))

(defun org-context-clock-maybe-create-clockedin-unnamed-heading ()
  (when (org-context-clock-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (org-context-clock-debug :debug "org-context-clock-maybe-create-unnamed-task: Already clockin unnamed task")
          (prog1
              (lotus-org-create-unnamed-task-task-clock-in)
            (org-context-clock-unassociate-context-start-time-reset))))))

(defun org-context-clock-maybe-create-unnamed-heading ()
  (when (org-context-clock-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (org-context-clock-debug :debug "org-context-clock-maybe-create-unnamed-task: Already clockin unnamed task")
          (cdr (lotus-org-create-unnamed-task))))))


(defun org-context-clock-maybe-create-unnamed-task ()
  ;; back
  (let* ((unnamed-heading-marker
         (cdr (lotus-org-create-unnamed-task)))
        (unnamed-task
         (when unnamed-heading-marker
           (with-current-buffer (marker-buffer unnamed-heading-marker)
             (goto-char unnamed-heading-marker)
             (org-context-clock-collect-task)))))
    unnamed-task))

(defun org-context-clock-maybe-create-unnamed-dyntaskpl (context)
  ;; back
  (let* ((unnamed-task
         (org-context-clock-maybe-create-unnamed-task))
        (unnamed-dyntaskpl
         (if unnamed-task
           (org-context-clock-build-dyntaskpl unnamed-task context))))
    unnamed-dyntaskpl))

(defun org-context-clock-maybe-create-clockedin-unnamed-dyntaskpl (context)
  ;; back
  (when (org-context-clock-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (org-context-clock-debug :debug "org-context-clock-maybe-create-unnamed-task: Already clockin unnamed task")
          (let* ((unnamed-dyntaskpl (org-context-clock-maybe-create-unnamed-dyntaskpl context))
                 (unnamed-task (plist-get unnamed-dyntaskpl :task))
                 (unnamed-marker (plist-get unnamed-task :task-clock-marker)))
            (prog1
                (org-context-clock-clockin-dyntaskpl unnamed-dyntaskpl)
              ;; id:x11 make org-context-clock version
              (lotus-org-unnamed-task-clock-marker unnamed-marker)
              (message "clockin to unnnamed task.")
              (org-context-clock-unassociate-context-start-time-reset)))))))

(defun org-context-clock-changable-p ()
  "Stay with a clock at least 2 mins."
  (if org-clock-start-time
      (let ((clock-duration
             (if (and
                  (stringp org-clock-start-time)
                  (string-equal "" org-clock-start-time))
                 0
                 (float-time (time-since org-clock-start-time)))))
        (or
         (< clock-duration 60)
         (> clock-duration 120)))
      t))
;; Unnamed task functions:1 ends here
