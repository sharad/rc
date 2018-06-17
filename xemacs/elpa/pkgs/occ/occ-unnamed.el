
;;; occ-unnamed.el --- occ-api               -*- lexical-binding: t; -*-
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

(require 'occ-object-method)

(defvar *occ-unassociate-context-start-time* nil)
(defvar *occ-swapen-unnamed-threashold-interval* (* 60 2)) ;2 mins

(defun occ-unassociate-context-start-time-reset ()
  (setq *occ-unassociate-context-start-time* nil))

(defun occ-can-create-unnamed-task-p ()
  (unless *occ-unassociate-context-start-time*
    (setq *occ-unassociate-context-start-time* (current-time)))
  (let ((unassociate-context-start-time *occ-unassociate-context-start-time*))
    (prog1
        (>
         (float-time (time-since unassociate-context-start-time))
         *occ-swapen-unnamed-threashold-interval*))))

(defun org-clock-marker-is-unnamed-clock-p (&optional clock)
  (let ((clock (or clock org-clock-marker)))
    (when (and
           clock
           (lotus-org-unnamed-task-clock-marker))
     (equal
      (marker-buffer org-clock-marker)
      ;; id:x11 make org-context-clock version
      (marker-buffer (lotus-org-unnamed-task-clock-marker))))))

(defun occ-maybe-create-clockedin-unnamed-heading ()
  (when (occ-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (occ-debug :debug "occ-maybe-create-unnamed-task: Already clockin unnamed task")
          (prog1
              (lotus-org-create-unnamed-task-task-clock-in)
            (occ-unassociate-context-start-time-reset))))))

(defun occ-maybe-create-unnamed-heading ()
  (when (occ-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (occ-debug :debug "occ-maybe-create-unnamed-task: Already clockin unnamed task")
          (cdr (lotus-org-create-unnamed-task))))))


(defun occ-maybe-create-unnamed-task ()
  ;; back
  (let* ((unnamed-heading-marker
         (cdr (lotus-org-create-unnamed-task)))
        (unnamed-task
         (when unnamed-heading-marker
           (occ-make-task unnamed-heading-marker (occ-task-builder)))))
    unnamed-task))

(cl-defmethod occ-maybe-create-unnamed-contextual-task ((context occ-context))
  ;; back
  (let* ((unnamed-task
         (occ-maybe-create-unnamed-task))
        (unnamed-contextual-task
         (when unnamed-task
           (occ-make-contextual-task unnamed-task context 0))))
    unnamed-contextual-task))

(defun occ-maybe-create-clockedin-unnamed-contextual-task ((context occ-context))
  ;; back
  (when (occ-can-create-unnamed-task-p)
    (let ((org-log-note-clock-out nil))
      (if (org-clock-marker-is-unnamed-clock-p)
          (occ-debug :debug "occ-maybe-create-unnamed-task: Already clockin unnamed task")
        (let* ((unnamed-contextual-task (occ-maybe-create-unnamed-contextual-task context))
               (unnamed-task            (occ-contextual-task-task unnamed-contextual-task))
               (unnamed-marker          (occ-task-marker unnamed-task)))
            (prog1
                (occ-clockin-contextual-task unnamed-contextual-task)
              ;; id:x11 make org-context-clock version
              (lotus-org-unnamed-task-clock-marker unnamed-marker)
              (message "clockin to unnnamed task.")
              (occ-unassociate-context-start-time-reset)))))))

(defun occ-changable-p ()
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

(provide 'occ-unnamed)
;;; occ-unnamed.el ends here
