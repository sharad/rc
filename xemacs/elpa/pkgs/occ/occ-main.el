;;; occ-main.el --- occ-api               -*- lexical-binding: t; -*-
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

(require 'occ-object-methods)
(require 'occ-unnamed)
(require 'occ-interactive)

(defcustom *occ-last-buffer-select-time*            nil "*occ-last-buffer-select-time*")
(defvar    *occ-task-current-context-time-interval* nil)
(defvar    *occ-task-previous-context*              nil)
(defvar    *occ-task-current-context*               nil)

(defun occ-set-global-task-collection-spec (spec)
  (setq
   occ-global-task-collection nil
   occ-global-task-collection-spec spec))

(occ-set-global-task-collection-spec
 (list :tree org-context-clock-task-tree-task-root-org-file))

(defun occ-update-current-context (&optional force)
  (interactive "P")
  (if (>
       (float-time (time-since *occ-last-buffer-select-time*))
       *occ-task-current-context-time-interval*)
      (let* ((context (occ-make-context))
             (buff    (occ-context-buffer context)))
        (setq *occ-task-current-context*  context)
        (if (and
             (occ-changable-p)
             buff (buffer-live-p buff)
             (not (minibufferp buff))
             (not              ;BUG: Reconsider whether it is catching case after some delay.
              (equal *occ-task-previous-context* *occ-task-current-context*)))

            (progn
              (setq
               *occ-task-previous-context* *occ-task-current-context*)
              (if (and
                   (not (occ-clock-marker-is-unnamed-clock-p))
                   (> (occ-current-task-associated-to-context-p context) 0))
                  (progn
                    (occ-debug :debug "occ-update-current-context: Current task already associate to %s" context))
                  (progn                ;current clock is not matching
                    (occ-debug :debug "occ-update-current-context: Now really going to clock.")
                    (unless (occ-run-associated-task context)
                      ;; not able to find associated, or intentionally not selecting a clock
                      (occ-debug :debug "trying to create unnamed task.")
                      (occ-maybe-create-clockedin-unnamed-contextual-task context))
                    (occ-debug :debug "occ-update-current-context: Now really clock done."))))

            (occ-debug :debug "occ-update-current-context: context %s not suitable to associate" context)))
    (occ-debug :debug "occ-update-current-context: not enough time passed.")))

(provide 'occ-main)
;;; occ-main.el ends here
