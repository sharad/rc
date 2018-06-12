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

(defun org-context-clock-update-current-context (&optional force)
  (interactive "P")
  (if (>
       (float-time (time-since *org-context-clock-last-buffer-select-time*))
       *org-context-clock-task-current-context-time-interval*)
      (let* ((context (org-context-clock-build-context))
             (buff    (plist-get context :buffer)))
        (setq *org-context-clock-task-current-context*  context)
        (if (and
             (org-context-clock-changable-p)
             buff (buffer-live-p buff)
             (not (minibufferp buff))
             (not              ;BUG: Reconsider whether it is catching case after some delay.
              (equal *org-context-clock-task-previous-context* *org-context-clock-task-current-context*)))

            (progn
              (setq
               *org-context-clock-task-previous-context* *org-context-clock-task-current-context*)
              (if (and
                   (not (org-clock-marker-is-unnamed-clock-p))
                   (> (org-context-clock-current-task-associated-to-context-p context) 0))
                  (progn
                    (org-context-clock-debug :debug "org-context-clock-update-current-context: Current task already associate to %s" context))
                  (progn                ;current clock is not matching
                    (org-context-clock-debug :debug "org-context-clock-update-current-context: Now really going to clock.")
                    (unless (org-context-clock-dyntaskpl-run-associated-dyntaskpl context)
                      ;; not able to find associated, or intentionally not selecting a clock
                      (org-context-clock-debug :debug "trying to create unnamed task.")
                      (org-context-clock-maybe-create-clockedin-unnamed-dyntaskpl context))
                    (org-context-clock-debug :debug "org-context-clock-update-current-context: Now really clock done."))))

            (org-context-clock-debug :debug "org-context-clock-update-current-context: context %s not suitable to associate" context)))
    (org-context-clock-debug :debug "org-context-clock-update-current-context: not enough time passed.")))

(provide 'occ-main)
;;; occ-main.el ends here
