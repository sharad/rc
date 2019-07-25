;;; org-rl-utils.el --- org rl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'org-rl-utils)


(require 'org-rl-intf)


(defvar org-rl-debug nil "Debug org advanced resolve clock")

(defun org-rl-debug (level &rest args)
  (let* ((ilevel (or level :debug))
         (ts (time-stamp-string))
         (fmt (format "%s: %s" ts (car args)))
         (args (append (list fmt) (cdr args))))
    (when org-rl-debug
      (apply #'lwarn 'org-rl-clock ilevel args)
      (when level
        (message
         "%s"
         (concat
          (format "org-rl-clock %s: " ilevel)
          (apply #'format args)))))))


(defvar org-rl-org-clock-persist nil "Control org-clock-persist at time of org-resolve clock-in")
(defvar org-rl-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of org-resolev clock-in")

;; lotus-with-file-pos-new-win: selecting buf report.org<hostapdng> [2 times]
;; org--align-node-property: Match data clobbered by buffer modification hooks
;; TODO: FIX: org--align-node-property: Match data clobbered by buffer modification hooks
;; BUG: TODO: need to use (occ-clock-in occ-ctxtual-tsk)
(defun org-rl-straight-org-clock-clock-in (clock &optional resume start-time)
  (progn
    (org-rl-debug nil "org-rl-straight-org-clock-clock-in: begin")
    (lotus-org-clock-load-only)
    (let ((org-clock-persist               org-rl-org-clock-persist)
          (org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))

      (org-with-narrow-to-marker (org-rl-clock-marker clock)
        (lotus-org-with-safe-modification
          (org-entry-put nil "Effort" "10")))

      (org-rl-intf-clock-clock-in
       (org-rl-clock-for-clock-in clock)
       resume start-time)

      (setf (org-rl-clock-marker clock) org-clock-marker)
      (setf (org-rl-clock-current clock) t)
      clock)))


(defun org-rl-org-clock-clock-in (clock &optional resume start-time)
  (org-rl-intf-clock-clock-in clock resume start-time))

(defun org-rl-org-clock-out (&optional switch-to-state fail-quietly at-time)
  (org-rl-intf-clock-out switch-to-state fail-quietly at-time))

(defun org-rl-org-clock-clock-out (clock &optional fail-quietly at-time)
  (org-rl-intf-clock-clock-out clock fail-quietly at-time))

(defun org-rl-org-capture+-helm-templates-alist (clock)
  (org-rl-intf-capture+-helm-templates-alist clock))

;;;###autoload
(defun org-rl-org-select-other-clock (clock &optional target)
  (interactive)
  (org-rl-intf-select-other-clock clock target))

;;; org-rl-utils.el ends here
