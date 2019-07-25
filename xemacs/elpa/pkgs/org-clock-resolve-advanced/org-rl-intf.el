;;; org-rl-intf.el --- org resolve clock interface   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

(provide 'org-rl-intf)


(defun org-rl-intf-register ())

(defun org-rl-intf-unregister ())


(defun org-rl-intf-clock-p (clock)
  t)


(defun org-rl-intf-clock-clock-in (clock &optional resume start-time)
  (org-rl-straight-org-clock-clock-in clock resume start-time))

(defun org-rl-intf-clock-out (&optional switch-to-state fail-quietly at-time)
  (org-clock-out switch-to-state fail-quietly at-time))

(defun org-rl-intf-clock-clock-out (clock &optional fail-quietly at-time)
  (org-clock-clock-out clock fail-quietly at-time))


;;;###autoload
(defun org-rl-intf-select-other-clock (&optional target)
  (interactive)
  (org-rl-debug nil "org-rl-select-other-clock: target[%s]" target)
  (org-with-refile
      file loc (or target org-refile-targets) "Refile other org heading"
    (let ((marker (make-marker)))
      (set-marker marker loc)
      marker)))


(defvar org-rl-capture+-helm-templates-alist org-capture+-helm-templates-alist)

;; (setq
;;  org-rl-capture+-helm-templates-alist
;;  '(("TODO" "* TODO %? %^g% %i [%a]" "* MILESTONE %? %^g %i [%a]")
;;    ("MEETING" "* MEETING %? %^g %i [%a]")))

;; (defun org-rl-build-capture+-option (interval prompt-fn options-fn default-fn)
;;   "To create new org entry"
;;   (let ((action #'(lambda ()
;;                     (let ((template (occ-capture+-helm-select-template)))
;;                       (when template
;;                         (let ((mrk (get-marker)))
;;                           (with-org-capture+ 'entry `(marker ,mrk) template '(:empty-lines 1)
;;                             (let ((capture-clock (make-org-rl-clock (point))))
;;                               t))))))))
;;     (helm-build-sync-source name
;;       :candidates (if (functionp options-fn)
;;                       (funcall options-fn)
;;                     options-fn)
;;       :action (list
;;                (cons "New Task" 'new-task))
;;       :action-transformer #'(lambda (actions candidate)
;;                               (list (cons "select"))))))

(defun org-rl-intf-capture+-helm-templates-alist ()
  org-rl-capture+-helm-templates-alist)
;;; org-rl-intf.el ends here
