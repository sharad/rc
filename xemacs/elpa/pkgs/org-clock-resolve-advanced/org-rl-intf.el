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


(defvar org-rl-interfaces nil)

(defun org-rl-interface-get (intf key)
  (plist-get (cdr (assoc intf org-rl-interfaces)) key))

(defun org-rl-interface-put (intf key value)
  (plist-set (cdr (assoc intf org-rl-interfaces)) key value))

(defun org-rl-find-intf (clock-marker)
  (some #'(lambda (intf)
            (let ((clock-p (org-rl-interface-get (car intf) :org-rl-clock-p)))
              (when (and clock-p
                         (funcall clock-p clock-marker))
                (car intf))))
        org-rl-interfaces))


(defun org-rl-find-intf-clock-p (clock-marker)
  (org-rl-interface-get (org-rl-find-intf clock-marker) :org-rl-clock-p))

(defun org-rl-find-intf-clock-clock-in (clock-marker)
  (org-rl-interface-get (org-rl-find-intf clock-marker) :org-rl-clock-clock-in))

(defun org-rl-find-intf-clock-out (clock-marker)
  (org-rl-interface-get (org-rl-find-intf clock-marker) :org-rl-clock-out))

(defun org-rl-find-intf-select-other-clock (clock-marker)
  (org-rl-interface-get (org-rl-find-intf clock-marker) :org-rl-select-other-clock))

(defun org-rl-find-intf-capture+-helm-templates-alist (clock-marker)
  (org-rl-interface-get (org-rl-find-intf clock-marker) :org-rl-capture+-helm-templates-alist))


(defun org-rl-intf-register (tag plist)
  (org-rl-intf-unregister tag)
  (pushnew (cons tag plist)
           org-rl-interfaces))

(defun org-rl-intf-unregister (tag)
  (setq org-rl-interfaces
        (assoc-delete-all tag org-rl-interfaces)))


(defun org-rl-intf-clock-p (clock-marker)
  (let ((fun (org-rl-find-intf-clock-p clock-marker)))
    (if fun
        (funcall fun clock-marker)
      (error "Not found org-rl-clock-p"))))


(defun org-rl-intf-clock-clock-in (clock-marker &optional resume start-time)
  (let ((fun (org-rl-find-intf-clock-clock-in clock-marker)))
    (if fun
        (funcall fun clock-marker resume start-time)
      (error "Not found org-rl-clock-clock-in"))))

(defun org-rl-intf-clock-out (&optional switch-to-state fail-quietly at-time)
  (let ((fun (if org-clock-marker
                 (org-rl-find-intf-clock-out org-clock-marker))))
    (if fun
        (funcall fun switch-to-state fail-quietly at-time)
      (error "Not found org-rl-clock-out"))))

(defun org-rl-intf-clock-clock-out (clock-marker &optional fail-quietly at-time)
  (let ((fun (org-rl-find-intf-clock-clock-out clock-marker)))
    (if fun
        (funcall fun clock-marker fail-quietly at-time)
      (error "Not found org-rl-clock-clock-out"))))

;;;###autoload
(defun org-rl-intf-select-other-clock (clock-marker &optional target)
  (interactive)
  (let ((fun (org-rl-find-intf-select-other-clock clock-marker)))
    (if fun
        (funcall fun clock-marker target)
      (error "Not found org-rl-select-other-clock"))))

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

(defun org-rl-intf-capture+-helm-templates-alist (clock-marker)
  (let ((fun (org-rl-find-intf-capture+-helm-templates-alist clock-marker)))
    (if fun
        (funcall fun clock-marker)
      (error "Not found org-rl-capture+-helm-templates-alist"))))
;;; org-rl-intf.el ends here
