;;; pa-planner.el --- Planner interface for PA

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad at home>
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

(require 'planner-interface)
;; (require 'general-testing)

;; (defun pa/find-task-in-page-main (task page &optional buf-op)
;;   "return t if able to find task in page and leave in that page."
;;   (let ((task (normalize-task task))
;;         (buf (find-file
;;               (concat planner-directory "/" page ".muse"))))
;;     (when buf
;;       (goto-char 0)
;;       (if (re-search-forward "^*\s\+Tasks")
;;           (let ((start (point))
;;                 (end (if (re-search-forward "^*\s\+\\w\+")
;;                          (point))))
;;             (when (and end (not (equal end start)))
;;               (goto-char start)
;;               (re-search-forward task)
;;               ;; (read-from-minibuffer "sfddsf: " (format "%d %d eq %s" end start (not (equal end start))))
;;               (if (functionp buf-op) (funcall buf-op buf))
;;               t))))))

;; (defun pa/find-task-in-page (task page &optional restore buf-op)
;;   "return t if able to find task in page and leave in that page."
;;   (if restore
;;       (save-window-excursion
;;         (save-excursion
;;           (save-restriction
;;             (pa/find-task-in-page-main task page &optional buf-op))))
;;       (pa/find-task-in-page-main task page buf-op)))

(defun pa/planner-create-note-from-task (task &optional page)
  (if (planner-find-task-in-page task (or page (planner-today-ensure-exists)))
      (planner-create-note-from-task t)
      ;; ((inform in wm task not found)
      ;;  (return back the state of emacs.))
      ))

(defun pa/planner-goto-task (task &optional page)
  (planner-find-task-in-page task (or page (planner-today-ensure-exists))))

(defun ci-task (task &optional page)
  (planner-find-task-in-page task (or page (planner-today-ensure-exists))
                             t
                             #'(lambda (b)
                                 ;; (timeclock-in)
                                 (planner-task-in-progress)
                                 (save-buffer)
                                 (bury-buffer buf)
                                 ;; (kill-buffer buf)
                                 )))

(defun co-task (task &optional page)
  (planner-find-task-in-page task (or page (planner-today-ensure-exists))
                             t
                             #'(lambda (b)
                                 ;; (timeclock-in)
                                 (timeclock-out)
                                 (save-buffer)
                                 (bury-buffer buf)
                                 ;; (kill-buffer buf)
                                 )))

(defun pa/planner-task-done (task &optional page)
  (planner-find-task-in-page task (or page (planner-today-ensure-exists))
                             t
                             #'(lambda (b)
                                 ;; (timeclock-in)
                                 (planner-task-done)
                                 (save-buffer)
                                 (bury-buffer buf)
                                 ;; (kill-buffer buf)
                                 )))


;; (defun pa/planner-task-change-status (task status &optional page)
;;   (planner-find-task-in-page task (or page (planner-today-ensure-exists))
;;                              t
;;                              #'(lambda (b)
;;                                  ;; (timeclock-in)
;;                                  (if (functionp status) (funcall status))
;;                                  (save-buffer)
;;                                  (bury-buffer buf)
;;                                  ;; (kill-buffer buf)
;;                                  )))




(provide 'pa-planner)
;;; pa-planner.el ends here
