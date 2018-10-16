;;; planner-multi-modified.el --- planner mutli modified

;; Copyright (C) 2014  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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

(defun planner-multi-task-string (info page-name links &optional new-pages)
  "Return task line for INFO on PAGE-NAME with LINKS, a list of pages to link to.
If non-nil, PAGES should be a list of the `planner-link-base's of LINKS."
  ;; Set up the new links list for easy testing
  ;; (setq new-pages (mapcar 'planner-link-base links))
  (setq new-pages (mapcar 'identity links))
  (cond
    ;; If this is a no-link task
    ((and (= (length new-pages) 1)
          (string= (car new-pages) page-name))
     (planner-format-task info nil nil nil nil "" ""))
    ;; If this is a standard singly-linked task (date, plan)
    ((and (= (length new-pages) 2)
          (string-match planner-date-regexp (car new-pages))
          (not (string-match planner-date-regexp (cadr new-pages))))
     (planner-format-task info nil nil nil nil
                          (planner-make-link
                           (if (string-match planner-date-regexp page-name)
                               (cadr new-pages)
                               (car new-pages)))))
    ;; If this is a standard singly-linked task (plan, date)
    ((and (= (length new-pages) 2)
          (not (string-match planner-date-regexp (car new-pages)))
          (string-match planner-date-regexp (cadr new-pages)))
     (planner-format-task info nil nil nil nil
                          (planner-make-link
                           (if (string-match planner-date-regexp page-name)
                               (car new-pages)
                               (cadr new-pages)))))
    ;; Multilink
    (t (planner-format-task info nil nil nil nil
                            (planner-make-link new-pages)))))

(provide 'planner-multi-modified)
;;; planner-multi-modified.el ends here
