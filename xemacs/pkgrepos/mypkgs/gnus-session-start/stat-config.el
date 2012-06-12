;;; stat.el --- Statetics function

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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

(deh-section "statistics"
;;{{
;; from: http://www.ichimusai.org/pub/dot-gnus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATISTICS FUNCTION
;;
;; Select from summary buffer and run M-x stat RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun stat (beg end)
  (interactive "r")
  (let (header from-list subject-list from subject (n 0) (chars 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(setq header (gnus-summary-article-header))
	(incf n)
	(incf chars (mail-header-chars header))
	(setq from (gnus-extract-address-components (mail-header-from header)))
	(setq from (or (car from) (cadr from)))
	(if (assoc from from-list)
	    (incf (cdr (assoc from from-list)))
	  (push (cons from 1) from-list))
	(setq subject (gnus-simplify-subject (mail-header-subject header)))
	(if (assoc subject subject-list)
	    (incf (cdr (assoc subject subject-list)))
	  (push (cons subject 1) subject-list))
	(forward-line)))
    (setq from-list (sort from-list (lambda (a b) (> (cdr a) (cdr b)))))
    (setq subject-list (sort subject-list (lambda (a b) (> (cdr a) (cdr b)))))
    (switch-to-buffer-other-window (get-buffer-create "*stat*"))
    (insert (format "Total number of posts: %i\n" n))
    (insert (format "Average bytes/post: %f\n" (/ (float chars) n)))
    (insert (format "Total number of posters: %i\n" (length from-list)))
    (insert (format "Average posts/poster: %f\n\n" (stat-mean from-list)))
    (stat-top from-list 20)
    (insert (format "\nTotal number of subjects: %i\n" (length subject-list)))
    (insert (format "Average posts/subject: %f\n\n" (stat-mean subject-list)))
    (stat-top subject-list 20)))

(defun stat-mean (alist)
  (let ((mean 0))
    (dolist (x alist)
      (incf mean (cdr x)))
    (/ (float mean) (length alist))))

(defun stat-top (alist &optional n)
  (dotimes (i (if (integerp n)
		  (min n (length alist))
		(length alist)))
    (insert (format "%4i %s\n"
		    (cdr (nth i alist))
		    (car (nth i alist))))))


;;}}

 )
(provide 'stat-config)
;;; stat.el ends here
