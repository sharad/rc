;;; date.el --- GNUS Date formats etc.

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: convenience, news

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

;; File for

;;; Code:

;; from http://emacs.wordpress.com/category/gnus/
;; (setq gnus-user-date-format-alist
;;       '(((gnus-seconds-today) . "Today, %H:%M")
;;         ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
;;         (604800 . "%A %H:%M") ;;that's one week
;;         ((gnus-seconds-month) . "%A %d")
;;         ((gnus-seconds-year) . "%B %d")
;;         (t . "%B %d '%y"))) ;;this one is used when no other does match

;; (setq gnus-summary-line-format
;;       (concat "%U%R %~(pad-right 2)t%* %uj %B%~(max-right 30)~(pad-right 30)n  "
;;               "%~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"))


(provide 'date-config)

;;; date.el ends here
