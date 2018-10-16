;;; planner-modified.el --- Planner Modifications

;; Copyright (C) 2014  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:convenience

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



(defun planner-make-link (link &optional name single)
  "Return a Wiki link to LINK with NAME as the text.
If SINGLE is non-nil, treat it as a single link.
If LINK is already a valid link, replace it's description
by NAME"
  (cond ((or (null link) (string= link ""))
         "")
        ((string-match muse-explicit-link-regexp link)
         (muse-make-link (match-string 1 link) (or name (match-string 2 link))))
        (t
         (muse-make-link link name))))





(provide 'planner-modified)
;;; planner-modified.el ends here
