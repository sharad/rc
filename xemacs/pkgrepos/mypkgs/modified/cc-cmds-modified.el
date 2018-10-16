;;; cc-cmds-modified.el --- CC Cmds

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

;; as from (pabbrev-expand-maybe UARG)
;; (pabbrev-get-previous-binding) -> c-indent-line-or-region
;; was not able to pass t for region

(defun c-indent-line-or-region (&optional arg region)
  "Indent active region, current line, or block starting on this line.
In Transient Mark mode, when the region is active, reindent the region.
Otherwise, with a prefix argument, rigidly reindent the expression
starting on the current line.
Otherwise reindent just the current line."
  (interactive
   (list current-prefix-arg (use-region-p)))
  (if (or (use-region-p) region)
      (c-indent-region (region-beginning) (region-end))
    (c-indent-command arg)))


(provide 'cc-cmds-modified)
;;; cc-cmds-modified.el ends here
