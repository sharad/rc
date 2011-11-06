;;; printing.el --- Printing

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

(deh-require-maybe 'ps-print
    (setq
     ps-print-color-p t                     ;print with colors
     ps-paper-type 'a4
     ;no header (try removing this line to see the header)
     ps-print-header nil)
    (defun ps-print-in-file (filename)
      "Print the buffer in a colored postscript file"
      (interactive "FPS file: ")
      (ps-print-buffer-with-faces filename)))


(user-provide 'printing)
;;; printing.el ends here
