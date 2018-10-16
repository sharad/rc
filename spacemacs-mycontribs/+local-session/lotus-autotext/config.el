;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
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



;; (provide 'config)
;;; config.el ends here


(defun start-latex () "Adds all that stuff to start a new LaTeX document" (interactive)
  (goto-char (point-min))
  (insert
   (concat
    "\\documentclass[a4paper,french]{article}\n"
    "\\title{}\n"
    "\\author{Jean-Baptiste Rouquier}\n"
    "\\date{}\n\n"
    "\\usepackage[french]{babel}    %\n"
    "\\usepackage{indentfirst}      % comment\n"
    "\\usepackage[latin1]{inputenc} % comment\n"
    "\\usepackage[T1]{fontenc}      % comment\n"
    "\\usepackage[pdftex]{graphicx}}\n"
    "\\begin{document}\n"
    "\\maketitle\n\n\n\n"))
  (goto-char (point-max))
  (insert "\n\\end{document}\n")
  (goto-char (point-min))
  (forward-line 2)
  (backward-char 2)
  (latex-mode))

(defun add-gpl () "Adds the GPL statements at the beginning of the file" (interactive)
  (let ((comment-style 'box)
        (gpl
         (concat
"This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation\; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY\; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
"
                 (if (boundp 'user-full-name)
                     (concat "\nWritten and (c) by " user-full-name "\n")
                     "")
;;                  (if (boundp 'user-mail-address) (concat
;;                                                   "Contact <"
;;                                                   user-mail-address
;;                                                   "> for comment & bug reports\n")
;;                    "")
                 )))

    (goto-char (point-min))
    (insert gpl)
    (comment-region (point-min) (+ (point-min) (length gpl)))))
