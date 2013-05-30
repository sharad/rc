;;  apt-elserv for emacs
;;  Copyright (C) 2003 Junichi Uekawa

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;The homepage can be found at : http://www.netfort.gr.jp/~dancer/software/apt-mode.html
;;The author can be contacted at : <dancer@netfort.gr.jp> or <dancer@debian.org> or <dancer@mikilab.doshisha.ac.jp>

;; elserv-module for apt-mode.

(defvar apt-elserv-process nil "The process of elserv for apt-elserv")
(defvar apt-elserv-port 8099 "The port number used with apt-elserv")

(defun apt-elserv-insert-header (title)
  "Insert header html into current buffer"
  (insert (format "
<html>
<head>
<title>APT/Elsev %s</title>
</head>
<body>
  <h1>APT/Elserv page %s</h1>
" title title)))

(defun apt-elserv-insert-footer ()
  "Insert footer html into current buffer"
  (insert "
</body>
</html>
"))

(defun apt-elserv-list-page (result)
  "dpkg -l page"
  (save-excursion
    (apt-dpkg-l)
    (let* ((htmlstr "") currentpkg)
      (beginning-of-buffer)
      (while (re-search-forward "^ii  \\([^ ]*\\)" nil t)
	(setq currentpkg (match-string 1))
	(setq htmlstr 
	      (concat
	       htmlstr
	       (format "<a href=\"/apt/pkg/%s\">%s</a> "
		       currentpkg currentpkg))))
      (with-temp-buffer 
	(apt-elserv-insert-header "list")
	(insert htmlstr)
	(apt-elserv-insert-footer)
	(elserv-set-result-header result
				  (list 'content-type "text/html;charset=utf-8"))
	(elserv-set-result-body result
				(encode-coding-string
				 (buffer-string) 'utf-8))))))

(defun apt-elserv-package-page (result packagename)
  "apt-cache show the package"
  (apt-cache-show packagename)
  (let* ((htmlstr "") currentpkg)
    (beginning-of-buffer)
    (while (re-search-forward "\\([^ ]+\\)" nil t)
      (setq currentpkg (match-string 1))
      (setq htmlstr
	    (concat 
	     htmlstr
	     (format "<a href=\"/apt/pkg/%s\">%s</a> "
		     currentpkg currentpkg))))
    (with-temp-buffer
      (apt-elserv-insert-header "list")
      (insert htmlstr)
      (apt-elserv-insert-footer)
      (elserv-set-result-header result
				(list 'content-type "text/html;charset=utf-8"))
      (elserv-set-result-body result
			      (encode-coding-string
			       (buffer-string) 'utf-8)))))


(defun apt-elserv-menu-page (result)
  "serve apt elserv main menu page"
  (elserv-set-result-header 
   result
   (list 'content-type "text/html"))
  (elserv-set-result-body 
   result
   "
<html>
<head>
<title>APT/Elserv</title>
</head>
<body>
  <h1>APT/Elserv page</h1>
  <p>
   Debian maintenance through APT. 
  </p>
</body>
</html>
"))

(defun apt-elserv-function (result path ppath request) 
  "APT elserv interface

RESULT is the resulting value
PATH is relative path from the published path
PPATH is the published path
REQUEST is the request data."
  (cond
   ((or (string= path "/") (string= path ""))			;default page
    (apt-elserv-menu-page result))
   ((string= path "/list")
    (apt-elserv-list-page result))
   ((string-match "^/pkg/" path)
    (apt-elserv-package-page result (substring path 5)))
   ))

(defun apt-elserv-start ()
  "Start apt-mode with elserv"
  (interactive)
  (setq apt-elserv-process (elserv-start apt-elserv-port))
  (elserv-publish apt-elserv-process
		  "/apt"
		  :allow '("localhost")
		  :function 'apt-elserv-function
		  :description "apt-mode"))


