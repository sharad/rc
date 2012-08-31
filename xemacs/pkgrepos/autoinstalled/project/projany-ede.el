;;;_ projany/ede.el --- Projany support for EDE

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; Projany support for Emacs Development Environment (EDE), which is
;; part of CEDET.


;;;_ , Requires
(require 'projany)

;;;_. Body

(projany-setup
   ;;get-project-keys 
   #'(lambda (&rest r)
	(mapcar
	   #'(lambda (x)
		(cons (ede-name x) x))
	   ede-projects))
   
   ;;get-resource-keys - get project resources, generally meaning
   ;;single-file resources (will depend on `type')
   #'(lambda (proj type)
	(cond
	   ((eq type 'org-remember-file-targets)
	      (mapcar
		 #'(lambda (x)
		      (cons x x))
		 (delete-if-not
 		    #'(lambda (x)
 			 (string-match ".*\\.org" x))
		    ;;`ede-documentation-files' won't do, it doesn't
		    ;;accept a project param.
		    (ede-buffer-documentation-files 
		       proj
		       ;;Messy param but required.
		       (current-buffer)))))
	   
	   ((eq type 'source)
	      (error "`source' type is unsupported in projany/ede")
	      ;;The following would get a list of sourcecode objects,
	      ;;but EDE does not document a way of getting the even
	      ;;the filename patterns, so can't even list the
	      ;;directory to get lists of single-file resources.
	      '  
	      (apply #'append
		 (ede-map-targets proj
		    #'(lambda (ta)
			 (list
			    (ede-name ta)
			    (ede-target-sourcecode ta))))))
	   (t (error 
		 "Other resources are unsupported in projany/ede."))))
   
   :resource->filename
   #'(lambda (resource type)
	(cond
	   ((eq type 'org-remember-file-targets)
	      resource)
	   (t (error 
		 "(EDE) Other resources are unsupported."))))
   
   :default-project-key 
   #'(lambda (&rest r) 
	(cond
	   (ede-object (ede-name ede-object))
	   (ede-selected-object (ede-name ede-selected-object))))
   )



;;;_. Footers
;;;_ , Provides

(provide 'projany-ede)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; projany/ede.el ends here
