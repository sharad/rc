;;; occ-macros.el --- occ macros                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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

(provide 'occ-macros)




(defmacro occ-with-marker (marker &rest body)
  `(let ((marker ,marker))
     (progn
       ,@body)))

(defun occ-get-location ())





(defun org-create-new-task ()
  (interactive)
  (let ((collection
         '(("* TODO %? %^g\n %i\n [%a]\n" . 1)
           ("* TODO %? %^g\n %i\n Test [%a]\n" . 2))))
   (org-capture-plus
    'entry
    '(function org-goto-refile)
    (helm :sources
          `(((name . "Templates: ")
             (multiline)
             (candidates ,@collection)
             (action . identity))))
            ;; ((name . "Section 2")
            ;;  (multiline)
            ;;  (candidates ("G\nH\nI" . 3)
            ;;              ("J\nK\nL" . 4)))

    :empty-lines 1)))



(let ((collection
       '("* TODO %? %^g\n %i\n [%a]\n"
         "* TODO %? %^g\n %i\n Test [%a]\n")))
  (helm :sources
        `(((name . "Templates: ")
           (multiline)
           (candidates ,@collection)
           (action . identity)))))







;; https://code.orgmode.org/bzg/org-mode/commit/e2bdc488ee071ea9743b00424db28fce3505fe5d
;; Refiling: Allow to create new nodes.

;; When refiling, you can now create new parent nodes on the fly.  To do
;; this, set the variable `org-refile-allow-creating-parent-nodes' to
;; `confirm'.  Then, at a refiling prompt, proceed with completion until
;; you have an existing heading, and then add "/new heading", i.e. a
;; slash followed by the new heading.  That heading will be created as a
;; child of the existing heading, and the entry to be refiled will end up
;; under that new heading.

;;; occ-macros.el ends here


(org-refile-get-location)


(org-capture+
 'entry
 '(marker org-clock-marker)
 "* Hello %^{PROMPT}"
 ;; :immediate-finish t
 :empty-lines 1)



;; https://stackoverflow.com/questions/3811448/can-call-with-current-continuation-be-implemented-only-with-lambdas-and-closures
(lambda (f k)
  (f (lambda (v k0) (k v)) k))

;; https://stackoverflow.com/questions/612761/what-is-call-cc
(defvar x 0)

(+ 2 (call/cc (lambda (cc) (setq x cc) 3)))

(x 4)



