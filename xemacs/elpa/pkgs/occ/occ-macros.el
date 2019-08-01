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
(put 'occ-with-marker 'lisp-indent-function 1)

(defmacro occ-debug-return (label &rest body)
  `(let ((retval
          (progn ,@body)))
     (occ-message "%s: returns %s\n" ,label retval)))
(put 'occ-debug-return 'lisp-indent-function 1)

(defmacro occ-debug-return (label &rest body)
  `(progn ,@body))
(put 'occ-debug-return 'lisp-indent-function 1)


(defmacro occ-try-until (tries test &rest body)
  `(let* ((total-tries ,tries)
          (try         total-tries))
     (while (and (> try 0)
                 ,test)
       (setq try (1- try))
       ,@body)))
(put 'occ-try-until 'lisp-indent-function 2)


(defmacro occ-aggrigate-list-rank (value values aggregator &rest body)
  `(let ((values    (if (consp ,values) ,values (list ,values)))
         (total-rank 0))
     (dolist (,value values)
       (let ((rank (progn
                     ,@body)))
         (setq total-rank
               (funcall ,aggregator total-rank rank))))
     total-rank))
(put 'occ-aggrigate-list-rank 'lisp-indent-function 3)


(defmacro occ-generate-plist-functions (prefix item)
  (let* ((plist  (intern (concat (symbol-name prefix) "-" (symbol-name item) "-plist")))
         (clear  (intern (concat (symbol-name prefix) "-" (symbol-name item) "-clear")))
         (set    (intern (concat (symbol-name prefix) "-" (symbol-name item) "-set")))
         (get    (intern (concat (symbol-name prefix) "-" (symbol-name item) "-get")))
         (allget (intern (concat (symbol-name prefix) "-" (symbol-name item) "s-get"))))
    `(progn

       (defvar ,plist nil)

       (defun ,clear ()
         (setq ,plist nil))

       (defun ,set (key name ,item)
         (setq ,plist
               (plist-put
                ,plist
                key (cons name ,item))))

       (defun ,get (key)
         (plist-get ,plist key))

       (defun ,allget (&rest keys)
         (let ((items nil))
           (dolist (key keys)
             (let ((name-item (,get key)))
               (when name-item
                 (setf items (nconc items (list name-item))))))
           items)))))

(defun occ-get-location ())

(when nil
  (let ((collection
         '("* TODO %? %^g\n %i\n [%a]\n"
           "* TODO %? %^g\n %i\n Test [%a]\n")))
    (helm :sources
          `(((name . "Templates: ")
             (multiline)
             (candidates ,@collection)
             (action . identity))))))

(when nil
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

  (x 4))
