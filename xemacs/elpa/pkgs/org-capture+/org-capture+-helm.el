;;; org-capture+-helm.el --- org capture+ helm       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'org-capture+-helm)


(require 'helm)


;; * Dynamic Match based templates

(defvar org-capture+-helm-templates-alist nil)


(defun org-capture+-helm-template-add (scope heading template)
  (unless (assoc heading org-capture+-helm-templates-alist)
    (pushnew (list heading) org-capture+-helm-templates-alist))
  (pushnew template
           (cdr (assoc heading org-capture+-helm-templates-alist))))

(org-capture+-helm-template-add 'test "TODO" "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "TODO" "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(defun max-depth (tree)
  (if (atom tree)
      0
    (1+ (reduce #'max (mapcar #'max-depth tree)))))

(defun collect-elem-cond (tree nodep predicate)
  (if (funcall nodep tree)
      tree
    (when (funcall predicate tree)
      (remove nil
              (mapcar #'(lambda (e) (collect-elem-cond e nodep predicate))
                      (cdr tree))))))

(defun collect-elem-cond-depth (tree nodep predicate depth)
  (collect-elem-cond tree
                     nodep
                     #'(lambda (subtree)
                         (or
                          (<= (max-depth subtree) depth)
                          (funcall predicate subtree)))))

(defun collect-with-depth (tree depth)
  (collect-elem-cond-depth tree
                           #'(lambda (x)
                               (not (listp x)))
                           #'(lambda (subtree)
                               (memq (car subtree) '(t)))))

(-flatten (collect-with-depth xmatch-kk 1))



;;;###autoload
(defun org-capture+-build-helm-template-source (name attrib-list &rest templates)
  `((name . ,name)
    (multiline)
    (candidates ,@templates)
    ,@attrib-list))

;;;###autoload
(defun org-capture+-build-helm-template-sources (attrib-list alist)
  (mapcar
   #'(lambda (e)
       (apply #'org-capture+-build-helm-template-source
              (car e)
              attrib-list
              (cdr e)))
   alist))

;;;###autoload
(defun org-capture+-helm-select-template (&optional attrib-list alist)
  (let ((attrib-list (or attrib-list '((action . identity))))
        (alist       (or alist org-capture+-helm-templates-alist)))
    (helm :sources
          (org-capture+-build-helm-template-sources attrib-list alist))))

;; (org-capture+-helm-select-template)







;; (t (pred1 (pred2 "y" "k") "x") (pred3 "z") "a")

;; (setq xmatch-kk '(t  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))

;; (defun collect-elem (list)
;;   (if (and
;;        (listp list)
;;        (consp (cadr list)))
;;       (mapcar #'(lambda (e) (collect-elem e))
;;               (cdr list))
;;     (if (listp list) (cdr list) list)))

;; (setq xmatch-kk '(t "i" "l"  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))

;; (defun collect-elem (list)
;;   (if (listp list)
;;       (mapcar #'(lambda (e) (collect-elem e))
;;               (cdr list))
;;     list))


;; (collect-elem xmatch-kk)

;; (collect-elem-cond xmatch-kk
;;                    #'(lambda (x) (not (listp x)))
;;                    #'(lambda (list)
;;                        (or
;;                         (<= (max-depth list) 0)
;;                         (memq (car list) '(t)))))

;; (max-depth '(t "i" "l"  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))
;; (max-depth '())




;;; org-capture+-helm.el ends here
