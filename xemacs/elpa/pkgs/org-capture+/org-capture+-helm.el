;;; org-capture+-helm.el --- org capture+ helm       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad
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


(require 'dash)
(require 'helm)


(defvar org-capture+-helm-templates-alist nil)

(defun org-capture+-helm-template-add (scope heading template)
  (unless (assoc heading org-capture+-helm-templates-alist)
    (pushnew (list heading) org-capture+-helm-templates-alist))
  (pushnew template
           (cdr (assoc heading org-capture+-helm-templates-alist))))

(org-capture+-helm-template-add 'test "TODO" "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "TODO" "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


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


(defun max-depth (tree &optional nodep)
  (let ((nodep (or nodep #'atom)))
    (if (nodep tree)
        0
      (1+ (reduce #'max
                  (mapcar #'max-depth tree))))))

(defun collect-elem-cond (tree nodep predicate)
  (if (funcall nodep tree)
      tree
    (when (funcall predicate tree)
      (remove nil
              (mapcar #'(lambda (subtree)
                          (collect-elem-cond subtree nodep predicate))
                      (cdr tree))))))

(defun collect-elem-cond-depth (tree nodep predicate depth)
  (collect-elem-cond tree
                     nodep
                     #'(lambda (subtree)
                         (or
                          (<= (max-depth subtree nodep) depth)
                          (funcall predicate subtree)))))

(defun collect-elem-simple-depth (tree depth)
  (collect-elem-cond-depth tree
                           #'(lambda (x)
                               (not (listp x)))
                           #'(lambda (subtree)
                               (memq (car subtree) '(t)))
                           depth))

(defun tree-add (keys item list)
  (let ((key (car keys)))
    (if (cdr list)
        (if key
            (progn
              (unless (assoc key (cdr list))
                (nconc (cdr list) (list (list key))))
              (tree-add (cdr keys) item (assoc key (cdr list))))
          (unless (memq item (cdr list))
            (nconc (cdr list) (list item))))
      (progn
        (when key (nconc list (list (list key))))
        (if (cdr keys)
            (tree-add (cdr keys) item (assoc key (cdr list)))
          (if key
              (nconc (assoc key (cdr list)) (list item))
            (nconc list (list item))))))))

(-flatten (collect-with-depth xmatch-kk 1))


;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(defvar org-capture+-helm-templates-plist nil)
(defvar org-capture+-helm-templates-tree  nil)


(setq org-capture+-helm-templates-plist '(:todo))

(setq org-capture+-helm-templates-tree
      (list t))

;; https://stackoverflow.com/questions/4387570/in-common-lisp-how-can-i-insert-an-element-into-a-list-in-place
(setq org-capture+-helm-templates-tree '(t))

(defun tree-add (z list)
  (if (cdr list)
      (setf (cdr list) (list z))
    (nconc list (list z))))

(tree-add 'x org-capture+-helm-templates-tree)
(tree-add 'y org-capture+-helm-templates-tree)

(setf testy-list '(a bar))
(defun modify (list)
  (setf (first list) 'foo))
(modify testy-list)


(setq org-capture+-helm-templates-tree '(t))

(defun tree-add (keys item list)
  (let ((key (car keys)))
    (if (cdr list)
        (if key
            (progn
              (unless (assoc key (cdr list))
                (nconc (cdr list) (list (list key))))
              (tree-add (cdr keys) item (assoc key (cdr list))))
          (unless (memq item (cdr list))
            (nconc (cdr list) (list item))))
      (progn
        (when key (nconc list (list (list key))))
        (if (cdr keys)
            (tree-add (cdr keys) item (assoc key (cdr list)))
          (if key
              (nconc (assoc key (cdr list)) (list item))
            (nconc list (list item))))))))

(setq org-capture+-helm-templates-tree '(t))
(tree-add '(x z) 'y org-capture+-helm-templates-tree)
(tree-add '(x z) 'k org-capture+-helm-templates-tree)
(tree-add '(a z) 'k org-capture+-helm-templates-tree)
(tree-add '(x n) 'i org-capture+-helm-templates-tree)
(tree-add '(x n b) 'i org-capture+-helm-templates-tree)
(tree-add '(x #'(lambda () t) x) 'c org-capture+-helm-templates-tree)



(defvar h-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>")   'h-sort)
    map)
  "keymap for a helm source.")

(defvar h-sort-fn nil)

(defun h-sort ()
  (interactive))

(defun h-candidates ()
  '("aaaa" "bbb" "ccc"))

(defun h-candidate-transformer (candidates source)
  (reverse (h-candidates)))

(defun h-action-transformer (actions candidate)
  '(("Even" . identity)))

(setq h-source
      (helm-build-sync-source "number-selector"
        :keymap h-map
        ;; :requires-pattern nil
        :match (list #'(lambda (c) t))
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        ;; :filter-one-by-one #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)






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
