;;; lotus-tree-lib.el --- Lotus Tree Library         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'lotus-tree-lib)


;; old
(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair (copy-tree alist))
      (if (assoc (car pair) ulist)
          (nconc (assoc (car pair) ulist) (cdr pair))
        (setf ulist (append ulist (list pair)))))
    ulist))

;; new
(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair alist)
      (if (assoc (car pair) ulist)
          (let ((xulist (assoc (car pair) ulist))
                (tlist nil)
                (ilist (cdr pair)))
            (dolist (i ilist)
              (unless (member i (cdr xulist))
                (push i tlist)))
            (nreverse tlist)
            (nconc (assoc (car pair) ulist) tlist))
        (let ((ef (car pair)))
          (when ef (push (list ef) ulist))
          (let ((xulist (assoc (car pair) ulist))
                (tlist nil)
                (ilist (cdr pair)))
            (dolist (i ilist)
              (unless (member i (cdr xulist))
                (push i tlist)))
            (nreverse tlist)
            (nconc (assoc (car pair) ulist) tlist)))))
    (nreverse ulist)))

(defun delete-dups-alist (alist)
  (dolist (pair alist)
    (setcdr pair (delete-dups (cdr pair))))
  alist)


(defun max-depth (tree &optional nodep)
  (let ((nodep (or nodep #'atom)))
    (if (funcall nodep tree)
        0
      (1+ (reduce #'max
                  (mapcar #'(lambda (subtree)
                              (if (consp subtree)
                                  (max-depth subtree nodep)
                                0))
                          tree))))))

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
        (when key
          (nconc list (list (list key))))
        (if (cdr keys)
            (tree-add (cdr keys) item (assoc key (cdr list)))
          (if key
              (nconc (assoc key (cdr list)) (list item))
            (nconc list (list item))))))))



(defun tree-add (keys item list)
  (let ((key (car keys)))
    (if (cdr list)
        (if key
            (progn
              (unless (assoc key (cdr list))
                (nconc (cdr list) (list (list key))))
              (tree-add (cdr keys) item (assoc key (cdr list))))
          (unless (member item (cdr list))
            (nconc (cdr list) (list item))))
      (progn
        (when key
          (nconc list (list (list key))))
        (if (cdr keys)
            (tree-add (cdr keys) item (assoc key (cdr list)))
          (if key
              (nconc (assoc key (cdr list)) (list item))
            (nconc list (list item))))))))


(defun tree-flatten (predicate L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (let ((predicate (or predicate #'atom)))
   (if (null L)
       nil
     (if (funcall predicate (first L))
         (cons (first L)
               (tree-flatten predicate (rest L)))
       (append (tree-flatten predicate (first L))
               (tree-flatten predicate (rest L)))))))


(when nil
 (setq org-capture+-helm-templates-tree '(t))
 (tree-add '(x z) 'y org-capture+-helm-templates-tree)
 (tree-add '(x z) 'k org-capture+-helm-templates-tree)
 (tree-add '(a z) 'k org-capture+-helm-templates-tree)
 (tree-add '(x n) 'i org-capture+-helm-templates-tree)
 (tree-add '(x n b) 'i org-capture+-helm-templates-tree)
 (tree-add '(x #'(lambda () t) x) 'c org-capture+-helm-templates-tree))

;;; lotus-tree-lib.el ends here
