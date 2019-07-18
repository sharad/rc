;;; tree-lib.el --- tree library                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: Sharad
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

(provide 'tree-lib)


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

;;; tree-lib.el ends here
