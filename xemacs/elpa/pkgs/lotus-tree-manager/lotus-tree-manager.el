;;; lotus-tree-manager.el --- Lotus Tree Manager     -*- lexical-binding: t; -*-

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

(provide 'lotus-tree-manager)


;;;###autoload
(defvar tree-helm-items-tree '(t))

;;;###autoload
(defun stree-add-class-item (keys class &rest items)
  (tree-add keys
            (list :item (cons class items))
            tree-helm-items-tree))

(defun stree-item-p (item)
  (eql :item (car item)))

(defun stree-tree-gen-predicate (predicate arg)
  #'(lambda (key)
      (funcall predicate key arg)))

(defun stree-collect-item-alist (predicate arg level)
  (let* ((level (or level 0))
         (items-tree
          (collect-elem-cond-depth tree-helm-items-tree
                                   #'tree-item-p
                                   (tree-tree-gen-predicate predicate arg)
                                   level)))
    (tree-flatten #'tree-item-p
                  items-tree)))

(defun stree-collect-items-alist (fn arg level)
  (let* ((fn    (or fn #'tree-tree-predicate))
         (alist (mapcar #'cadr
                        (tree-collect-item-alist fn arg level))))
    (let ((items-alist
           (collect-carlist alist)))
      items-alist)))

(defun stree-collect-items (fn arg level)
  (let* ((fn    (or fn #'tree-tree-predicate))
         (alist (tree-collect-items-alist fn arg level)))
    (let ((items (apply #'append
                        (mapcar #'cdr alist))))
      items)))

(defun stree-collect-item-classes ()
  (mapcar #'car
          (tree-collect-items-alist #'(lambda (key-tree arg) t)
                                    '(t)
                                    0)))

(defun stree-tree-predicate (key-tree arg)
  (memq (car key-tree)
        arg))


(defun tree-item-p (item)
  (eql :item (car item)))

(defun tree-tree-gen-predicate (predicate arg)
  #'(lambda (key)
      (funcall predicate key arg)))

(defun tree-add-class-item (tree keys class &rest item)
  (unless (eq t (car tree))
    (push t tree))
  (tree-add keys
            (list :item (cons class item))
            tree))

(defun tree-collect-item-alist (tree predicate arg level)
  (let* ((level (or level 0))
         (items-tree
          (collect-elem-cond-depth tree
                                   #'tree-item-p
                                   (tree-tree-gen-predicate predicate arg)
                                   level)))
    (tree-flatten #'tree-item-p
                  items-tree)))

(defun tree-collect-items-alist (tree predicate arg level)
  (let* ((predicate (or predicate #'tree-tree-predicate))
         (alist     (mapcar #'cadr
                            (tree-collect-item-alist tree predicate arg level))))
    (let ((items-alist
           (collect-carlist alist)))
      items-alist)))

(defun tree-collect-items (tree predicate arg level)
  (let* ((predicate (or predicate #'tree-tree-predicate))
         (alist     (tree-collect-items-alist tree predicate arg level)))
    (let ((items (apply #'append
                        (mapcar #'cdr alist))))
      items)))

(defun tree-collect-item-classes (tree)
  (mapcar #'car
          (tree-collect-items-alist tree
                                    #'(lambda (key-tree arg) t)
                                    '(t)
                                    0)))

(defun tree-tree-predicate (key-tree arg)
  (memq (car key-tree)
        arg))


(defvar test-tree '(t))
(setq test-tree '(t))
(tree-add-class-item test-tree '(a b c) "todo" "test")
(tree-collect-items-alist test-tree nil '(t a b c) 0)
(tree-collect-items test-tree nil '(t a b c) 0)
(tree-collect-item-classes test-tree)

;;; lotus-tree-manager.el ends here
