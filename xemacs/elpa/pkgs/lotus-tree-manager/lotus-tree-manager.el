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
(defvar org-capture+-helm-templates-tree  '(t))


;;;###autoload
(defun org-capture+-add-heading-template (keys heading &rest templates)
  (tree-add keys
            (list :template (cons heading templates))
            org-capture+-helm-templates-tree))

(defun org-capture+-template-p (template)
  (eql :template (car template)))

(defun org-capture+-tree-gen-predicate (predicate arg)
  #'(lambda (key)
      (funcall predicate key arg)))

(defun org-capture+-collect-template-alist (predicate arg level)
  (let* ((level (or level 0))
         (templates-tree
          (collect-elem-cond-depth org-capture+-helm-templates-tree
                                   #'org-capture+-template-p
                                   (org-capture+-tree-gen-predicate predicate arg)
                                   level)))
    (tree-flatten #'org-capture+-template-p
                  templates-tree)))

;; TODO: keyword replacement
(defun org-capture+-collect-templates-alist (fn arg level)
  (let* ((fn    (or fn #'org-capture+-tree-predicate))
         (alist (mapcar #'cadr
                        (org-capture+-collect-template-alist fn arg level))))
    (let ((templates-alist (collect-alist alist)))
      ;; (delete-dups-alist templates-alist)
      templates-alist)))

(defun org-capture+-collect-templates (fn arg level)
  (let* ((fn    (or fn   #'org-capture+-tree-predicate))
         (alist (org-capture+-collect-templates-alist fn arg level)))
    (let ((templates (apply #'append
                            (mapcar #'cdr alist))))
      templates)))

(defun org-capture+-collect-template-classes ()
  (mapcar #'car
          (org-capture+-collect-templates-alist #'(lambda (key-tree arg) t)
                                                '(t)
                                                0)))

(defun org-capture+-tree-predicate (key-tree arg)
  (memq (car key-tree) arg))

;;; lotus-tree-manager.el ends here
