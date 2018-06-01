;;; occ-func.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(require 'occ-common)
(require 'occ-base-objects)
(require 'occ-object-methods)

(defun occ-heading-content-only ()
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((start (progn
                         (goto-char (org-element-property :contents-begin (org-element-at-point)))
                         (while (org-at-drawer-p)
                           (goto-char (org-element-property :end (org-element-at-point))))
                         ;; (if (org-at-heading-p) (backward-char))
                         (point))))
            (unless (org-at-heading-p)
              (progn
                (outline-next-heading)
                ;; (outline-next-visible-heading 1)
                (backward-char)
                (buffer-substring start (point)))))))))

(defun occ-make-task-at-point (builder)
  ;; (org-element-at-point)
  (let (task
        (heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task-plist (cadr (org-element-at-point))))
      (when heading
        (setf task
              (funcall builder
                       :name    heading
                       :heading heading
                       :file file
                       :point point
                       :heading-prop heading-prop
                       :clock-sum clock-sum
                       :plist task-plist))

        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-get-property task prop)
                (occ-set-property task prop val))))))
      task)))

;; (defun org-task-collect-task-clock-info ()
(defun occ-make-task-from-clock (builder)
  ;; NOT used anywhere
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task-plist (cadr (org-element-at-point))))
      ;; (task-content-start )
      (when heading
        (setf task
              (funcall builder
                       :name    heading
                       :heading heading
                       :file file
                       :point point
                       :heading-prop heading-prop
                       :clock-sum clock-sum
                       :plist task-plist))
        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-get-property task prop)
                (occ-set-property task prop val)))))
        (if heading-with-string-prop
            (occ-set-property task 'task-clock-content (occ-heading-content-only))))
      task)))

(defun occ-make-context (&optional buff)
  (let* ((buff (if buff
                   (if (bufferp buff)
                       buff
                     (if (stringp buff)
                         (or
                          (get-buffer buff)
                          (if (file-exists-p buff)
                              (get-file-buffer buff)))))
                 (window-buffer)))
         (buf (org-base-buffer buf))
         (file (buffer-file-name buff))
         (context (make-occ-context
                   :name nil
                   :file file
                   :buffer buff)))
    context))

(defmethod occ-make-contextual-task ((task occ-task) (context occ-context))
  (make-occ-contextual-task
   :name nil
   :task task
   :context context))

(defvar occ-global-tree-task-collection nil)
(defun occ-make-tree-task-collection (root-file)
  (unless occ-global-tree-task-collection
    (let ((collection (make-occ-tree-task-collection
                       :name nil
                       :root-file root-file)))
      (setf occ-global-tree-task-collection collection)))

  (cl-struct-slot-value
   (cl-classname occ-global-tree-task-collection)
   'tree))

(defvar occ-global-list-task-collection nil)
(defun occ-make-list-task-collection (root-dir)
  (unless occ-global-list-task-collection
    (let ((collection (make-occ-list-task-collection
                       :name nil
                       :root-dir root-dir)))
      (setf occ-global-list-task-collection collection)))
  (cl-struct-slot-value (cl-classname collection) 'list))

(provide 'occ-func)
;;; occ-func.el ends here
