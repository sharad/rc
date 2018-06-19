;;; occ-ctor.el --- occ-api               -*- lexical-binding: t; -*-
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
;; (require 'occ-object-methods)
(require 'occ-tree)

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
         (if (org-before-first-heading-p)
             "empty heading"
           (org-get-heading 'notags))))
    (let ((heading (when heading-with-string-prop
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
                       :heading-prop heading-prop
                       :marker  marker
                       :file file
                       :point point
                       :clock-sum clock-sum
                       :plist task-plist))

        (let ((inherited-props (cl-method-first-arg 'occ-readprop)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-get-property task prop)
                (occ-set-property task prop val))))))
      task)))

(cl-defmethod occ-make-task ((n number)
                             builder)
  (occ-debug :debug "point %s" n)
  (if (<= n (point-max))
      (save-restriction
        (save-excursion
          (goto-char n)
          (occ-make-task-at-point builder)))))

(cl-defmethod occ-make-task ((m marker)
                             builder)
  (occ-debug :debug "point %s" m)
  (if (and
       (marker-buffer m)
       (numberp (merker-position m)))
      (with-current-buffer (marker-buffer m)
        (if (<= (occ-make-task (merker-position m)) (point-max))
            (occ-make-task (merker-position m) builder)))))

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
                   :name (buffer-name buff)
                   :file file
                   :buffer buff)))
    context))

(cl-defmethod occ-make-contextual-task ((task occ-task)
                                        (context occ-context)
                                        (rank number))
  ;; use occ-build-contextual-task
  (make-occ-contextual-task
   :name    nil
   :task    task
   :context context
   :rank    rank))

(defvar occ-global-task-collection nil)

(cl-defmethod occ-make-task-collection ((file-spec (head :tree)))
  (unless occ-global-task-collection
    (let ((collection (make-occ-tree-task-collection
                       :name "task collection tree"
                       :root-files (cdr file-spec))))
      (setf occ-global-task-collection collection))))

(cl-defmethod occ-make-task-collection ((file-spec (head :list)))
  (unless occ-global-task-collection
    (let ((collection (make-occ-list-task-collection
                       :name "task collection list"
                       :root-files (cdr dir-spec))))
      (setf occ-global-task-collection collection))))

(cl-defmethod occ-collect-tasks (collection
                              force)
  (error "first argument should be of type (or occ-tree-task-collection occ-list-task-collection)"))

(cl-defmethod occ-collect-tasks ((collection occ-tree-task-collection)
                              force)
  (unless (occ-tree-task-collection-tree collection)
    (setf
     (occ-tree-task-collection-tree collection)
     (occ-task-tree-build
      #'(lambda ()
          (or
           (occ-make-task-at-point #'make-occ-tree-task)
           (make-occ-tree-task :name "empty tree task"))) ;; note: only using first file of root-files
      (car (occ-tree-task-collection-root-files collection))))))

(cl-defmethod occ-collect-tasks ((collection occ-list-task-collection)
                              force)
  (unless (occ-list-task-collection-list collection)
    (setf
     (occ-list-task-collection-list collection)
     (remove nil
             (org-map-entries
              #'(lambda ()
                  (or
                   (occ-make-task-at-point #'make-occ-list-task)
                   (make-occ-list-task :name "empty list task")))
              t
              (occ-list-task-collection-root-files collection))))))

(cl-defmethod occ-collection ((collection occ-tree-task-collection))
  (occ-tree-task-collection-tree occ-global-task-collection))

(cl-defmethod occ-collection ((collection occ-list-task-collection))
  (occ-tree-task-collection-list occ-global-task-collection))

(defun occ-collection-object ()
  occ-global-task-collection)




(when nil
  (progn
    (setq occ-global-task-collection nil)
    (occ-make-task-collection (list :tree org-context-clock-task-tree-task-root-org-file))
    (occ-tree-task-collection-tree occ-global-task-collection)
    (occ-collect-tasks occ-global-task-collection t)
    (occ-tree-task-collection-root-files occ-global-task-collection)
    (setf occ-gtree
          (occ-tree-task-collection-tree occ-global-task-collection)))

  (setf
   occ-test-gtree
   (occ-task-tree-build
    #'(lambda ()
        (or
         (occ-make-task-at-point #'make-occ-tree-task)
         (make-occ-tree-task :name "empty tree task"))) ;; note: only using first file of root-files
    "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/xx.org"))

  (setq occ-test-gtree
        (occ-task-tree-build
         #'(lambda ()
             (or
              (occ-make-task-at-point #'make-occ-tree-task)
              (make-occ-tree-task :name "empty tree task"))) ;; note: only using first file of root-files
         org-context-clock-task-tree-task-root-org-file))

  (with-current-buffer (find-file-noselect "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/xx.org")
    (goto-char (point-min))
    (setf occ-file-subtree
          (occ-task-tree-map-subheading
           #'(lambda ()
               (occ-task-tree-collect-task
                #'(lambda ()
                    (or
                     (occ-make-task-at-point #'make-occ-tree-task)
                     (make-occ-tree-task :name "empty tree task")))))))))

(provide 'occ-ctor)
;;; occ-ctor.el ends here
