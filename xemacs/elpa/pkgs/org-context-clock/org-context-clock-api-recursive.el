;;; org-context-clock-api-recursive.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad
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


;; "org tasks access api for recursive task"

(require 'org-context-clock-api)

(defvar org-context-clock-task-tree-task-root-org-file nil
  "Root file to build recursive org tasks tree")

(defvar org-context-clock-task-tree-tasks nil
  "Recursive org tasks tree")

(defvar org-context-clock-task-tree-files nil
  "Recursive org tasks tree file")

(defun org-task-get-tasks (file)
  "Build recursive org tasks tree from file"
  (org-context-clock-task-tree-tasks-tree file))

(defun org-task-get-files ()
  "Build recursive org tasks tree from file"
  (org-context-clock-task-tree-tasks-files))

;;;###autoload
(defun org-context-clock-setup-task-tree-task-root-org-file (file)
  (setq
   org-context-clock-task-tree-task-root-org-file file))

;;;###autoload
(defun org-context-clock-task-recursive-update-tasks (&optional force) ;; API (org-context-clock-api-set :predicate :update  'org-entry-list-update-tasks)
  "Update recursive org tasks tree"
  (interactive "P")
  (if org-context-clock-task-tree-task-root-org-file
      (if (file-exists-p org-context-clock-task-tree-task-root-org-file)
          (unless (and (not force)
                       org-context-clock-task-tree-tasks)
            (setq org-context-clock-task-tree-tasks
                  (org-context-clock-task-tree-get-tasks
                   org-context-clock-task-tree-task-root-org-file)))
          (message "file %s not exists." org-context-clock-task-tree-task-root-org-file))
      (message "org-context-clock-task-tree-task-root-org-file is nil"))
  org-context-clock-task-tree-tasks)
(org-context-clock-access-api-set :recursive :update  'org-context-clock-task-recursive-update-tasks)

;;;###autoload
(defun org-context-clock-task-recursive-update-files (&optional force)
  (unless (and (not force)
               org-context-clock-task-tree-files)
    (org-context-clock-task-recursive-update-tasks force)
    (setq
     org-context-clock-task-tree-files (org-context-clock-task-tree-get-files)))
  org-context-clock-task-tree-files)
(org-context-clock-access-api-set :recursive :files  'org-context-clock-task-recursive-update-files)

(defun org-context-clock-task-tree-map-subheading (fun)
  "Call FUN for every heading underneath the current heading"
  ;; (org-back-to-heading)
  (let ((level (funcall outline-level))
        (collection nil))
    (save-excursion
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level) level))
                  (not (eobp)))
        (if (= (funcall outline-level) (1+ level))
            (push (funcall fun) collection))))
    collection))

(defun org-context-clock-task-tree-build (collector &optional file)
  "Build recursive org tasks from org FILE (or current buffer) using COLLECTOR function e.g. org-context-clock-task-collect-task"
  (with-current-buffer (if file
                           (find-file-noselect file)
                           (current-buffer))
    (if file (goto-char (point-min)))
    (let* ((entry (funcall collector))
           (sub-tree
            (append
             (org-context-clock-task-tree-map-subheading 'org-context-clock-task-tree-collect-task)
             (let* ((file (if file file (buffer-file-name)))
                    (subtree-file
                     (org-context-clock-task-get-property entry :SUBTREEFILE))
                    (subtree-file
                     (if (and subtree-file
                              (file-relative-name subtree-file))
                         (expand-file-name subtree-file
                                           (if file
                                               (file-name-directory file)
                                               default-directory))
                         subtree-file)))
               (if (and
                    subtree-file
                    (file-readable-p subtree-file))
                   (list
                    (org-context-clock-task-tree-collect-task subtree-file)))))))
      (if sub-tree
          (org-context-clock-task-set-property entry :sub-tree sub-tree)
          entry))))

(defun org-context-clock-task-tree-collect-task (&optional file)
  "Build recursive org tasks from org FILE (or current buffer)"
  (org-context-clock-task-tree-build 'org-context-clock-collect-task file))

(defun org-context-clock-task-tree-tasks-tree (&optional file)
  "Build recursive org tasks from org FILE (or current buffer)"
  (org-context-clock-task-tree-collect-task file))

(defun org-context-clock-task-tree-get-tasks (&optional file)
  "Build recursive org tasks from org FILE (or current buffer)"
  (let ()
    (org-context-clock-task-tree-collect-task file)))

(defun org-context-clock-task-tree-tasks-files ()
  (let ((tasks (org-context-clock-task-recursive-update-tasks))
        (files '()))
      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: BEFORE files %s[%d]" files (length files))
      (org-context-clock-tree-mapc-tasks
       #'(lambda (task args)
           (push
            (org-context-clock-task-get-property task :task-clock-file)
            files))
       tasks
       nil)
      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: AFTER files %s[%d]" "files" (length files))
      files))

(defun org-context-clock-task-tree-get-files ()
  "Build recursive org tasks from org FILE (or current buffer)"
  (let ()
    (remove nil
            (delete-dups
             (org-context-clock-task-tree-tasks-files)))))


(defun org-context-clock-task-tree-task-node-p (tx)
  "Test org TX is org tasks tree non-leaf node"
  (org-context-clock-task-get-property tx :sub-tree))

(progn ;; "tree api"
  (defun tree-mapcar-nodes (nonleafnodep fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS, function NONLEAFNODEP require to find nonleaf node"
    (list
     (funcall fn tree args)
     :sub-tree
     (mapcar
      #'(lambda (e)
        (tree-mapcar-nodes nonleafnodep fn e args))
      (funcall nonleafnodep tree))))

  (defun tree-mapc-nodes (nonleafnodep fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS, function NONLEAFNODEP require to find nonleaf node"
    (funcall fn tree args)
    (mapc
     #'(lambda (e)
       (tree-mapc-nodes nonleafnodep fn e args))
     (funcall nonleafnodep tree)))

  (defun tree-remove-if-not-nodes (nonleafnodep predicate tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE, function NONLEAFNODEP require to find nonleaf node"
    (if (funcall nonleafnodep tree)
        (let ((rootele
               (if (funcall predicate tree args) tree))
              (subtree
               (remove
                nil
                (mapcar
                 #'(lambda (e)
                   (tree-remove-if-not-nodes nonleafnodep predicate e args))
                 (funcall nonleafnodep tree)))))
          (if (or rootele subtree)
              (plist-put tree :sub-tree subtree)))
        (if (funcall predicate tree args) tree)))

  (defun org-context-clock-tree-mapcar-tasks (fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS"
    (tree-mapcar-nodes
     'org-context-clock-task-tree-task-node-p fn tree args))

  (defun org-context-clock-tree-mapc-tasks (fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS"
    (tree-mapc-nodes
     'org-context-clock-task-tree-task-node-p fn tree args))

  (defun org-context-clock-tree-remove-if-not-tasks (fn tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE"
    (tree-remove-if-not-nodes
     'org-context-clock-task-tree-task-node-p fn tree args))

  ;; (testing
  ;;  (setq
  ;;   testxx-remove
  ;;   (tree-remove-if-not-tasks
  ;;    #'(lambda (e) (eq (plist-get e :pre-blank) 4))
  ;;    testxx))

  ;;  (setq testxxmapcar
  ;;        (tree-mapcar-nodes #'(lambda (tx) (plist-get tx :sub-tree))
  ;;                           #'(lambda (tx) (plist-get tx :title))
  ;;                           ;; testxx
  ;;                           (car (plist-get testxx :sub-tree))
  ;;                           ))

  ;;  (setq testxxmapc
  ;;        (tree-mapc-nodes #'(lambda (tx) (plist-get tx :sub-tree))
  ;;                         #'(lambda (tx) (plist-get tx :title))
  ;;                         ;; testxx
  ;;                         (car (plist-get testxx :sub-tree))
  ;;                         )))
  )

;; API (org-context-clock-api-set :predicate :update  'org-task-list-update-tasks)
(defun org-context-clock-recursive-matching-tasks (context)
  (let ((tasks (org-context-clock-task-recursive-update-tasks))
        (matched '()))
      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (org-context-clock-tree-mapc-tasks
       #'(lambda (task args)
           (let ((rank
                  (funcall org-context-clock-api-task-associated-to-context-p task args)))
             (unless rank (error "org-context-clock-entries-associated-to-context-by-keys[lambda]: rank is null"))
             (when (> rank 0)
               (push task matched)
               (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys[lambda]: task %s MATCHED RANK %d"
                        (org-context-clock-task-get-heading task)
                        (length matched)))))
       tasks
       context)

      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: AFTER matched %s[%d]" "matched" (length matched))

      matched))
(org-context-clock-access-api-set :recursive :tasks  'org-context-clock-recursive-matching-tasks)

(defun org-context-clock-recursive-build-dyntaskpl (task context)
  (list
   :rank (funcall org-context-clock-api-task-associated-to-context-p task context)
   :task task
   :marker (org-context-clock-task-get-property task :task-clock-marker)))
(org-context-clock-access-api-set :recursive :dyntaskpl  'org-context-clock-recursive-build-dyntaskpl)

(defun org-context-clock-recursive-matching-dyntaskpls (context)
  (let ((tasks (org-context-clock-task-recursive-update-tasks))
        (matched '()))
      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (org-context-clock-tree-mapc-tasks #'(lambda (task args)
                                             (let ((rank
                                                    (funcall org-context-clock-api-task-associated-to-context-p task args)))
                                               (unless rank (error "org-context-clock-entries-associated-to-context-by-keys[lambda]: rank is null"))
                                               (when (> rank 0)
                                                 (push
                                                  (org-context-clock-build-dyntaskpl task args)
                                                  matched)
                                                 (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys[lambda]: task %s MATCHED RANK %d"
                                                                          (org-context-clock-task-get-heading task)
                                                                          (length matched)))))
                                         tasks context)
      (org-context-clock-debug :debug "org-context-clock-entries-associated-to-context-by-keys: AFTER matched %s[%d]" "matched" (length matched))
      matched))
(org-context-clock-access-api-set :recursive :dyntaskpls  'org-context-clock-recursive-matching-dyntaskpls)

(defun org-context-clock-recursive-dyntaskpl-print (dyntaskpl heading)
  (let ((task (plist-get dyntaskpl :task)))
    (format "[%4d] %s"
            (plist-get dyntaskpl :rank)
            (org-context-clock-fontify-like-in-org-mode task))))
(org-context-clock-access-api-set :recursive :dyntaskplprint  'org-context-clock-recursive-dyntaskpl-print)

(provide 'org-context-clock-api-recursive)
;;; org-context-clock-api-recursive.el ends here
