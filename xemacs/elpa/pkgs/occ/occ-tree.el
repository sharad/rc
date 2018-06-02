;;; occ-tree.el --- occ-api               -*- lexical-binding: t; -*-
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

(progn ;; "tree api"
  (defun tree-mapcar-nodes (subtreefn fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS, function SUBTREEFN require to find nonleaf node"
    (list
     (funcall fn tree args)
     :subtree
     (mapcar
      #'(lambda (e)
        (tree-mapcar-nodes subtreefn fn e args))
      (funcall subtreefn tree))))

  (defun tree-mapc-nodes (subtreefn fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS, function SUBTREEFN require to find nonleaf node"
    (funcall fn tree args)
    (mapc
     #'(lambda (e)
       (tree-mapc-nodes subtreefn fn e args))
     (funcall subtreefn tree)))

  (defun tree-remove-if-not-nodes (subtreefn predicate tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE, function SUBTREEFN require to find nonleaf node"
    (if (funcall subtreefn tree)
        (let ((rootele
               (if (funcall predicate tree args) tree))
              (subtree
               (remove
                nil
                (mapcar
                 #'(lambda (e)
                   (tree-remove-if-not-nodes subtreefn predicate e args))
                 (funcall subtreefn tree)))))
          (if (or rootele subtree)
              (plist-put tree :subtree subtree)))
        (if (funcall predicate tree args) tree)))

  ;; (testing
  ;;  (setq
  ;;   testxx-remove
  ;;   (tree-remove-if-not-tasks
  ;;    #'(lambda (e) (eq (plist-get e :pre-blank) 4))
  ;;    testxx))

  ;;  (setq testxxmapcar
  ;;        (tree-mapcar-nodes #'(lambda (tx) (plist-get tx :subtree))
  ;;                           #'(lambda (tx) (plist-get tx :title))
  ;;                           ;; testxx
  ;;                           (car (plist-get testxx :subtree))
  ;;                           ))

  ;;  (setq testxxmapc
  ;;        (tree-mapc-nodes #'(lambda (tx) (plist-get tx :subtree))
  ;;                         #'(lambda (tx) (plist-get tx :title))
  ;;                         ;; testxx
  ;;                         (car (plist-get testxx :subtree))
  ;;                         )))
  )



(progn
  (defun occ-task-tree-task-node-p (tx)
    "Test org TX is org tasks tree non-leaf node"
    (occ-get-property tx 'subtree))

  (defun occ-task-tree-task-subtree (tx)
    "Test org TX is org tasks tree non-leaf node"
    (occ-get-property tx 'subtree))

  (defun occ-tree-mapcar-tasks (fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS"
    (tree-mapcar-nodes
     'occ-task-tree-task-subtree fn tree args))

  (defun occ-tree-mapc-tasks (fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS"
    (tree-mapc-nodes
     'occ-task-tree-task-subtree fn tree args))

  (defun occ-tree-remove-if-not-tasks (fn tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE"
    (tree-remove-if-not-nodes
     'occ-task-tree-task-subtree fn tree args)))


(defun occ-task-tree-map-subheading (fun)
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

(defun occ-task-tree-collect-task (&optional file)
  "Build recursive org tasks from org FILE (or current buffer)"
  (occ-task-tree-build
   '(occ-collect-task)
   file))

(defun occ-task-tree-build (collector &optional file)
  "Build recursive org tasks from org FILE (or current buffer) using COLLECTOR function e.g. occ-task-collect-task"
  (with-current-buffer (if file
                           (find-file-noselect file)
                           (current-buffer))
    (if file (goto-char (point-min)))
    (let* ((entry (funcall collector))
           (sub-tree
            (append
             (occ-task-tree-map-subheading 'occ-task-tree-collect-task)
             (let* ((file (if file file (buffer-file-name)))
                    (subtree-file
                     (occ-get-property entry :SUBTREEFILE))
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
                    (occ-task-tree-collect-task subtree-file)))))))
      (if sub-tree
          (occ-set-property entry 'subtree sub-tree)
          entry))))

;;;###autoload
(defun occ-task-recursive-update-tasks (&optional force) ;; API (occ-api-set :predicate :update  'org-entry-list-update-tasks)
  "Update recursive org tasks tree"
  (interactive "P")
  (if occ-task-tree-task-root-org-file
      (if (file-exists-p occ-task-tree-task-root-org-file)
          (unless (and (not force)
                       occ-task-tree-tasks)
            (setq occ-task-tree-tasks
                  (occ-task-tree-get-tasks
                   occ-task-tree-task-root-org-file)))
        (message "file %s not exists." occ-task-tree-task-root-org-file))
    (message "occ-task-tree-task-root-org-file is nil"))
  occ-task-tree-tasks)

(defun occ-task-tree-tasks-files ()
  (let ((tasks (occ-task-recursive-update-tasks))
        (files '()))
    (occ-debug :debug "occ-entries-associated-to-context-by-keys: BEFORE files %s[%d]" files (length files))
    (occ-tree-mapc-tasks
     #'(lambda (task args)
         (push
          (occ-get-property task 'task-clock-file)
          files))
     tasks
     nil)
    (occ-debug :debug "occ-entries-associated-to-context-by-keys: AFTER files %s[%d]" "files" (length files))
    files))

(defun occ-task-tree-get-files ()
  "Build recursive org tasks from org FILE (or current buffer)"
  (let ()
    (remove nil
            (delete-dups
             (occ-task-tree-tasks-files)))))





;; API (occ-api-set :predicate :update  'org-task-list-update-tasks)
(defun occ-recursive-matching-tasks (context)
  (let ((tasks (occ-task-recursive-update-tasks))
        (matched '()))
      (occ-debug :debug "occ-entries-associated-to-context-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (occ-tree-mapc-tasks
       #'(lambda (task args)
           (let ((rank
                  (funcall occ-api-task-associated-to-context-p task args)))
             (unless rank (error "occ-entries-associated-to-context-by-keys[lambda]: rank is null"))
             (when (> rank 0)
               (push task matched)
               (occ-debug :debug "occ-entries-associated-to-context-by-keys[lambda]: task %s MATCHED RANK %d"
                        (occ-task-get-heading task)
                        (length matched)))))
       tasks
       context)

      (occ-debug :debug "occ-entries-associated-to-context-by-keys: AFTER matched %s[%d]" "matched" (length matched))

      matched))

(provide 'occ-tree)
;;; occ-tree.el ends here
