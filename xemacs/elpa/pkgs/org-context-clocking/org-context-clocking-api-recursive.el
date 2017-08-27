;;; org-context-clocking-api-recursive.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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


;; "org entries access api for recursive task"

(defvar org-context-clocking-entry-tree-task-infos nil
  "Recursive org entries tree")

(defun org-entry-get-task-infos (files)
  "Build recursive org entries tree from files"
  (org-context-clocking-entry-tree-task-infos-tree files))

(defvar org-context-clocking-entry-tree-task-info-root-org-file nil
  "Root file to build recursive org entries tree")

;;;###autoload
(defun org-context-clocking-setup-entry-tree-task-info-root-org-file (file)
  (setq
   org-context-clocking-entry-tree-task-info-root-org-file file))

;;;###autoload
(defun org-context-clocking-entry-tree-update-task-infos (&optional force) ;[BUG not working for subtree]
  "Update recursive org entries tree"
  (interactive "P")
  (if org-context-clocking-entry-tree-task-info-root-org-file
      (if (file-exists-p org-context-clocking-entry-tree-task-info-root-org-file)
          (unless (and (not force)
                       org-context-clocking-entry-tree-task-infos)
            (setq org-context-clocking-entry-tree-task-infos
                  (org-context-clocking-entry-tree-get-task-infos
                   org-context-clocking-entry-tree-task-info-root-org-file)))
          (message "file %s not exists." org-context-clocking-entry-tree-task-info-root-org-file))
      (message "org-context-clocking-entry-tree-task-info-root-org-file is nil"))
  org-context-clocking-entry-tree-task-infos)

(defun org-context-clocking-entry-tree-map-subheading (fun)
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

(defun org-context-clocking-entry-tree-build (collector &optional file)
  "Build recursive org entries from org FILE (or current buffer) using COLLECTOR function e.g. org-context-clocking-entry-collect-task-info"
  (with-current-buffer (if file
                           (find-file-noselect file)
                           (current-buffer))
    (if file (goto-char (point-min)))
    (let* ((entry (funcall collector))
           (sub-tree
            (append
             (org-context-clocking-entry-tree-map-subheading 'org-context-clocking-entry-tree-collect-task-info)
             (let* ((file (if file file (buffer-file-name)))
                    (subtree-file
                     (org-context-clocking-entry-task-info-get-property entry :SUBTREEFILE))
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
                    (org-context-clocking-entry-tree-collect-task-info subtree-file)))))))
      (if sub-tree
          (org-context-clocking-entry-task-info-set-property entry :sub-tree sub-tree)
          entry))))

(defun org-context-clocking-entry-tree-collect-task-info (&optional file)
  "Build recursive org entries from org FILE (or current buffer)"
  (org-context-clocking-entry-tree-build 'org-context-clocking-entry-collect-task-info file))

(defun org-context-clocking-entry-tree-task-infos-tree (&optional file)
  "Build recursive org entries from org FILE (or current buffer)"
  (org-context-clocking-entry-tree-collect-task-info file))

(defun org-context-clocking-entry-tree-get-task-infos (&optional file)
  "Build recursive org entries from org FILE (or current buffer)"
  (let ()
    (org-context-clocking-entry-tree-collect-task-info file)))

(defun org-context-clocking-entry-tree-task-node-p (tx)
  "Test org TX is org entries tree non-leaf node"
  (org-context-clocking-entry-task-info-get-property tx :sub-tree))

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

  (defun org-context-clocking-tree-mapcar-task-infos (fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS"
    (tree-mapcar-nodes
     'org-context-clocking-entry-tree-task-node-p fn tree args))

  (defun org-context-clocking-tree-mapc-task-infos (fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS"
    (tree-mapc-nodes
     'org-context-clocking-entry-tree-task-node-p fn tree args))

  (defun org-context-clocking-tree-remove-if-not-task-infos (fn tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE"
    (tree-remove-if-not-nodes
     'org-context-clocking-entry-tree-task-node-p fn tree args))

  ;; (testing
  ;;  (setq
  ;;   testxx-remove
  ;;   (tree-remove-if-not-task-infos
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


(provide 'org-context-clocking-api-recursive)
;;; org-context-clocking-api-recursive.el ends here
